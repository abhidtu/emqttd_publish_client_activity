%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015 eMQTT.IO, All Rights Reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc
%%% emqttd_publish_message_states.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(emqttd_publish_message_states).

-include("../../../include/emqttd.hrl").

-export([onload/1, onunload/0]).

%% Hooks functions
-export([on_client_connected/3, on_client_disconnected/3]).

-export([on_client_subscribe/3, on_client_subscribe_after/3, on_client_unsubscribe/3]).

-export([on_message_publish/2, on_message_acked/3]).

-record(struct, {lst=[]}).

%% Called when the plugin application start
onload(Env) ->

    emqttd_broker:hook('client.connected', {?MODULE, on_client_connected},
                       {?MODULE, on_client_connected, [Env]}),

    emqttd_broker:hook('client.disconnected', {?MODULE, on_client_disconnected},
                       {?MODULE, on_client_disconnected, [Env]}),

    emqttd_broker:hook('client.subscribe', {?MODULE, on_client_subscribe},
                       {?MODULE, on_client_subscribe, [Env]}),

    emqttd_broker:hook('client.subscribe.after', {?MODULE, on_client_subscribe_after},
                       {?MODULE, on_client_subscribe_after, [Env]}),

    emqttd_broker:hook('client.unsubscribe', {?MODULE, on_client_unsubscribe},
                       {?MODULE, on_client_unsubscribe, [Env]}),

    emqttd_broker:hook('message.publish', {?MODULE, on_message_publish},
                       {?MODULE, on_message_publish, [Env]}),

    emqttd_broker:hook('message.acked', {?MODULE, on_message_acked},
                       {?MODULE, on_message_acked, [Env]}).



on_client_connected(ConnAck, _Client = #mqtt_client{client_id = ClientId}, _Env) ->
    io:format("client ~s connected, connack: ~w~n", [ClientId, ConnAck]).

on_client_disconnected(Reason, ClientId, _Env) ->
    io:format("client ~s disconnected, reason: ~w~n", [ClientId, Reason]).

%% should retain TopicTable
on_client_subscribe(ClientId, TopicTable, _Env) ->
    io:format("client ~s will subscribe ~p~n", [ClientId, TopicTable]),
    TopicTable.
   
on_client_subscribe_after(ClientId, TopicTable, _Env) ->
    io:format("client ~s subscribed ~p~n", [ClientId, TopicTable]).
    
on_client_unsubscribe(ClientId, Topics, _Env) ->
    io:format("client ~s unsubscribe ~p~n", [ClientId, Topics]),
    Topics.



%%-----------message publish start----------------------------------------%%

%% transform message and return
on_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    Message;

on_message_publish(Message, _Env) ->
    io:format("published ~s~n", [emqttd_message:format(Message)]),
    publishMsgArrival(Message),
    Message.

%%get the msg source and topic details and publish it on a channel with QoS = 1
publishMsgArrival(#mqtt_message{msgid = MsgId, pktid = PktId, from = From,
  qos = QoS, retain = Retain, dup = Dup, topic =Topic, payload = Payload, timestamp = Timestamp}) ->
  io:format("entered publishMsgArrival ~n"),
  publishMessageStates(From,"SYSTEM/message",Payload).

%%-----------message publish end----------------------------------------%%




%%-----------acknowledgement publish start----------------------------------------%%

on_message_acked(ClientId, Message, _Env) ->
    io:format("client ~s acked ~s ~n", [ClientId, emqttd_message:format(Message)]),
    publishMsgAck(ClientId,Message).

%%get the msg source and topic details and publish it on a channel with QoS = 1
publishMsgAck(ClientId, #mqtt_message{msgid = MsgId, pktid = PktId, from = From,
  qos = QoS, retain = Retain, dup = Dup, topic =Topic, payload = Payload, timestamp = Timestamp}) ->

  try
    %%decode the json and get the details embedded by msg producer
    Struct = mochijson2:decode(Payload),
    ProductId = proplists:get_value(<<"product_id">>, Struct#struct.lst),
    MessageId = proplists:get_value(<<"message_id">>, Struct#struct.lst),

    %% build json to send using product_id and message_id taken from message ack
    Json = mochijson2:encode([
      {pub_client_id, From},
      {ack_client_id, ClientId},
      {product_id, ProductId},
      {message_id, MessageId},
      {topic,Topic}
    ]),

    if ProductId =/= undefined, MessageId =/= undefined ->

      publishMessageStates(From,"SYSTEM/msgack",list_to_binary(Json));

      true ->
      io:format(" product id or message id not defined ~n")

    end

  catch
    _:_ -> io:fwrite("not json ~n")
  end.

%%-----------acknowledgement publish end----------------------------------------%%



publishMessageStates(From,Topic,Payload)->
  if From =/= <<"broker">>
    ->
    Msg = emqttd_message:make(<<"broker">>, 1, list_to_binary(Topic), Payload),
    emqttd_pubsub:publish(Msg);
    true ->
      io:format("publishMsgArrival from broker ~n")
  end.


%% Called when the plugin application stop
onunload() ->
    emqttd_broker:unhook('client.connected', {?MODULE, on_client_connected}),

    emqttd_broker:unhook('client.disconnected', {?MODULE, on_client_disconnected}),

    emqttd_broker:unhook('client.subscribe', {?MODULE, on_client_subscribe}),

    emqttd_broker:unhook('client.subscribe.after', {?MODULE, on_client_subscribe_after}),

    emqttd_broker:unhook('client.unsubscribe', {?MODULE, on_client_unsubscribe}),

    emqttd_broker:unhook('message.publish', {?MODULE, on_message_publish}),

    emqttd_broker:unhook('message.acked', {?MODULE, on_message_acked}).

