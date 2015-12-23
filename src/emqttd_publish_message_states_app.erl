-module(emqttd_publish_message_states_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqttd_publish_message_states_sup:start_link(),
    Env = application:get_all_env(),
    emqttd_publish_message_states:onload(Env),
    {ok, Sup}.

stop(_State) ->
    emqttd_publish_message_states:onunload().
