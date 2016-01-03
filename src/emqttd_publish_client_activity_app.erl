-module(emqttd_publish_client_activity_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqttd_publish_client_activity_sup:start_link(),
    Env = application:get_all_env(),
    emqttd_publish_client_activity:onload(Env),
    {ok, Sup}.

stop(_State) ->
    emqttd_publish_client_activity:onunload().
