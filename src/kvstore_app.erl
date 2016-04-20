-module(kvstore_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case kvstore_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, kvstore_vnode}]),
            ok = riak_core_ring_events:add_guarded_handler(kvstore_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(kvstore_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(kvstore, self()),
			
            ok = riak_core:register([{vnode_module, kvstore_dht_vnode}]),
            ok = riak_core_node_watcher:service_up(kvstore_dht, self()),
			
			
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
