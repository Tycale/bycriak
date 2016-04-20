-module(kvstore_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = { kvstore_vnode_master,
                  {riak_core_vnode_master, start_link, [kvstore_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

    Dht     = { kvstore_dht_vnode_master,
                  {riak_core_vnode_master, start_link, [kvstore_dht_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},
	
	ModifyFSMs = {kvstore_modify_fsm_sup,
                 {kvstore_modify_fsm_sup, start_link, []},
                 permanent, infinity, supervisor, [kvstore_modify_fsm_sup]},

    GetFSMs = {kvstore_get_fsm_sup,
               {kvstore_get_fsm_sup, start_link, []},
               permanent, infinity, supervisor, [kvstore_get_fsm_sup]},

    { ok,
        { {one_for_one, 5, 10},
          [VMaster, Dht, ModifyFSMs, GetFSMs]}}.
