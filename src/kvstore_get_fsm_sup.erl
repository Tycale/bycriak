%% @author rumapaul
%% @doc @todo Add description to kvstore_get_fsm_sup.


-module(kvstore_get_fsm_sup).
-behavior(supervisor).

%% ====================================================================
%% API
%% ====================================================================
-export([start_get_fsm/1, start_link/0]).

%% Callbacks
-export([init/1]).


%% ====================================================================
%% API functions
%% ====================================================================
start_get_fsm(Args) ->
	supervisor:start_child(?MODULE, Args).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	GetFsm = {undefined,
			  {kvstore_get_fsm, start_link, []},
			  temporary, 5000, worker, [kvstore_get_fsm]},
	{ok, {{simple_one_for_one, 10, 10}, [GetFsm]}}.


