%% @author rumapaul
%% @doc @todo Add description to kvstore_modify_fsm_sup.


-module(kvstore_modify_fsm_sup).
-behavior(supervisor).

%% ====================================================================
%% API
%% ====================================================================
-export([start_modify_fsm/1, start_link/0]).

%% Callbacks
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
start_modify_fsm(Args) ->
	supervisor:start_child(?MODULE, Args).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	ModifyFsm = {undefined,
			  {kvstore_modify_fsm, start_link, []},
			  temporary, 5000, worker, [kvstore_modify_fsm]},
	{ok, {{simple_one_for_one, 10, 10}, [ModifyFsm]}}.


%% ====================================================================
%% Internal functions
%% ====================================================================


