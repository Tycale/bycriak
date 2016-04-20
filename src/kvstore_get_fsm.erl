%% @author rumapaul
%% @doc The coordinator for get operations.  The key here is to
%% generate the preflist and then query each
%% replica and wait until a quorum is met.


-module(kvstore_get_fsm).
-behavior(gen_fsm).
-include("kvstore.hrl").

%% ====================================================================
%% API
%% ====================================================================
-export([start_link/4, get/2]).

%% Callbacks
-export([init/1, code_change/4, handle_event/3, handle_info/3,
		 handle_sync_event/4, terminate/3]).

%% States
-export([prepare/2, execute/2, waiting/2]).

-record(state, {req_id,
				from,
				client,
				key_name,
				preflist,
				num_r=0,
				replies=[]}).

%% ====================================================================
%% API functions
%% ====================================================================

start_link(ReqID, From, Client, KeyName) ->
	gen_fsm:start_link(?MODULE, [ReqID, From, Client, KeyName], []).

get(Client, KeyName) ->
	ReqID = mk_reqid(),
	kvstore_get_fsm_sup:start_get_fsm([ReqID, self(), Client, KeyName]),
	{ok, ReqID}.

%% ====================================================================
%% States
%% ====================================================================

%% Initialize state data.
init([ReqId, From, Client, KeyName]) ->
	SD = #state{req_id=ReqId,
				from=From,
				client=Client,
				key_name = KeyName},
	{ok, prepare, SD, 0}.

prepare(timeout, SDInit=#state{client=Client, key_name=KeyName}) ->
	DocIdx = riak_core_util:chash_key({list_to_binary(Client), 
									   list_to_binary(KeyName)}),
	PrefList = riak_core_apl:get_apl(DocIdx, ?N, kvstore_dht),
	SD = SDInit#state{preflist=PrefList},
	{next_state, execute, SD, 0}.

% @doc Execute the get reqs
execute(timeout, SDInit=#state{req_id=ReqId,
							   key_name=KeyName,
							   preflist=PrefList}) ->
	kvstore_dht_vnode:get(PrefList, ReqId, KeyName),
	{next_state, waiting, SDInit}.

%% @doc Wait for R replies and then respond to From (original client
%% that called `kvstore:get/2').
waiting({ok, ReqId, Val}, SDInit=#state{from=From, 
										num_r=NumRInit, 
										replies=RepliesInit}) ->
	NumR=NumRInit+1,
	Replies=[Val|RepliesInit],
	SD=SDInit#state{num_r=NumR, replies=Replies},
	if
		NumR=:=?R ->
			Reply = 
				case lists:any(different(Val),Replies) of
					true ->
						Replies;
					false ->
						Val
				end,
	            From ! {ReqId, ok, Reply},
	            {stop, normal, SD};
			true -> {next_state, waiting, SD}
	end.

handle_info(_Info, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
	ok.
				
%% ====================================================================
%% Internal functions
%% ====================================================================

mk_reqid() ->
	erlang:phash2(erlang:now()).

different(A) -> fun(B) -> A =/= B end.
