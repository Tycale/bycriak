%% @author rumapaul
%% @doc  The coordinator for DHT modify opeartions. 
%% Replication of data in Riak Core by making use
%% of the _preflist_.


-module(kvstore_modify_fsm).
-behavior(gen_fsm).
-include("kvstore.hrl").

%% ====================================================================
%% API
%% ====================================================================
-export([start_link/5, start_link/6, modify/3, modify/4]).

%% Callbacks
-export([init/1, code_change/4, handle_event/3, handle_info/3,
		 handle_sync_event/4, terminate/3]).

%% States
-export([prepare/2, execute/2, waiting/2]).

%% req_id: The request id so the caller can verify the response.
%%
%% from: The pid of the sender so a reply can be made.
%%
%% client: The external entity that wrote this key.
%%
%% key_name: The name of the statistic.
%%
%% op: The stat op, one of [put, incr, incrby]
%%
%% prelist: The preflist for the given {Client, StatName} pair.
%%
%% num_w: The number of successful write replies.
-record(state, {req_id :: pos_integer(),
				from :: pid(),
				client :: string(),
				key_name :: string(),
				op :: atom(),
				val = undefined :: term() | undefined,
				preflist :: riak_core_apl:preflist2(),
				num_w=0 :: non_neg_integer()}).

%% ====================================================================
%% API functions
%% ====================================================================

start_link(ReqID, From, Client, KeyName, Op) ->
	start_link(ReqID, From, Client, KeyName, Op, undefined).

start_link(ReqID, From, Client, KeyName, Op, Val) ->
	gen_fsm:start_link(?MODULE, [ReqID, From, Client, KeyName, Op, Val], []).

modify(Client, KeyName, Op) ->
	modify(Client, KeyName, Op, undefined).

modify(Client, KeyName, Op, Val) ->
	ReqID = mk_reqid(),
	kvstore_modify_fsm_sup:start_modify_fsm([ReqID, self(), Client, KeyName, Op, Val]),
	{ok, ReqID}.

%% ====================================================================
%% States
%% ====================================================================

%% Initialize state data.
init([ReqId, From, Client, KeyName, Op, Val]) ->
	SD = #state{req_id=ReqId,
				from=From,
				client=Client,
				key_name = KeyName,
				op = Op,
				val = Val},
	{ok, prepare, SD, 0}.

prepare(timeout, SDInit=#state{client=Client, key_name=KeyName}) ->
	DocIdx = riak_core_util:chash_key({list_to_binary(Client), 
									   list_to_binary(KeyName)}),
	PrefList = riak_core_apl:get_apl(DocIdx, ?N, kvstore_dht),
	SD = SDInit#state{preflist=PrefList},
	{next_state, execute, SD, 0}.

% @doc Execute the modify reqs
execute(timeout, SDInit=#state{req_id=ReqId,
							   key_name=KeyName,
							   op = Op,
							   val = Val,
							   preflist=PrefList}) ->
	case Val of
		undefined -> kvstore_dht_vnode:Op(PrefList, ReqId, KeyName);
		_ -> kvstore_dht_vnode:Op(PrefList, ReqId, KeyName, Val)
	end,
	{next_state, waiting, SDInit}.

%% @doc Wait for W replies to respond.
waiting({ok, ReqId}, SDInit=#state{from=From, 
										num_w=NumWInit}) ->
	NumW=NumWInit+1,
	SD=SDInit#state{num_w=NumW},
	if
		NumW =:= ?W ->
	            From ! {ReqId, ok},
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

