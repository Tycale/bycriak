%% @author rumapaul
%% @doc A vnode to handle get & put commands for DHT.  The vnode
%% requests will be hashed on Client and Key and will use a
%% coordinator to enforce N/R/W values.

-module(kvstore_dht_vnode).
-behavior(riak_core_vnode).
-include("kvstore.hrl").
-include_lib("../deps/riak_core/include/riak_core_vnode.hrl").

%% ====================================================================
%% API
%% ====================================================================
-export([
         get/3,
         put/4,
         incr/3,
         incrby/4
        ]).

%% Callbacks
-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-record(state, {partition, keys}).

-define(MASTER, kvstore_dht_vnode_master).

%%%===================================================================
%%% API Functions
%%%===================================================================

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

get(PrefList, ReqId, KeyName) ->
	riak_core_vnode_master:command(PrefList,
								   {get, ReqId, KeyName},
								   {fsm, undefined, self()},
									?MASTER).

put(PrefList, ReqId, KeyName, Val) ->
	riak_core_vnode_master:command(PrefList,
								   {put, ReqId, KeyName, Val},
								   {fsm, undefined, self()},
									?MASTER).

incr(PrefList, ReqId, KeyName) ->
	riak_core_vnode_master:command(PrefList,
								   {incr, ReqId, KeyName},
								   {fsm, undefined, self()},
									?MASTER).
incrby(PrefList, ReqId, KeyName, Val) ->
	riak_core_vnode_master:command(PrefList,
								   {incrby, ReqId, KeyName, Val},
								   {fsm, undefined, self()},
									?MASTER).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([Partition]) ->
    {ok, #state { partition=Partition, keys=dict:new() }}.

handle_command({get, ReqId, KeyName}, _Sender, #state{partition=_, keys=Keys}=State) ->
    Reply =
        case dict:find(KeyName, Keys) of
            error ->
                not_found;
            {ok, Found} ->
                Found
        end,
	%?PRINT({dht_reply, Partition, KeyName, Reply}),
    {reply, {ok, ReqId, Reply}, State};

handle_command({put, ReqId, KeyName, Val}, _Sender, #state{keys=KeysInit}=State) ->
    Keys = dict:store(KeyName, Val, KeysInit),
    {reply, {ok, ReqId}, State#state{keys=Keys}};

handle_command({incr, ReqId, KeyName}, _Sender, #state{keys=KeysInit}=State) ->
    Keys = dict:update_counter(KeyName, 1, KeysInit),
    {reply, {ok,ReqId}, State#state{keys=Keys}};

handle_command({incrby, ReqId, KeyName, Val}, _Sender, #state{keys=KeysInit}=State) ->
    Keys = dict:update_counter(KeyName, Val, KeysInit),
    {reply, {ok,ReqId}, State#state{keys=Keys}}.

handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
    Acc = dict:fold(Fun, Acc0, State#state.keys),
    {reply, Acc, State}.

handoff_starting(_TargetNode, _State) ->
    {true, _State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(Data, #state{keys=KeysInit}=State) ->
    {KeyName, Val} = binary_to_term(Data),
    Keys = dict:store(KeyName, Val, KeysInit),
    {reply, ok, State#state{keys=Keys}}.

encode_handoff_item(KeyName, Val) ->
    term_to_binary({KeyName,Val}).

is_empty(State) ->
   case dict:size(State#state.keys) of
          0 -> {true, State};
          _ -> {false, State}
  end.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.



