-module(kvstore).
-include("kvstore.hrl").
-include_lib("../deps/riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
         get_partitions/0,
         start_race/2,
         get/1,
         put/2,
         incr/1,
         incrby/2
        ]).

-define(BUCKET, "default").
-define(TIMEOUT, 5000).

-define(ROUND_LENGTH, 10).
-define(DISTANCE, 100).
-define(ENERGY, 112).

-record(state_race, {id, 
                                   position, 
                                   speed, 
                                   energy}).

%% Public API

%% Start the race
start_race(N, Tot_bikers) ->
    io:format("Welcome to the race biker #~p, you'll compete with ~p other bikers! ~n", [N, Tot_bikers]),
    My_state = create_biker_state(N),
    States = create_biker_list(Tot_bikers),
    ?PRINT(States),
    ?PRINT(My_state),
    {ok, Choice} = ask_choice(),
    ?PRINT(Choice),
    {ok}.

create_biker_state(N) ->
    #state_race{id=N, position=0, speed=0, energy=?ENERGY}.

create_biker_list(N) ->
    create_biker_list(N, []).

create_biker_list(N, L) ->
    case N of 
        0 -> L;
        _ -> create_biker_list(N-1, [create_biker_state(N) | L])
    end.

ask_choice() ->
    io:fwrite("Choose your strategy for this round: ~n"),
    io:fwrite("1. Change your speed ~n"),
    io:fwrite("2. Go behind a player ~n"),
    io:fwrite("3. Use boost ~n"),
    Choice = io:fread('choice> ', "~d"),
    {ok, Choice}.



%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, kvstore),
    [{IndexNode, _Type}] = PrefList,
    ?PRINT({"indexnode", IndexNode}),
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, kvstore_vnode_master).

get_partitions() ->
	?PRINT(riak_core_nodeid:get()),
	{ok, CHBin} = riak_core_ring_manager:get_chash_bin(),
    chashbin:to_list(CHBin).

%% @doc Get a key's value.
get(KeyName) ->
    {ok, ReqID} = kvstore_get_fsm:get(?BUCKET, KeyName),	
    wait_for_reqid(ReqID, ?TIMEOUT).

%% @doc Put a key's value, replacing the current value.
put(KeyName, Val) ->
   do_write(KeyName, put, Val).

%% @doc Increment the key's value by 1.
incr(KeyName) ->
    do_write(KeyName, incr).

%% @doc Increment the key's value by Val.
incrby(KeyName, Val) ->
    do_write(KeyName, incrby, Val).


%%%===================================================================
%%% Internal Functions
%%%===================================================================

wait_for_reqid(ReqId, Timeout) ->
	receive
		{ReqId, ok} -> ok;
		{ReqId, ok, Val} -> {ok,Val}
	after Timeout ->
		{error, timeout}
	end.

do_write(KeyName, Op) ->
	{ok, ReqID} = kvstore_modify_fsm:modify(?BUCKET, KeyName, Op),
	wait_for_reqid(ReqID, ?TIMEOUT).

do_write(KeyName, Op, Val) ->
	{ok, ReqID} = kvstore_modify_fsm:modify(?BUCKET, KeyName, Op, Val),
	wait_for_reqid(ReqID, ?TIMEOUT).