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

-record(state_biker, {id, 
                                   position, 
                                   speed, 
                                   energy,
                                   behind,
                                   boost}).

%% Public API

%% Start the race
start_race(N, Tot_bikers) ->
    io:format("Welcome to the race biker #~p, you'll compete with ~p other bikers! ~n", [N, Tot_bikers]),
    States = create_biker_list(Tot_bikers),
    My_choices = [{speed, 0}],
    do_race(N, Tot_bikers, States, My_choices, 0),
    {ok} .

do_race(N, Tot_bikers, States, My_choices, Round) ->
    
    display_positions(States),
    Choice = guarded_choice(lists:nth(length(My_choices), My_choices), States, N),
    ?PRINT(Choice),
    Decisions = [{speed, 10}, {speed, 9}, {speed, 8}],
    S1 = update_states(States, Decisions, Tot_bikers),
    S2 = update_states(S1, Decisions, Tot_bikers),
    ?PRINT(S1),
    ?PRINT(S2),

    case have_win(States) of
        false -> do_race(N, Tot_bikers, States, My_choices ++ [Choice], Round+1);
        X -> io:format("We got a winner ! ~p have completed the race !", [X])
    end
    .

create_biker_state(N) ->
    #state_biker{id=N, position=0, speed=0, energy=?ENERGY}.

create_biker_list(N) ->
    create_biker_list(N, []).

create_biker_list(N, L) ->
    case N of 
        0 -> L;
        _ -> create_biker_list(N-1, [create_biker_state(N) | L])
    end.

wait_input(Timeout) ->
    Parent = self(),
    Get_cmd = fun() -> 
        Parent ! io:get_line("choice > ") 
    end,
    Pid = spawn(Get_cmd),
    receive
        Data -> Data
    after Timeout ->
        exit(Pid, kill),
        timeout
    end.

ask_choice(Last_choice) ->
    io:fwrite("Choose your strategy for this round: ~n"),
    io:fwrite("* Change your speed, type : 1 NEW_SPEED~n"),
    io:fwrite("* Go behind a player, type : 2 ID_PLAYER ~n"),
    io:fwrite("* Use boost, type : 3 ~n"),
    case wait_input(10000) of
        timeout -> io:format("~nA time out occured, your last choice will be used ~n", []), Last_choice;
        Cmd -> 
            T = lists:map(fun(X) -> {Int, _} = string:to_integer(X), Int end, string:tokens(Cmd, " ")),
            case T of
                [1, X] -> {speed, X};
                [2, X] -> {behind, X};
                [3] -> {boost};
                _ -> io:format("~nInput error, your last choice will be used ~n", []), Last_choice
            end
    end.

guarded_choice(Last_choice, States, N) ->
    case ask_choice(Last_choice) of 
        {speed, X} -> 
            E = get_energy(States, N),
            if X > E -> 
                io:format("You don't have any energy to perform this action, setting your speed to 0."), 
                {speed, 0}; 
                true -> {speed, X}
            end;
        Choice -> Choice
    end.

update_states(Old_states, Decisions, Tot_bikers) ->
    update_states(Old_states, Decisions, Tot_bikers, []).

update_states(Old_states, Decisions, N, L) ->
    case N of
        0 -> L;
        _ ->  NS = update_state(lists:nth(N,Old_states), Old_states, N, lists:nth(N, Decisions)),
                update_states(Old_states, Decisions, N-1, [ NS | L ])
    end.

update_state(State, Old_states, Id, Decision) ->
    case Decision of 
        {speed, X} -> State#state_biker{speed=X, position=calc_position(Old_states, Id), energy=calc_energy(Old_states, Id)};
        _ -> notImplementedYet
    end.

calc_position(States, Id) ->
    State = get_state(States, Id),
    case State of
        #state_biker{behind=undefined} -> get_position(States, Id) + get_speed(States, Id);
        #state_biker{behind=X} -> get_position(States, X) % TODO : not good
    end.

calc_energy(States, Id) ->
    State = get_state(States, Id),
    case State of
        #state_biker{boost=true} -> 0;
        #state_biker{energy=E, behind=undefined} -> E - 0.12 * get_speed(States, Id) * get_speed(States, Id);
        #state_biker{energy=E, behind=B} -> E - 0.06 * get_speed(States, B) * get_speed(States, B)
    end.

get_state(States, Id) ->
    lists:nth(Id, States).

get_position(States, Id) ->
    P = get_state(States, Id),
    P#state_biker.position.

get_speed(States, Id) ->
    P = get_state(States, Id),
    P#state_biker.speed.

get_energy(States, Id) ->
    P = get_state(States, Id),
    P#state_biker.energy.

display_positions(States) -> % TODO : Display position in tuple
    Comp_fct = fun(X, Y) -> X#state_biker.position > Y#state_biker.position end,
    Format_fct = fun(A, AccIn) -> io:format("#~p (position: ~p, speed: ~p, energy: ~p) ~n", 
        [A#state_biker.id, A#state_biker.position, A#state_biker.speed, A#state_biker.energy]), AccIn end,
    io:fwrite(lists:foldr(Format_fct, "", lists:sort(Comp_fct, States))).

have_win(States) ->
    Comp_fct = fun(X, Y) -> X#state_biker.position > Y#state_biker.position end,
    Sorted_states = lists:sort(Comp_fct, States),
    [H|_] = Sorted_states,
    if H#state_biker.position >= ?DISTANCE -> H#state_biker.id;
        true -> false
    end
    .


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