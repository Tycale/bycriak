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
         incrby/2,
         display_positions/1
        ]).

-define(BUCKET, "default").
-define(TIMEOUT, 5000).

-define(ROUND_LENGTH, 60).
-define(DISTANCE, 100).
-define(ENERGY, 112).

-record(state_biker, {id, 
                                   position, 
                                   speed, 
                                   energy,
                                   behind,
                                   boost,
                                   prev_decision}).

%% Public API

%% Start the race
start_race(N, Tot_bikers) ->
    io:format("Welcome to the race biker #~p, you'll compete with ~p other bikers! ~n", [N, Tot_bikers]),
    States = create_biker_list(Tot_bikers),
    My_choices = [{speed, 0}],
    do_race(N, Tot_bikers, States, My_choices, 0),
    {ok} .

do_race(N, Tot_bikers, States, My_choices, Round) ->
    io:format("======================~nRound ~p is going on !~n", [Round]),
    display_positions(States),
    Choice = guarded_choice(lists:nth(length(My_choices), My_choices), States, N),
    write_choice(N, Choice, Round),
    Decisions = read_decisions(Tot_bikers, Round),
    New_State = update_states(States, Decisions, Tot_bikers),
    
    case have_win(New_State) of
        false -> do_race(N, Tot_bikers, New_State, My_choices ++ [Choice], Round+1);
        nowinner -> io:format("The game is over, no biker managed to finish the race !");
        X -> io:format("We got a winner ! ~p have completed the race !~n", [X])
    end.

write_choice(N, Choice, Round) ->
    Biker_kv = "biker_decision" ++ integer_to_list(N),
    %io:format("~s = ~p ~n", [Biker_kv, Choice]),
    kvstore:put(Biker_kv, {Round, Choice}).

read_decisions(Tot_bikers, Round) ->
    io:format("Waiting after decisions of the other bikers..~n"),
        read_decisions(Tot_bikers, Round, []).
    
read_decisions(Tot_bikers, Round, Res) ->
    timer:sleep(100),
    if 
        Tot_bikers == 0 -> 
            Res;
        true -> 
            Biker_kv = "biker_decision" ++ integer_to_list(Tot_bikers),
            Kv = kvstore:get(Biker_kv),
            case Kv of
                {ok, {Round, X}} -> read_decisions(Tot_bikers-1, Round, [X | Res]);
                _ -> read_decisions(Tot_bikers, Round, Res)
            end
    end.

create_biker_state(N) ->
    #state_biker{id=N, position=0, speed=0, energy=?ENERGY, prev_decision={speed, 0}}.

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
        Parent ! io:get_line(" ") 
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
    io:fwrite("* Use boost, type : 3 ~nchoice >"),
    case wait_input(?ROUND_LENGTH * 1000) of
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
    Can_play = get_energy(States, N) > 0,
    if Can_play == false -> 
        io:format("======================~nNo energy remaining ! ~n"),
        {speed, 0}; 
        true ->
            io:format("======================~nEnergy remaining : ~p ~n", [get_energy(States, N)]),
            Ask_choice = ask_choice(Last_choice),
            guarded_choice_loop(Ask_choice, States, N)
    end.

guarded_choice_loop(Last_choice, States, N) ->
    case Last_choice of 
        {speed, X} -> 
            E = get_energy(States, N) - 0.12 * get_speed(States, N) * get_speed(States, N),
            if E  < 0 ->
                io:format("You don't have any energy to perform this action, setting your speed to 0.~n"), 
                {speed, 0}; 
                true -> {speed, X}
            end;
        {behind, Y} ->
            G = get_position(States, Y) > get_position(States, N) + get_speed(States, N),
            if G ->
                io:format("You cannot be behind this biker as you're too far away from him, performing your last choice instead.~n"),
                get_last_decision(States, N);
                true -> 
                    G2 = (Y > 0) and (Y =< length(States)) and (Y /= N),
                    if G2 -> {behind, Y};
                        true -> io:format("You cannot be behind this biker.. Trying last action instead~n"),
                            guarded_choice_loop(get_last_decision(States, N), States, N)
                    end
            end;
        {boost} -> {boost};
        Choice -> Choice
    end.

update_states(Old_states, Decisions, Tot_bikers) ->
    Order_comp = compute_order_exec(Decisions),
    case Order_comp of
        {ok, Order} ->
            Ordered_states = lists:map(fun(X) -> lists:nth(X, Old_states) end, Order),
            Ordered_decisions = lists:map(fun(X) -> lists:nth(X, Decisions) end, Order),
            New_States = update_states(Ordered_states, Ordered_decisions, Tot_bikers, []),
            Comp_fct = fun(X, Y) -> X#state_biker.id < Y#state_biker.id end,
            lists:sort(Comp_fct, New_States);
        {cycle, _} -> io:format("Cycle detected. Replaying this round.~n"), Old_states
    end.

update_states(Old_states, Decisions, N, L) ->
    case N of
        0 -> L;
        _ -> NS = update_state(lists:nth(N,Old_states), Old_states, N, lists:nth(N, Decisions), L),
                update_states(Old_states, Decisions, N-1, [ NS | L ])
    end.

compute_order_exec(Decisions) ->
    G = digraph:new(),
    Nb = length(Decisions),
    List_vertex = lists:map(fun(X) -> digraph:add_vertex(G, X, integer_to_list(X)) end, lists:seq(1, Nb)),
    F = fun(X) ->
        case lists:nth(X, Decisions) of
            {behind, Z} -> digraph:add_edge(G, lists:nth(X, List_vertex), lists:nth(Z, List_vertex));
            _ -> ok
        end
    end,
    lists:foreach(F, lists:seq(1, Nb)),
    Acycl = digraph_utils:is_acyclic(G),
    if  Acycl == true -> {ok, digraph_utils:topsort(G)};
        true -> Loop = digraph_utils:loop_vertices(G), {cycle, Loop}
    end.

update_state(State, Old_states, Id, Decision, Constructing_new_states) ->
    case Decision of 
        {speed, X} -> State#state_biker{speed=X, position=calc_position(Old_states, Id, X), energy=calc_energy_speed(Old_states, Id, X), behind=undefined, prev_decision=Decision};
        {behind, X} -> State#state_biker{speed=get_speed(Constructing_new_states, X), position=get_position(Constructing_new_states, X), energy=calc_energy_behind(Old_states, Id, Constructing_new_states, X), behind=X, prev_decision=Decision};
        {boost} -> State#state_biker{speed=0, position=calc_position_boost(Old_states, Id), energy=0,  behind=undefined, prev_decision=Decision}
    end.

calc_position_boost(States, Id) ->
    State = lists:nth(Id, States),
    State#state_biker.position + 3.87*math:sqrt(State#state_biker.energy).

calc_position(States, Id, Speed) ->
    State = lists:nth(Id, States),
    State#state_biker.position + Speed.

calc_energy_speed(States, Id, Speed) ->
    State = lists:nth(Id, States),
    case State of
        #state_biker{energy=E} -> E - 0.12 * Speed * Speed
    end.

calc_energy_behind(States, Id, New_States, OtherId) ->
    State = lists:nth(Id, States),
    case State of
        #state_biker{energy=E} -> E - 0.06 * get_speed(New_States, OtherId) * get_speed(New_States, OtherId)
    end.

get_position(States, Id) ->
    P = hd(lists:filter(fun(X) -> X#state_biker.id == Id end, States)),
    P#state_biker.position.

get_speed(States, Id) ->
    P = hd(lists:filter(fun(X) -> X#state_biker.id == Id end, States)),
    P#state_biker.speed.

get_energy(States, Id) ->
    P =hd(lists:filter(fun(X) -> X#state_biker.id == Id end, States)),
    P#state_biker.energy.

get_last_decision(States, Id) ->
    P =hd(lists:filter(fun(X) -> X#state_biker.id == Id end, States)),
    P#state_biker.prev_decision.

groupBy(F, L) -> lists:foldr(fun({K,V}, D) -> dict:append(K, V, D) end , dict:new(), [ {F(X), X} || X <- L ]).

display_positions(States) ->
    io:format("Displaying course state~n======================~nBiker = Position~n"),
    Comp_fct = fun(X, Y) -> {X#state_biker.position, X#state_biker.behind} < {Y#state_biker.position, Y#state_biker.behind} end,
    Dict_post = groupBy(fun(X) -> X#state_biker.position end, States),
    P = fun(K) -> 
        V = lists:map(fun(X) ->  integer_to_list(X#state_biker.id) end, lists:sort(Comp_fct,dict:fetch(K, Dict_post))),
        S = string:join(V, ", "),
        io:format("{~s} = ~p~n", [S, K])
    end,
    lists:foreach(P, lists:sort(fun(X,Y) -> X>Y end, dict:fetch_keys(Dict_post))).

have_win(States) ->
    Comp_fct = fun(X, Y) -> X#state_biker.position > Y#state_biker.position end,
    Sorted_states = lists:sort(Comp_fct, States),
    [H|_] = Sorted_states,
    if H#state_biker.position >= ?DISTANCE -> H#state_biker.id;
        true -> 
            P = fun(X) -> X#state_biker.energy =< 0 end, 
            No_more_energy_in_game = lists:all(P, Sorted_states) == true,
            if  No_more_energy_in_game -> nowinner;
                true -> false
            end
    end.


%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, kvstore),
    [{IndexNode, _Type}] = PrefList,
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