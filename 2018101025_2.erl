-module('2018101025_2').

-export([bellman_ford/2, main/1]).

-define(INF, 1000000000).

main(Args) ->
    [Input_file, _] = Args,
    % read input file
    {Ok, Input_desc} = file:open(Input_file, [read]),
    {Ok, [P, N, M]} = io:fread(Input_desc, [], "~d~d~d"),
    Edges = [{1, 2, 1}, {1, 3, 1}, {1, 4, 3}, {2, 4, 1},
	     {3, 4, 1}],
    Source = 1,
    % initialize distance array with infinity with dis[source] = 0
    Distance = lists:duplicate(Source - 1, ?INF) ++
		 [0] ++ lists:duplicate(N - Source, ?INF),
    % No of edges per process
    No_per_process = M div P,
    Pid_lst = create_process(self(), P, M, Edges, 1,
			     No_per_process, []),
    Ans = loop(P, 0, Pid_lst, Distance),
    io:format("Ans ~w ~n", [Ans]).

loop(P, P, _, Distance) -> Distance;
loop(P, Cnt, Pid_lst, Distance) ->
    [Pid ! {Distance} || Pid <- Pid_lst],
    Final_dis = send_distance(P, 0, Distance),
    io:format("FFFs ~w ~n", [Final_dis]),
    loop(P, Cnt + 1, Pid_lst, Final_dis).

send_distance(P, P, Distance) -> Distance;
send_distance(P, Cnt, Distance) ->
    receive
      {Upd_distance} ->
	  Final_dis = update(Upd_distance, Distance, [])
    end,
    send_distance(P, Cnt + 1, Final_dis).

update([], [], Final_dis) -> Final_dis;
update(Upd_distance, Distance, Final_dis) ->
    [Dis1 | Tail1] = Upd_distance,
    [Dis2 | Tail2] = Distance,
    update(Tail1, Tail2, Final_dis ++ [min(Dis1, Dis2)]).

create_process(Main_pid, P, M, Edges, Start,
	       No_per_process, Pid_lst) ->
    if P > 1 ->
	   % Distributing edges equally among all processes
	   Pid = spawn('2018101025_2', bellman_ford,
		       [Main_pid,
			lists:sublist(Edges, Start, No_per_process)]),
	   create_process(Main_pid, P - 1, M, Edges,
			  Start + No_per_process, No_per_process,
			  Pid_lst ++ [Pid]);
       P == 1 ->
	   % send remaining edges to last process
	   Pid = spawn('2018101025_2', bellman_ford,
		       [Main_pid, lists:sublist(Edges, Start, M - Start + 1)]),
	   Pid_lst ++ [Pid]
    end.

bellman_ford(Main_pid, Edges) ->
    % io:format("K ~w  ~n", [Edges]),
    receive
      {Distance} ->
	  %   io:format("OP ~w  ~n", [Distance]),
	  Updated_dist = update_dist(Edges, Distance),
	  Main_pid ! {Updated_dist}
    end,
    bellman_ford(Main_pid, Edges).

update_dist([], Distance) -> Distance;
update_dist(Edges, Distance) ->
    [Edg | Tail] = Edges,
    {Src, Dest, Cost} = Edg,
    % io:format("AA ~w ~w ~w~n", [Src, Dest, Cost]),
    A = lists:nth(Src, Distance),
    B = lists:nth(Dest, Distance),
    % io:format("BB ~w ~w ~w ~n", [A, B, Cost]),
    if B > A + Cost ->
	   Update_dist = lists:sublist(Distance, Dest - 1) ++
			   [A + Cost] ++ lists:nthtail(Dest, Distance);
       A > B + Cost ->
	   % undirected graph
	   Update_dist = lists:sublist(Distance, Src - 1) ++
			   [B + Cost] ++ lists:nthtail(Src, Distance);
       true -> Update_dist = Distance
    end,
    update_dist(Tail, Update_dist).
