-module('2018101025_2').

-export([bellman_ford/4, main/1]).

-define(INF, 1000000000).

main(Args) ->
    [Input_file, Output_file] = Args,
    % read input file
    {Ok, Fin} = file:open(Input_file, [read]),
    {Ok, [P, N, M]} = io:fread(Fin, [], "~d~d~d"),
    Edges = read_edges(Fin, M, []),
    {Ok, [Source]} = io:fread(Fin, [], "~d"),
    file:close(Fin),
    % initialize distance array with all values infinity but dis[source] = 0
    Distance = lists:duplicate(Source - 1, ?INF) ++
		 [0] ++ lists:duplicate(N - Source, ?INF),
    % No of edges per process: To distribute edges equally among all processes
    No_per_process = M div P,
    % create all processes and store their pid's
    Pid_lst = create_process(self(), P, M, N, Edges, 1,
			     No_per_process, []),
    Final_dist = loop(N, P, 0, Pid_lst, Distance),
    io:format("Ans ~w ~n", [Final_dist]),
    {Ok, Fout} = file:open(Output_file, [write]),
    % write output to file
    [io:format(Fout, "~w ~w~n",
	       [Ind, lists:nth(Ind, Final_dist)])
     || Ind <- lists:seq(1, N)],
    file:close(Output_file).

read_edges(_, 0, Edges) -> Edges;
read_edges(Fin, M, Edges) ->
    {_, [Src, Dest, Cost]} = io:fread(Fin, [], "~d~d~d"),
    read_edges(Fin, M - 1, Edges ++ [{Src, Dest, Cost}]).

create_process(Main_pid, P, M, N, Edges, Start,
	       No_per_process, Pid_lst) ->
    if P > 1 ->
	   % Distributing edges equally among all processes
	   Pid = spawn('2018101025_2', bellman_ford,
		       [Main_pid, 0, N,
			lists:sublist(Edges, Start, No_per_process)]),
	   create_process(Main_pid, P - 1, M, N, Edges,
			  Start + No_per_process, No_per_process,
			  Pid_lst ++ [Pid]);
       P == 1 ->
	   % send remaining edges to last process
	   Pid = spawn('2018101025_2', bellman_ford,
		       [Main_pid, 0, N,
			lists:sublist(Edges, Start, M - Start + 1)]),
	   % return Pid list to main
	   Pid_lst ++ [Pid]
    end.

loop(N, _, N, _, Distance) ->
    % run loop N times and then return Distance
    Distance;
loop(N, P, Cnt, Pid_lst, Distance) ->
    % send updated distance array to all pid's
    [Pid ! {Distance} || Pid <- Pid_lst],
    Final_dist = recieve_distance(P, 0, Distance),
    io:format("Updated ~w ~n", [Final_dist]),
    loop(N, P, Cnt + 1, Pid_lst, Final_dist).

recieve_distance(P, P, Distance) -> Distance;
recieve_distance(P, Cnt, Distance) ->
    % receive distance from each process, merge all and send back for next iteration
    receive
      {Updated_dist} ->
	  Final_dist = merge(Updated_dist, Distance, [])
    end,
    recieve_distance(P, Cnt + 1, Final_dist).

merge([], [], Final_dist) -> Final_dist;
merge(Updated_dist, Distance, Final_dist) ->
    % merge all distance array returned by taking minimum of each index elements and return
    [Dis1 | Tail1] = Updated_dist,
    [Dis2 | Tail2] = Distance,
    merge(Tail1, Tail2, Final_dist ++ [min(Dis1, Dis2)]).

bellman_ford(_, N, N, _) -> N;
bellman_ford(Main_pid, Cnt, N, Edges) ->
    % io:format("Edges ~w  ~n", [Edges]),
    receive
      {Distance} ->
	  Updated_dist = update_dist(Edges, Distance),
	  % send back update distance to main process
	  Main_pid ! {Updated_dist}
    end,
    bellman_ford(Main_pid, Cnt + 1, N, Edges).

update_dist([], Distance) ->
    % return update distance after looping over all edges
    Distance;
update_dist(Edges, Distance) ->
    [Edg | Tail] = Edges,
    {Src, Dest, Cost} = Edg,
    % io:format("AA ~w ~w ~w~n", [Src, Dest, Cost]),
    A = lists:nth(Src, Distance),
    B = lists:nth(Dest, Distance),
    % io:format("BB ~w ~w ~w ~n", [A, B, Cost]),
    if B > A + Cost ->
	   Updated_dist = lists:sublist(Distance, Dest - 1) ++
			    [A + Cost] ++ lists:nthtail(Dest, Distance);
       A > B + Cost ->
	   % undirected graph
	   Updated_dist = lists:sublist(Distance, Src - 1) ++
			    [B + Cost] ++ lists:nthtail(Src, Distance);
       true -> Updated_dist = Distance
    end,
    update_dist(Tail, Updated_dist).
