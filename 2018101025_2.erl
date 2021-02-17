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
    % io:format("~w  ~w ~n", [lists:nth(1, Edges), Source]),
    % initialize distance array with infinity with dis[source] = 0
    Distance = lists:duplicate(Source - 1, ?INF) ++
		 [0] ++ lists:duplicate(N - Source, ?INF),
    % No of edges per process
    No_per_process = M div P,
    % io:format("~w  ~n",
    %       [lists:sublist(Edges, 1, No_per_process)]),
    Pid_lst = create_process(self(), P, M, Edges, 1,
			     No_per_process, []),
    % io:format("Pids ~w ~n", [Pid_lst]),
    [Pid ! {Distance} || Pid <- Pid_lst],
    send_distance(Distance).

send_distance(Distance) ->
    receive
      {Upd_distance} -> io:format("DS ~w ~n", [Upd_distance])
    end,
    send_distance(Distance).

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
    io:format("K ~w  ~n", [Edges]),
    receive
      {Distance} ->
	  %   io:format("OP ~w  ~n", [Distance]),
	  Updated_dist = update_dist(Edges, Distance),
	  Main_pid ! {Updated_dist}
    end.

update_dist([], Distance) -> Distance;
update_dist(Edges, Distance) ->
    [Edg | Tail] = Edges,
    {Src, Dest, Cost} = Edg,
    % io:format("AA ~w ~w ~w~n", [Src, Dest, Cost]),
    A = lists:nth(Src, Distance),
    B = lists:nth(Dest, Distance),
    if B > A + Cost ->
	   Update_dist = lists:sublist(Distance, Dest - 1) ++
			   [A + Cost] ++ lists:nthtail(Dest, Distance);
       A > B + Cost ->
	   % undirected graph
	   Update_dist = lists:sublist(Distance, Src - 1) ++
			   [B + Cost] ++ lists:nthtail(Src, Distance)
    end,
    update_dist(Tail, Update_dist).

% update_dist(Edg, Distance) ->
%     {Src, Dest, Cost} = Edg,
%     A = lists:nth(Src, Distance),
%     B = lists:nth(Dest, Distance),
%     if B > A + Cost ->
% 	   io:format("~w ~w ~w~n", [A, Dest, Cost]);
%        % undirected graph
%        A > B + Cost -> io:format("~w ~n", [Cost])
%     end.

