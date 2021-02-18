-module('2018101025_1').

-export([main/1, ring/3]).

main(Args) ->
    [Input_file, Output_file] = Args,
    % read input file
    {Ok, Fin} = file:open(Input_file, [read]),
    {Ok, [P, Token]} = io:fread(Fin, [], "~d~d"),
    file:close(Fin),
    % Open output file
    {Ok, Fout} = file:open(Output_file, [write]),
    % spwan root 0 process
    Root_pid = spawn('2018101025_1', ring,
		     [0, self(), Fout]),
    % create all processes and form ring from them
    create_process(P - 1, Root_pid, Root_pid, Token, Fout),
    % close output file when all processes are finished
    receive {_} -> file:close(Fout) end.

create_process(ID, Prev_pid, Root_pid, Token, Fout) ->
    Pid = spawn('2018101025_1', ring, [ID, Prev_pid, Fout]),
    if ID > 1 ->
	   create_process(ID - 1, Pid, Root_pid, Token, Fout);
       ID == 1 -> Root_pid ! {1, Pid, Token}
    end.

ring(0, Main_pid, Fout) ->
    receive
      % get next node for 0th node
      {_, Next_pid, Token} -> Next_pid ! {0, Token};
      {Sender_ID, Token} ->
	  io:format(Fout,
		    "Process 0 recieved token ~w from process "
		    "~w. ~n",
		    [Token, Sender_ID]),
	  % send back message when all processes are done
	  Main_pid ! {1},
	  halt()
    end,
    ring(0, Main_pid, Fout);
ring(ID, Next_pid, Fout) ->
    receive
      {Sender_ID, Token} ->
	  io:format(Fout,
		    "Process ~w recieved token ~w from process "
		    "~w. ~n",
		    [ID, Token, Sender_ID]),
	  % send message to next node of ring
	  Next_pid ! {ID, Token}
    end.
