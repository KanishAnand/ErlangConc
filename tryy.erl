-module(tryy).

-export([main/0, ring/2]).

main() ->
    {_, Fout} = file:open("output.txt", [write]),
    Pid = self(),
    Root_pid = spawn(tryy, ring, [0, Pid]),
    Root_pid ! {Fout},
    receive {_} -> io:format("BB ~w", [2]) end.

ring(0, Pid) ->
    receive
      {Fout} -> io:format(Fout, "Bocesasfds", []), Pid ! {1}
    end.
