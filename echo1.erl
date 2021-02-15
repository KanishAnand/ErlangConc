-module(echo).

-export([main/0, ring/0]).

main() ->
    % dfas
    Pid = spawn(echo, ring, []),
    Pid ! {self(), "Token"},
    receive
      {Parent_id, Token} ->
	  io:format("Process ~w recieved token ~w from process "
		    "~w.~n,",
		    [self(), Token, Parent_id])
    end.

% Input = [read_integers(File, [])],
% read_integers(Device, Acc) ->
%     case io:fread(Device, [], "~d~d") of
%       eof -> lists:reverse(Acc);
%       {ok, [D1, D2]} ->
% 	  read_integers(Device, [{D1, D2} | Acc]);
%       {error, What} ->
% 	  io:format("io:fread error: ~w~n", [What]),
% 	  read_integers(Device, Acc)
%     end.

ring() ->
    receive
      {Parent_id, Token} ->
	  if Token == "token" ->
		 io:format("Process ~w recieved token ~w from process "
			   "~w.~n",
			   [self(), Token, Parent_id]),
		 Pid = spawn(echo, ring, []),
		 Pid ! {self(), Token};
	     % else
	     Token == "Token" ->
		 % dfas
		 io:format("Hello"),
		 Pid = spawn(echo, main, []),
		 Pid ! {self(), Token}
	  end
    end.
