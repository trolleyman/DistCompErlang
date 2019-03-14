%command to run: cat test.txt | escript tarry.erl

-module(tarry).
-compile(export_all).

% We use a standard format for passing around messages
% Messages are of form:
% {<type_of_message>, ...payload} where payload can be 0 or more parameters
% This makes it easier to match against specific messages that we expect
% For example "start_tarry" tells the initiator to start the algorithm
% "continue_tarry" tells a node (initiator or not) to carry on with the algorithm

main(Input) ->
  %io:fwrite("start called ~n"),

  % Read and tokenize input
  Content = read_input(Input),
  io:format("Content from stdin: ~s ~n", [Content]). %debug

read_input(Acc) ->
  case io:get_line("") of
    eof ->
      Acc;
    Line ->
      read_input(Acc ++ [Line])
  end.
