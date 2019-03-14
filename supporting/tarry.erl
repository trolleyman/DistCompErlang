%Flawed but often working implementation of Tarry's algorithm
%command to run: cat test.txt | escript tarry.erl

-module(tarry).
-export([main/1, node/3]).

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
  %io:format("Input content: ~s ~n", [Content]),

  [_ | Nodes] = tokenize(Content),
  %io:format("Nodes: ~s ~n", [Nodes]),

  % Spawn nodes and send them their neighbours
  InitiatorId = set_up_nodes(Nodes),

  % Tell initiator to start Tarry
  InitiatorId ! {"start_tarry", []},

  % Receive token and output it
  receive {"token", Token} ->
    print_formatted(Token)
  end.


% Output Operations

% Adds a space in between node names and print them
print_formatted(Token) ->
  FormattedToken = lists:join(" ", Token),
  io:format("~s ~n", [FormattedToken]).


% Input Operations
read_input(Acc) ->
  case io:get_line("") of
    eof ->
      Acc;
    Line ->
      read_input(Acc ++ [Line])
  end.

tokenize(InputList) ->
  lists:map(fun(L) -> string:tokens(L, " \n") end, InputList).


% Nodes setup
set_up_nodes(Nodes) ->
  % io:fwrite("set_up_nodes called ~n"),
  % io:format("Initiator is ~s ~n", [Initiator]),

  % Spawn nodes
  NodeNames = [NodeName || [NodeName | _] <- Nodes], %Filter to get individual nodes in graph
  %io:format("All node names: ~s ~n", [NodeNames]),

  InitiatorName = lists:nth(1, NodeNames),

  % bug on this line
  Pids = [spawn(tarry, node, [NodeName, NodeName =:= InitiatorName, self()]) || NodeName <- NodeNames],

  NodeNamesAndIds = lists:zip(NodeNames, Pids), 

  % Send neighbour ids list to each node
  NeighbourIdLists = get_neighbour_ids(Nodes, NodeNamesAndIds),
  _ = [Pid ! {"neighbour_ids", NeighbourIds} || {Pid, NeighbourIds} <- lists:zip(Pids, NeighbourIdLists)],

  % Return process id of initiator
  lists:nth(1, Pids).

% Helper that returns the neighbours list of each node
% Format of return is [[{name, pid}, {name, pid}], [{name, pid}]]
get_neighbour_ids(Nodes, NodeNamesAndIds) ->
  % io:fwrite("get_neighbour_ids called ~n"),
  % io:format("NodeNamesAndIds: ~p ~n", [NodeNamesAndIds]),

  NeighbourLists = [NeighbourList || [_ | NeighbourList] <- Nodes],
  NeighbourListsWithIds = [[{NodeName, get_pid_for_node_name(NodeName, NodeNamesAndIds)} || NodeName <- NeighbourList] ||
    NeighbourList <- NeighbourLists],

  % io:fwrite("get_neighbour_ids ended ~n"),
  NeighbourListsWithIds.


% Create a new node (initiator or normal node)
node(NodeName, InitiatorFlag, MainProcess) ->
  % io:format("Node ~p spawned. Initiator? ~p ~n", [NodeName, InitiatorFlag]),
  receive
    {"neighbour_ids", NeighbourIds} when InitiatorFlag ->
      % io:format("Neighbour ids of initiator (~p): ~p ~n", [NodeName, NeighbourIds]),
      initiator(NodeName, NeighbourIds, MainProcess);
    {"neighbour_ids", NeighbourIds} ->
      % io:format("Neighbour ids of node ~p: ~p ~n", [NodeName, NeighbourIds]),
      normal_node(NodeName, NeighbourIds, [])
  end.

% In the following lines the notion of Neighbours refers to possible children
% i.e channels that haven't been used so far and could potentially be used next
% so whenever we send through a channel we then remove that neighbour from the list

% Initiator behaviour in Tarry algorithm

% If the initiator has no neighbours
% then just send back the updated token (list of names) to the main process
initiator(NodeName, [], MainProcess) ->
  receive
    {"start_tarry", Token} ->
      % io:format("Initiator ~s received start_tarry. No neighbours ~n", [NodeName]),
      NewToken = Token ++ NodeName,
      % io:format("Token is now ~p ~n", [NewToken]),
      MainProcess ! {"token", NewToken};

    {"continue_tarry", Token, _} ->
      % io:format("Initiator ~s received continue_tarry. No neighbours ~n", [NodeName]),
      NewToken = Token ++ NodeName,
      % io:format("Token is now ~p ~n", [NewToken]),
      MainProcess ! {"token", NewToken}
  end;

% If there are any neighbours left randomly send the updated token to one of them
% There is some code duplication in this function that we couldn't (wouldn't) avoid
initiator(NodeName, NeighbourIds, MainProcess) ->
  receive
    {"start_tarry", Token} ->
      % io:format("Initiator ~p received start_tarry. Neighbours: ~p ~n", [NodeName, NeighbourIds]),
      NewToken = Token ++ NodeName,
      [NeighbourToSend | RemainingNeighbours] = randomise_list(NeighbourIds),
      {_, Pid} = NeighbourToSend,
      Pid ! {"continue_tarry", NewToken, self()},
      % io:format("Token is now ~p ~n", [NewToken]),
      initiator(NodeName, RemainingNeighbours, MainProcess);

    {"continue_tarry", Token, _} ->
      % io:format("Initiator ~p received continue_tarry. Neighbours: ~p ~n", [NodeName, NeighbourIds]),
      NewToken = Token ++ NodeName,
      [NeighbourToSend | RemainingNeighbours] = randomise_list(NeighbourIds),
      {_, Pid} = NeighbourToSend,
      Pid ! {"continue_tarry", NewToken, self()},
      % io:format("Token is now ~p ~n", [NewToken]),
      initiator(NodeName, RemainingNeighbours, MainProcess)
  end.


% Normal node behaviour
% Again note that after we send to a neighbour we remove it from the list
% so we don't use the same channel twice

% If the node has no neighbours and no parent
% Register the node that has just sent it a message as its parent
% And just send back the updated token to the parent
normal_node(NodeName, [], []) ->
  receive
    {"continue_tarry", Token, From} ->
      % io:format("Node ~p received continue_tarry from node ~p. No parents, no neighbours ~n", [NodeName, From]),
      % io:format("New parent is: ~p ~n", [From]),
      NewToken = Token ++ NodeName,
      From ! {"continue_tarry", NewToken, self()},
      % io:format("Token is now ~p ~n", [NewToken]),
      normal_node(NodeName, [], From)
  end;

% If the node has no neighbours but has a parent
% Send the updated token back to its parent
normal_node(NodeName, [], Parent) ->
  receive
    {"continue_tarry", Token, _} ->
      % io:format("Node ~p received continue_tarry from node ~p. Parent ~p, no neighbours ~n", [NodeName, From, Parent]),
      NewToken = Token ++ NodeName,
      Parent ! {"continue_tarry", NewToken, self()},
      % io:format("Token is now ~p ~n", [NewToken]),
      normal_node(NodeName, [], Parent)
  end;

% If the node has neighbours but no parent
% Register the new parent
% Remove it from the list of possible children
% (because once we got the token from the parent we only send it back when we're out of options)
% and randomly send the updated token to one of the neighbours
normal_node(NodeName, NeighbourIds, []) ->
  receive
    {"continue_tarry", Token, From} ->
      % io:format("Node ~p received continue_tarry from node ~p. No parents, neighbours: ~p ~n", [NodeName, From, NeighbourIds]),
      % io:format("New parent is: ~p ~n", [From]),
      NewToken = Token ++ NodeName,
      [NeighbourToSend | RemainingNeighbours] = randomise_list(NeighbourIds),
      {_, Pid} = NeighbourToSend,
      Pid ! {"continue_tarry", NewToken, self()},
      % io:format("Token is now ~p ~n", [NewToken]),

      % Remove parent from neighbour list
      % (We only send to parent if we're out of neighbours, now parent is not a possible child anymore)
      RemainingNeighboursWithoutParent = lists:filter(fun({_, Id}) -> Id =/= From end, RemainingNeighbours),

      normal_node(NodeName, RemainingNeighboursWithoutParent, From)
  end;

% If the node has neighbours and a parent
% Randomly send the updated token to one of the neighbours
normal_node(NodeName, NeighbourIds, Parent) ->
  receive
    {"continue_tarry", Token, _} ->
      % io:format("Node ~p received continue_tarry from node ~p. Parent ~p, neighbours: ~p ~n", [NodeName, From, Parent, NeighbourIds]),
      NewToken = Token ++ NodeName,
      [NeighbourToSend | RemainingNeighbours] = randomise_list(NeighbourIds),
      {_, Pid} = NeighbourToSend,
      Pid ! {"continue_tarry", NewToken, self()},
      % io:format("Token is now ~p ~n", [NewToken]),
      normal_node(NodeName, RemainingNeighbours, Parent)
  end.


% Helper functions

% Gets the process id of node denoted by NodeName
get_pid_for_node_name(NodeName, NodeNamesAndIds) ->
  {_, Pid} = lists:keyfind(NodeName, 1, NodeNamesAndIds),
  Pid.

% Shuffles around the elements of a list
randomise_list(List) ->
  [X || {_, X} <- lists:sort([ {rand:uniform(), Y} || Y <- List])].