% command to run: cat input.txt | escript tarry.erl

-module(tarry).
-export([main/0, main/1, init_node/3]).

% Needed, to disable interpreted mode, to allow us to reference functions
% below others in the file
-mode(compile).

% == Utils ==
fdebug(true, Format, Args) ->
  io:format(Format, Args);
fdebug(false, _, _) ->
  ok.

shuffle_list(List) ->
  {MegaSecs, Secs, MicroSecs} = now(),
  Hash = crypto:hash(md5, pid_to_list(self())),
  Seed = {MegaSecs + binary:at(Hash, 0), Secs + binary:at(Hash, 1), MicroSecs + binary:at(Hash, 2)},
  random:seed(Seed),
  [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])].

list_contains([], _) -> false;
list_contains([Item|List], SearchItem) ->
  case SearchItem =:= Item of
    true -> true;
    false -> list_contains(List, SearchItem)
  end.

fmt_token(Token) ->
  string:join(Token, " ").

fmt_nbours(NBours) ->
  string:join([Name || {_, Name} <- NBours], " ").

% == Main functions ==
main() ->
  main([]).

main(Args) ->
  IsVerbose = list_contains(Args, "--verbose"),
  IsDepthFirst = not list_contains(Args, "--breadth-first"),

  % Read and tokenize input
  {InitiatorName, NodeSpec} = get_node_spec(),

  fdebug(IsVerbose, "Nodes: ~p~n", [NodeSpec]),
  fdebug(IsVerbose, "Initiator: ~p~n", [InitiatorName]),

  % Spawn nodes
  {InitiatorId, AllIds} = spawn_nodes(IsVerbose, IsDepthFirst, NodeSpec, InitiatorName),

  % Initiate tarry
  Token = run_tarry(InitiatorId, AllIds),
  
  io:format("~s~n", [fmt_token(Token)]).

% Get node spec from stdin
get_node_spec() -> 
  [{InititatorName, _}|Nodes] = get_node_spec([]),
  {InititatorName, Nodes}.

get_node_spec(Acc) ->
  case io:get_line("") of
    eof ->
      lists:reverse(Acc);
    Line ->
      case string:tokens(Line, " \t\n\r\n") of
        [] -> get_node_spec(Acc);
        [Name | Neighbours] -> get_node_spec([{Name, Neighbours}|Acc])
      end
  end.

% Spawn nodes, and return initiator ID
spawn_nodes(IsVerbose, IsDepthFirst, NodeSpec, InitiatorName) ->
  % Spawn nodes, and keep PIDs in a list
  NodePids = [spawn(?MODULE, init_node, [IsVerbose, IsDepthFirst, Name]) || {Name, _} <- NodeSpec],

  % Combine node PIDs into list of tuples {Pid, Name, NeighbourNames}
  {NodeNames, NodeNeighbours} = lists:unzip(NodeSpec),
  Nodes = lists:zip3(NodePids, NodeNames, NodeNeighbours),

   % GetPid gets the PID of the node with the specified name, and returns a tuple of that PID, and the name passed.
  GetPid = fun (Name) -> case lists:keyfind(Name, 2, Nodes) of {Pid, _, _} -> {Pid, Name} end end,

  % Send neighbour info to all nodes
  lists:foreach(fun ({Pid, _, NeighbourNames}) -> Pid ! [neighbour_ids, lists:map(GetPid, NeighbourNames)] end, Nodes),

  % Get initiator PID, and return
  {InitiatorPid, _, _} = lists:keyfind(InitiatorName, 2, Nodes),

  % Get list of all PIDs, so that we know which nodes to send exit message to
  AllPids = [Pid || {Pid, _, _} <- Nodes],
  {InitiatorPid, AllPids}.

% Run tarry algorithm, once all the nodes are set up correctly.
% Also, clean up the nodes once the algorithm is complete.
% Send initiation token
run_tarry(InitiatorId, AllIds) ->
  % Send start token
  InitiatorId ! [token, [], self()],

  % Receive token
  receive [token, RevToken, _] ->
    % Clean up nodes
    lists:foreach(fun (Id) -> Id ! [exit] end, AllIds),

    % Reverse token so that it is in the correct direction,
    % and return it. We do this so that it is more efficient
    % when appending to the token throughout the tarry
    lists:reverse(RevToken)
  end.

% Run node - waits for neighbour ids
init_node(IsVerbose, IsDepthFirst, Name) ->
  fdebug(IsVerbose, "~p ~s: Waiting for neighbour IDs...~n", [self(), Name]),
  receive
    [neighbour_ids, Neighbours] ->
      fdebug(IsVerbose, "~p ~s: Received neighbours: ~s~n", [self(), Name, fmt_nbours(Neighbours)]),
      RandomNeighbours = shuffle_list(Neighbours),
      node(IsVerbose, IsDepthFirst, Name, RandomNeighbours, none);
    Msg ->
      fdebug(IsVerbose, "~p ~s: Unknown message: ~p~n", [self(), Name, Msg])
  end.

% Run node
node(IsVerbose, IsDepthFirst, Name, Neighbours, OldParentId) ->
  receive
    [exit] -> ok;
    [token, Token, From] ->
      % Get neighbours without ParentId
      ParentId = case OldParentId of
        none -> From;
        _ -> OldParentId
      end,
      FilteredNeighbours = case IsDepthFirst of
        true  -> [Neighbour || {Pid, NName} = Neighbour <- Neighbours, Pid =/= ParentId, not list_contains(Token, NName)];
        false -> [Neighbour || {Pid, _    } = Neighbour <- Neighbours, Pid =/= ParentId]
      end,
      case FilteredNeighbours of
        [] ->
          % No remaining neighbours - return to parent
          fdebug(IsVerbose, "~p ~s: no neighbours, returning token to parent (~p): ~s~n", [self(), Name, ParentId, fmt_token(Token)]),
          ParentId ! [token, [Name|Token], self()],
          node(IsVerbose, IsDepthFirst, Name, [], ParentId);
        [{NPid, NName} | RemainingNeighbours] ->
          fdebug(IsVerbose, "~p ~s: remaining neighbours: ~p~n", [self(), Name, RemainingNeighbours]),
          fdebug(IsVerbose, "~p ~s: got token, sending to neighbour ~s (~p): ~s~n", [self(), Name, NName, NPid, fmt_token(Token)]),
          NPid ! [token, [Name|Token], self()],
          node(IsVerbose, IsDepthFirst, Name, RemainingNeighbours, ParentId)
      end;
    Msg ->
      fdebug(IsVerbose, "~p ~s: Unknown message: ~p~n", [self(), Name, Msg])
  end.

