%command to run: cat test.txt | escript tarry.erl

-module(tarry).
-export([main/0, main/1, init_node/1]).

% Needed, to disable interpreted mode, to allow us to reference functions
% below others in the file
-mode(compile).

-ifdef(debug).
-define(DEBUG(Format, Args), io:format(Format, Args)).
-else.
-define(DEBUG(Format, Args), id(Format), id(Args)).
-endif.

main() ->
  main([]).

main([]) ->
  % Read and tokenize input
  {InitiatorName, Nodes} = get_nodes(),

  ?DEBUG("Nodes: ~p~n", [Nodes]),
  ?DEBUG("Initiator: ~p~n", [InitiatorName]),

  % Spawn nodes
  {InitiatorId, AllIds} = spawn_nodes(Nodes, InitiatorName),

  % Initiate tarry
  InitiatorId ! [token, [], self()],

  receive [token, Token, _] ->
    io:format("~s~n", [fmt_token(lists:reverse(Token))])
  end,

  % Tell nodes to exit
  lists:foreach(fun (Id) -> Id ! [exit] end, AllIds);


main(_) ->
  halt(1).

% == Utils ==
shuffle_list(List) ->
  {MegaSecs, Secs, MicroSecs} = now(),
  Hash = crypto:hash(md5, pid_to_list(self())),
  Seed = {MegaSecs + binary:at(Hash, 0), Secs + binary:at(Hash, 1), MicroSecs + binary:at(Hash, 2)},
  random:seed(Seed),
  ?DEBUG("Seed random: ~p~n", [Seed]),
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

id(X) -> X.

% == Main functions ==
% Get nodes from stdin
get_nodes() -> 
  [{InititatorName, _}|Nodes] = get_all_nodes([]),
  {InititatorName, Nodes}.

get_all_nodes(Acc) ->
  case io:get_line("") of
    eof ->
      lists:reverse(Acc);
    Line ->
      case string:tokens(Line, " \t\n\r\n") of
        [] -> get_all_nodes(Acc);
        [Name | Neighbours] -> get_all_nodes([{Name, Neighbours}|Acc])
      end
  end.

% Spawn nodes, and return initiator ID
spawn_nodes(NodeSpec, InitiatorName) ->
  % Spawn nodes, and keep PIDs in a list
  NodePids = [spawn(?MODULE, init_node, [Name]) || {Name, _} <- NodeSpec],

  % Combine node PIDs into list
  {NodeNames, NodeNeighbours} = lists:unzip(NodeSpec),
  Nodes = lists:zip3(NodePids, NodeNames, NodeNeighbours),

  % Send neighbour info to all nodes
  GetNeighbours = fun (Name) -> case lists:keyfind(Name, 2, Nodes) of {Pid, _, _} -> {Pid, Name} end end,
  lists:foreach(fun ({Pid, _, NeighbourNames}) -> Pid ! [neighbour_ids, lists:map(GetNeighbours, NeighbourNames)] end, Nodes),

  % Get initiator PID, and return
  {InitiatorPid, _, _} = lists:keyfind(InitiatorName, 2, Nodes),
  AllPids = [Pid || {Pid, _, _} <- Nodes],
  {InitiatorPid, AllPids}.


% Run node - waits for neighbour ids
init_node(Name) ->
  ?DEBUG("~p ~s: Waiting for neighbour IDs...~n", [self(), Name]),
  receive
    [neighbour_ids, Neighbours] ->
      ?DEBUG("~p ~s: Received neighbours: ~s~n", [self(), Name, fmt_nbours(Neighbours)]),
      RandomNeighbours = shuffle_list(Neighbours),
      node(Name, RandomNeighbours, none);
    Msg ->
      ?DEBUG("~p ~s: Unknown message: ~p~n", [self(), Name, Msg])
  end.

% Run node
node(Name, Neighbours, OldParentId) ->
  receive
    [exit] -> ok;
    [token, Token, From] ->
      % Get neighbours without ParentId
      ParentId = case OldParentId of
        none -> From;
        _ -> OldParentId
      end,
      FilteredNeighbours = [Neighbour || {Pid, NName} = Neighbour <- Neighbours, Pid =/= ParentId, not list_contains(Token, NName)],
      case FilteredNeighbours of
        [] ->
          % No remaining neighbours - return to parent
          ?DEBUG("~p ~s: no neighbours, returning token to parent (~p): ~s~n", [self(), Name, ParentId, fmt_token(Token)]),
          ParentId ! [token, [Name|Token], self()],
          node(Name, [], ParentId);
        [{NPid, NName} | RemainingNeighbours] ->
          ?DEBUG("~p ~s: remaining neighbours: ~p~n", [self(), Name, RemainingNeighbours]),
          ?DEBUG("~p ~s: got token, sending to neighbour ~s (~p): ~s~n", [self(), Name, NName, NPid, fmt_token(Token)]),
          NPid ! [token, [Name|Token], self()],
          node(Name, RemainingNeighbours, ParentId)
      end;
    Msg ->
      ?DEBUG("~p ~s: Unknown message: ~p~n", [self(), Name, Msg])
  end.

