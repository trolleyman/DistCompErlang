%command to run: cat test.txt | escript tarry.erl

-module(tarry).
-export([main/0, main/1, node/3, node/4]).

% Needed, to disable interpreted mode, to allow us to reference functions
% below others in the file
-mode(compile).

% TODO: Check it works in the lab (different erlang version)

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
  {InitiatorId, OtherIds} = spawn_nodes(Nodes, InitiatorName),

  % Initiate tarry
  InitiatorId ! [start, [], []],

  receive [token, Token] ->
    io:format("~s~n", [fmt_token(Token)])
  end,

  % Tell nodes to exit
  lists:foreach(fun (Id) -> Id ! {exit, []} end, OtherIds),

  ok; % TODO: remove

main(_) ->
  halt(1).

% == Utils ==
shuffle_list(List) ->
  [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- List])].

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
      case string:lexemes(string:chomp(Line), " \t") of
        [] -> get_all_nodes(Acc);
        [Name | Neighbours] -> get_all_nodes([{Name, Neighbours}|Acc])
      end
  end.

% Spawn nodes, and return initiator ID
spawn_nodes(NodeSpec, InitiatorName) ->
  NodePids = [spawn(?MODULE, node, [Name, Name =:= InitiatorName, self()]) || {Name,_} <- NodeSpec],

  {NodeNames, NodeNeighbours} = lists:unzip(NodeSpec),

  Nodes = maps:from_list(lists:zip(NodeNames, lists:zip(NodePids, NodeNeighbours))),

  GetNeighbours = fun (NName) -> case maps:get(NName, Nodes) of {NPid, _} -> {NPid, NName} end end,

  maps:map(fun (_, {Pid, NeighbourNames}) -> Pid ! [neighbour_ids, lists:map(GetNeighbours, NeighbourNames)] end, Nodes),

  {InitiatorPid, _} = maps:get(InitiatorName, Nodes),

  {InitiatorPid, [Pid || {Name, {Pid, _}} <- maps:to_list(Nodes), Name =/= InitiatorName]}.


% Run node - waits for neighbour ids
node(Name, IsInitiator, MainProcess) ->
  ?DEBUG("~p ~s: Waiting for neighbour IDs...~n", [self(), Name]),
  receive [neighbour_ids, Neighbours] ->
      ?DEBUG("~p ~s: Received neighbours: ~s~n", [self(), Name, fmt_nbours(Neighbours)]),
      RandomNeighbours = shuffle_list(Neighbours),
      node(Name, IsInitiator, MainProcess, RandomNeighbours)
  end.

% Run initiator node
node(Name, true, MainProcess, []) ->
  receive
    [start, Token, []] ->
      ?DEBUG("~p ~s: Initiator: no neighbours, returning token to Main: ~s~n", [self(), Name, fmt_token(Token)]),
      MainProcess ! [token, lists:reverse([Name|Token])];
    [start, Token, [ParentId|Parents]] ->
      ?DEBUG("~p ~s: Initiator: no neighbours, returning token to parent ~p: ~s~n", [self(), Name, ParentId, fmt_token(Token)]),
      ParentId ! [start, [Name|Token], Parents],
      node(Name, true, MainProcess, []);
    Msg ->
      ?DEBUG("~p ~s: Unknown message: ~p~n", [self(), Name, Msg])
  end;

node(Name, true, MainProcess, Neighbours) ->
  receive
    [start, Token, Parents] ->
      [{NPid, NName}|RemainingNeighbours] = Neighbours,
      ?DEBUG("~p ~s: Initiator: got token, sending to neighbour ~s (~p): ~s~n", [self(), Name, NName, NPid, fmt_token(Token)]),
      NPid ! [start, [Name|Token], [self()|Parents]],
      node(Name, true, MainProcess, RemainingNeighbours);
    Msg ->
      ?DEBUG("~p ~s: Unknown message: ~p~n", [self(), Name, Msg])
  end;

% Run normal node
node(Name, false, MainProcess, Neighbours) ->
  receive
    [exit] -> ok;
    [start, Token, [ParentId|Parents]] ->
      % Get neighbours without ParentId
      FilteredNeighbours = [Neighbour || {Pid, _} = Neighbour <- Neighbours, Pid =/= ParentId],
      case FilteredNeighbours of
        [] ->
          % No remaining neighbours - return to parent
          ?DEBUG("~p ~s: no neighbours, returning token to parent (~p): ~s~n", [self(), Name, ParentId, fmt_token(Token)]),
          ParentId ! [start, [Name|Token], Parents],
          node(Name, false, MainProcess, []);
        [{NPid, NName}|_] ->
          ?DEBUG("~p ~s: got token, sending to neighbour ~s (~p): ~s~n", [self(), Name, NName, NPid, fmt_token(Token)]),
          NPid ! [start, [Name|Token], [self(),ParentId|Parents]],
          RemainingNeighbours = [Neighbour || {Pid, _} = Neighbour <- Neighbours, Pid =/= NPid],
          node(Name, false, MainProcess, RemainingNeighbours)
      end;
    Msg ->
      ?DEBUG("~p ~s: Unknown message: ~p~n", [self(), Name, Msg])
  end.
