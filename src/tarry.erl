%command to run: cat test.txt | escript tarry.erl

-module(tarry).
-export([main/0, main/1, start_node/3]).

% Needed, to disable interpreted mode, to allow us to reference functions
% below others in the file
-mode(compile).

% TODO: Check it works in the lab (different erlang version)

%-ifdef(debug).
-define(DEBUG(Format, Args), io:format(Format, Args)).
%-else.
%-define(DEBUG(Format, Args), id(Format), id(Args)).
%-endif.

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
  {MegaSecs, Secs, MicroSecs} = now(),
  Hash = crypto:hash(md5, pid_to_list(self())),
  Seed = {MegaSecs + binary:at(Hash, 0), Secs + binary:at(Hash, 1), MicroSecs + binary:at(Hash, 2)},
  random:seed(Seed),
  ?DEBUG("Seed random: ~p~n", [Seed]),
  [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])].

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
  NodePids = [spawn(?MODULE, start_node, [Name, Name =:= InitiatorName, self()]) || {Name, _} <- NodeSpec],

  % Combine node PIDs into list
  {NodeNames, NodeNeighbours} = lists:unzip(NodeSpec),
  Nodes = lists:zip3(NodePids, NodeNames, NodeNeighbours),

  % Send neighbour info to all nodes
  GetNeighbours = fun (Name) -> case lists:keyfind(Name, 2, Nodes) of {Pid, _, _} -> {Pid, Name} end end,
  lists:foreach(fun ({Pid, _, NeighbourNames}) -> Pid ! [neighbour_ids, lists:map(GetNeighbours, NeighbourNames)] end, Nodes),

  % Get initiator PID, and return
  {InitiatorPid, _, _} = lists:keyfind(InitiatorName, 2, Nodes),
  OtherPids = [Pid || {Pid, _, _} <- Nodes, Pid =/= InitiatorPid],
  {InitiatorPid, OtherPids}.


% Run node - waits for neighbour ids
start_node(Name, IsInitiator, MainProcess) ->
  ?DEBUG("~p ~s: Waiting for neighbour IDs...~n", [self(), Name]),
  receive
    [neighbour_ids, Neighbours] ->
      ?DEBUG("~p ~s: Received neighbours: ~s~n", [self(), Name, fmt_nbours(Neighbours)]),
      RandomNeighbours = shuffle_list(Neighbours),
      case IsInitiator of
        true -> initiator_node(Name, MainProcess, RandomNeighbours);
        false -> normal_node(Name, RandomNeighbours, none)
      end;
    Msg ->
      ?DEBUG("~p ~s: Unknown message: ~p~n", [self(), Name, Msg])
  end.

% Run initiator node
initiator_node(Name, MainProcess, []) ->
  receive
    [start, Token, _] ->
      ?DEBUG("~p ~s: Initiator: no neighbours, returning token to Main: ~s~n", [self(), Name, fmt_token(Token)]),
      MainProcess ! [token, lists:reverse([Name|Token])];
    Msg ->
      ?DEBUG("~p ~s: Unknown message: ~p~n", [self(), Name, Msg])
  end;

initiator_node(Name, MainProcess, Neighbours) ->
  receive
    [start, Token, _] ->
      [{NPid, NName}|RemainingNeighbours] = Neighbours,
      ?DEBUG("~p ~s: Initiator: got token, sending to neighbour ~s (~p): ~s~n", [self(), Name, NName, NPid, fmt_token(Token)]),
      NPid ! [start, [Name|Token], self()],
      initiator_node(Name, MainProcess, RemainingNeighbours);
    Msg ->
      ?DEBUG("~p ~s: Unknown message: ~p~n", [self(), Name, Msg])
  end.

% Run normal node
normal_node(Name, Neighbours, OldParentId) ->
  receive
    [exit] -> ok;
    [start, Token, From] ->
      % Get neighbours without ParentId
      ParentId = case OldParentId of
        none -> From;
        _ -> OldParentId
      end,
      FilteredNeighbours = [Neighbour || {Pid, _} = Neighbour <- Neighbours, Pid =/= ParentId],
      case FilteredNeighbours of
        [] ->
          % No remaining neighbours - return to parent
          ?DEBUG("~p ~s: no neighbours, returning token to parent (~p): ~s~n", [self(), Name, ParentId, fmt_token(Token)]),
          ParentId ! [start, [Name|Token], self()],
          normal_node(Name, [], ParentId);
        [{NPid, NName} | RemainingNeighbours] ->
          ?DEBUG("~p ~s: got token, sending to neighbour ~s (~p): ~s~n", [self(), Name, NName, NPid, fmt_token(Token)]),
          NPid ! [start, [Name|Token], self()],
          normal_node(Name, RemainingNeighbours, ParentId)
      end;
    Msg ->
      ?DEBUG("~p ~s: Unknown message: ~p~n", [self(), Name, Msg])
  end.

