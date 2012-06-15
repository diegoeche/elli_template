%% @author Johannes Huning <johannes.huning@wooga.net>
%% @author John-Paul Bader <john-paul.bader@wooga.net>
%% @copyright 2012 Wooga GmbH

%% @doc JSON and orddict related functions.
% Any manipulation of JSON should be made through these functions.
-module (bi_json).

% Import internal representation to use.
-include ("bi_json.hrl").

% All functions mey be publicly used.
-compile (export_all).

%% @doc Insert a new element to the JSON-object.
store(Json, Key, Value) ->
    % Note that orddict:store inserts elements sorted, which orddict:fetch
    % assumes to be true to operate.
    ?JSON_IMPL:store(Key, Value, Json).


%% @doc Create a new JSON-struct in the underlying representation.
create() ->
    % For orddict, this would be the empty list: [].
    ?JSON_IMPL:new().


%% @doc Construct a new JSON struct from the given proplist.
% By calling `store`, of the underlying representation, recursively for every
% element passed in, the resulting structure will be adhere to the rules of its
% representation. In the case of orddicts, this is order of its keys.
construct(Proplist = [{_,_}|_]) ->
    Step = fun({Key, Value}, Json) ->
        store(Json, Key, construct(Value))
    end,
    lists:foldl(Step, create(), Proplist);


% For all other elements which are not proplists, return their identity; only
% proplists need recursive folding to ensure order in the underlying orddict.
construct(Literal) -> Literal.


%% @doc Encode a JSON binary from a JSON struct or literal.
encode(Term) ->
  jiffy:encode(pack(construct(Term))).


%% @doc Decode a JSON binary to a JSON struct or literal.
decode(Binary) ->
  construct(unpack(jiffy:decode(Binary))).


% JSON is encoded from and decoded to recursive JSON-structures as used by
% jiffy, see https://github.com/davisp/jiffy for more information.
% Thus any structure obtained via jiffy:decode needs to be unpacked first.
% That is stripped of extra tuples and list constructors.

%% @doc Attempts to extract a orddict from the given jiffy-JSON.
unpack(Json) when is_list(Json) orelse is_tuple(Json) ->
  unpack(Json, orddict:new());

%% Only tuples and list require deeper unpacking, return simple structs.
unpack(Json) -> Json.

%% @doc Recursively unpacks a nested jiffy-JSON object.
unpack({Proplist}, Dict) when is_list(Proplist) ->
  lists:foldl(
    fun({Key, Value}, Acc) ->
      orddict:store(Key, unpack(Value), Acc)
    end,
    Dict,
    Proplist
  );

% List of jiffy-JSON => list of unpacked structs.
unpack(List, _) when is_list(List) ->
  [unpack(Elem) || Elem <- List].

%% @doc Recursively builds a jiffy-JSON struct from the given orddict.
% Single orddict => jiffy-JSON object.
pack(Orddict = [Head|_]) when is_list(Orddict) andalso is_tuple(Head) ->
  {orddict:fold(
    fun(Key, Value, Acc) ->
      Acc ++ [{Key, pack(Value)}]
    end,
    [],
    Orddict
  )};

% Treat the empty list as an empty object.
pack([]) -> [];

% List of orddicts => list of jiffy-JSON objects.
pack(List) when is_list(List) ->
  [pack(Elem) || Elem <- List];

pack(undefined) -> null;

% Simple term => same simple term.
pack(Value) -> Value.
