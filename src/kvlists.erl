%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2013 Juan Jose Comellas
%%% @doc
%%% Convenience functions for lists of key-value pairs.
%%% @end
%%%-------------------------------------------------------------------
-module(kvlists).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([delete/2]).
-export([get_value/2, get_value/3]).
-export([get_path/2]).
-export([set_value/3]).

-export_type([key/0, kv/0, kvlist/0, path/0, value/0]).

-type key()       :: atom() | binary().
-type value()     :: term().
-type kv()        :: {key(), value()}.
-type kvlist()    :: [kv()].
-type path_key()  :: key() | non_neg_integer().
-type path()      :: [path_key()] | path_key().

%% @doc Deletes all entries associated with <code>Key</code> from
%% <code>List</code>.
-spec delete(Key :: key(), List :: kvlist()) -> kvlist().
delete(Key, List) ->
    lists:keydelete(Key, 1, List).


%% @equiv get_value(Key, List, undefined)
-spec get_value(Key :: key(), List :: kvlist()) -> value() | undefined.
get_value(Key, List) ->
    get_value(Key, List, undefined).

%% @doc Returns the value of a simple key/value property in <code>List</code>.
%% If the <code>Key</code> is found in the list, this function returns the
%% corresponding <code>Value</code>, otherwise <code>Default</code> is returned.
%%
%% @see get_value/2
%% @see set_value/3
-spec get_value(Key :: key(), List :: kvlist(), Default :: value()) -> value().
get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Value;
        false        -> Default
    end.


%% @doc Performs the lookup of a <code>Path</code> (list of keys) over a nested
%% <code>List</code> of key/value pairs. Each <code>path_key()</code> can
%% either be a name (<code>atom()</code> or <code>binary()</code>) or a
%% positive integer (with 1-based indexing).
-spec get_path(Path :: path(), List :: kvlist()) -> value().
get_path([Key | Tail], [Elem | _] = List) when is_integer(Key); is_tuple(Elem) ->
    %% Lookups on lists of key-value pairs.
    path_key(Key, fun (Value) -> get_path(Tail, Value) end, List);
get_path([Key | Tail], [Elem | _] = List) when is_list(Elem) ->
    %% Lookups on lists of lists of key-value pairs.
    get_path(Tail, path_filter(Key, List));
get_path(Key, List) when not is_list(Key) ->
    %% Scalar key lookups.
    path_key(Key, fun (Value) -> Value end, List);
get_path([], List) ->
    List;
get_path([_ | _], _List) ->
    [].

path_key(Key, Fun, List) when is_integer(Key) ->
    %% Integer (1-based position) keys.
    try lists:nth(Key, List) of
        Value -> Fun(Value)
    catch
        _:_   -> []
    end;
path_key(Key, Fun, List) ->
    %% Named (atom/binary) keys.
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Fun(Value);
        false        -> []
    end.

path_filter(Key, List) ->
    path_filter(Key, List, []).

path_filter(Key, [List | Tail], Acc) when is_list(List) ->
    NewAcc = case lists:keyfind(Key, 1, List) of
                 {Key, Value} -> [Value | Acc];
                 false        -> Acc
             end,
    path_filter(Key, Tail, NewAcc);
path_filter(Key, [_Elem | Tail], Acc) ->
    path_filter(Key, Tail, Acc);
path_filter(_Key, [], Acc) ->
    lists:reverse(Acc).


%% @doc Adds a property to the <code>List</code> with the corresponding
%% <code>Key</code> and <code>Value</code>.
%%
%% @see get_value/2
%% @see get_value/3
-spec set_value(Key :: key(), Value :: value(), List :: kvlist()) -> kvlist().
set_value(Key, Value, List) ->
    lists:keystore(Key, 1, List, {Key, Value}).
