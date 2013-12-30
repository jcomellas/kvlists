%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2013 Juan Jose Comellas
%%% @doc
%%% Convenience functions for lists of key/value pairs.
%%% @end
%%%-------------------------------------------------------------------
-module(kvlists).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([delete/2]).
-export([get_value/2, get_value/3]).
-export([get_path/2]).
-export([get_path/3]).
-export([member/2]).
-export([set_nth/3]).
-export([set_path/3]).
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
-spec get_value(Key :: path_key(), List :: kvlist()) -> value() | undefined.
get_value(Key, List) ->
    get_value(Key, List, undefined).

%% @doc Returns the value of a simple key/value property in <code>List</code>.
%% If the <code>Key</code> is found in the list, this function returns the
%% corresponding <code>Value</code>, otherwise <code>Default</code> is returned.
%%
%% @see get_value/2
%% @see set_value/3
-spec get_value(Key :: path_key(), List :: kvlist(), Default :: value()) -> value().
get_value(Key, List, Default) when is_integer(Key) ->
    %% Integer (1-based position) keys.
    try lists:nth(Key, List) of
        Value -> Value
    catch
        _:_   -> Default
    end;
get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Value;
        false        -> Default
    end.


%% @doc Performs the lookup of a <code>Path</code> (list of keys) over a nested
%% <code>List</code> of key/value pairs. Each <code>path_key()</code> can
%% either be a name (<code>atom()</code> or <code>binary()</code>) or a
%% positive integer (using 1-based indexing). If no value is found corresponding
%% to the <code>Path</code> then <code>[]</code> is returned.
%%
%% @see set_path/3
-spec get_path(Path :: path(), List :: kvlist()) -> value().
get_path([Key | Tail], [Elem | _] = List) when is_integer(Key); is_tuple(Elem) ->
    %% Lookups on lists of key/value pairs.
    get_path_value(Key, fun (Value) -> get_path(Tail, Value) end, List);
get_path([Key | Tail], [Elem | _] = List) when is_list(Elem) ->
    %% Lookups on lists of lists of key/value pairs.
    get_path(Tail, multi_get_path(Key, List));
get_path(Key, List) when not is_list(Key) ->
    %% Scalar key lookups.
    get_path_value(Key, fun (Value) -> Value end, List);
get_path([], List) ->
    List;
get_path([_ | _], _List) ->
    [].

get_path_value(Key, Fun, List) when is_integer(Key) ->
    %% Integer (1-based position) keys.
    try lists:nth(Key, List) of
        Value -> Fun(Value)
    catch
        _:_   -> []
    end;
get_path_value(Key, Fun, List) ->
    %% Named (atom/binary) keys.
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Fun(Value);
        false        -> []
    end.

multi_get_path(Key, List) ->
    multi_get_path(Key, List, []).

multi_get_path(Key, [List | Tail], Acc) when is_list(List) ->
    NewAcc = case lists:keyfind(Key, 1, List) of
                 {Key, Value} -> [Value | Acc];
                 false        -> Acc
             end,
    multi_get_path(Key, Tail, NewAcc);
multi_get_path(Key, [_Elem | Tail], Acc) ->
    multi_get_path(Key, Tail, Acc);
multi_get_path(_Key, [], Acc) ->
    lists:reverse(Acc).


%% @doc Returns <code>true</code> if there is an entry in <code>List</code>
%% whose key is equal to <code>Key</code>, otherwise <code>false</code>.
-spec member(Key :: key(), List :: kvlist()) -> boolean().
member(Key, List) ->
    lists:keymember(Key, 1, List).


-spec set_nth(N :: non_neg_integer(), Value :: value(), List :: kvlist()) -> kvlist().
set_nth(N, Value, List) when is_integer(N), N > 0, is_list(List) ->
    set_nth(N, Value, List, []).

set_nth(N, Value, List, Acc) when N > 1 ->
    [Head | Tail] = case List of
                        [_ | _] -> List;
                        []      -> [undefined | []]
                    end,
    set_nth(N - 1, Value, Tail, [Head | Acc]);
set_nth(1, Value, [_Head | Tail], Acc) ->
    lists:reverse([Value | Acc], Tail);
set_nth(1, Value, [], Acc) ->
    lists:reverse([Value | Acc]).


%% @doc Assigns a <code>Value</code> to the element in a <code>List</code> of
%% key/value pairs corresponding to the <code>Key</code> that was passed. The
%% <code>Key</code> can be a sequence of names (<code>atom()</code> or
%% <code>binary()</code>) or indexes (1-based).
%%
%% @see get_path/2
-spec set_path(Path :: path(), Value :: value(), List :: kvlist()) -> kvlist().
set_path([Key], Value, List) ->
    set_path(Key, Value, List);
set_path([Key | Tail], Value, List) ->
    Elem = case get_path(Key, List) of
               Elem1 when is_list(Elem1) -> Elem1;
               _                         -> []
           end,
    set_value(Key, set_path(Tail, Value, Elem), List);
set_path(Key, Value, List) ->
    if
        is_list(List)->
            if is_list(hd(List)) ->
                    %% Set the value on multiple (identical) keys at the same time.
                    multi_set_path(Key, Value, List);
               true ->
                    set_value(Key, Value, List)
            end;
        is_integer(Key) ->
            set_nth(Key, Value, []);
        true ->
            [{Key, Value}]
    end.

multi_set_path(Key, Value, List) when is_list(Value) ->
    multi_set_path_list(Key, Value, List, []);
multi_set_path(Key, Value, List) ->
    multi_set_path_scalar(Key, Value, List, []).

multi_set_path_list(Key, [Value | ValueTail], [Elem | ElemTail], Acc) ->
    NewAcc = [lists:keyreplace(Key, 1, Elem, {Key, Value}) | Acc],
    multi_set_path_list(Key, ValueTail, ElemTail, NewAcc);
multi_set_path_list(Key, [Value | ValueTail], [], Acc) ->
    multi_set_path_list(Key, ValueTail, [], [[{Key, Value}] | Acc]);
multi_set_path_list(_Key, [], List, Acc) ->
    lists:reverse(Acc, List).

multi_set_path_scalar(Key, Value, [Elem | Tail], Acc) ->
    NewAcc = [lists:keyreplace(Key, 1, Elem, {Key, Value}) | Acc],
    multi_set_path_scalar(Key, Value, Tail, NewAcc);
multi_set_path_scalar(_Key, _Value, [], Acc) ->
    lists:reverse(Acc).


%% @doc Adds a property to the <code>List</code> with the corresponding
%% <code>Key</code> and <code>Value</code>.
%%
%% @see get_value/2
%% @see get_value/3
-spec set_value(Key :: path_key(), Value :: value(), List :: kvlist()) -> kvlist().
set_value(Key, Value, List) when is_integer(Key) ->
    %% Integer (1-based position) keys.
    set_nth(Key, Value, List);
set_value(Key, Value, List) ->
    %% Named (atom/binary) keys.
    lists:keystore(Key, 1, List, {Key, Value}).
