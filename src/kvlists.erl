%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2013 Juan Jose Comellas
%%% @doc
%%% Convenience functions for lists of key/value pairs.
%%% @end
%%%-------------------------------------------------------------------
-module(kvlists).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([delete_nth/2]).
-export([delete_path/2]).
-export([delete_value/2]).
-export([equal/2]).
-export([get_value/2, get_value/3]).
-export([get_values/2]).
-export([get_path/2]).
-export([member/2]).
-export([set_nth/3]).
-export([set_path/3]).
-export([set_value/3]).
-export([set_values/2]).
-export([with/2]).
-export([without/2]).

-export_type([key/0, kv/0, kvlist/0, path/0, value/0]).

-type element_id() :: atom() | binary().
-type key()        :: atom() | binary().
-type value()      :: term().
-type kv()         :: {key(), value()}.
-type kvlist()     :: [kv()].
-type path_key()   :: key() | non_neg_integer() | {key(), element_id()}.
-type path()       :: [path_key()] | path_key().


-spec delete_nth(N :: non_neg_integer(), List :: kvlist()) -> kvlist().
delete_nth(N, List) when is_integer(N), N > 0, is_list(List) ->
    delete_nth(N, List, []).

delete_nth(N, [Head | Tail], Acc) when N > 1 ->
    delete_nth(N - 1, Tail, [Head | Acc]);
delete_nth(1, [_Head | Tail], Acc) ->
    lists:reverse(Acc, Tail);
delete_nth(_N, [], Acc) ->
    lists:reverse(Acc).


-spec delete_path(Path :: path(), List :: kvlist()) -> value().
delete_path([], _List) ->
    [];
delete_path(_Path, Elem) when not is_list(Elem) ->
    Elem;
delete_path(PathKey, List) when not is_list(PathKey) ->
    delete_path_value(PathKey, List);
delete_path([PathKey], List) ->
    delete_path_value(PathKey, List);
delete_path([{_Key, _ElemId} | _PathTail] = Path, List) ->
    %% Set path on element of a list whose Key matches the ElemId.
    delete_path_by_element_id(Path, List);
delete_path([PathKey | PathTail], List) ->
    Elem = case get_path(PathKey, List) of
               Elem1 when is_list(Elem1) -> Elem1;
               Other                    -> Other
           end,
    set_path_value(PathKey, delete_path(PathTail, Elem), List).


delete_path_value(Index, List) when is_integer(Index), is_list(List) ->
    %% Integer (1-based position) keys.
    delete_nth(Index, List);
delete_path_value(PathKey, [Head | _Tail] = List) when is_list(Head) ->
    case PathKey of
        %% Set path on element of a list whose Key matches the ElemId.
        {_Key, _ElemId} -> delete_path_by_element_id([PathKey], List);
        %% Set the value on multiple (identical) keys at the same time.
        _               -> delete_path_by_element_key(PathKey, List)
    end;
delete_path_value(Key, List) when not is_list(Key) ->
    %% Named (atom/binary) keys.
    lists:keydelete(Key, 1, List).


%% @doc Delete element of a list choosing it by element ID (i.e. when a
%% Key matches the ElemId).
delete_path_by_element_id(Path, List) ->
    delete_path_by_element_id(Path, List, []).

delete_path_by_element_id([{Key, ElemId} | PathTail] = Path, [Elem | ElemTail], Acc) when is_list(Elem) ->
    case lists:keyfind(Key, 1, Elem) of
        {Key, ElemId} ->
            NewElemTail = case delete_path(PathTail, Elem) of
                              []      -> ElemTail;
                              NewElem -> [NewElem | ElemTail]
                          end,
            lists:reverse(Acc, NewElemTail);
        _ ->
            delete_path_by_element_id(Path, ElemTail, [Elem | Acc])
    end;
delete_path_by_element_id(Path, [Elem | ElemTail], Acc) ->
    delete_path_by_element_id(Path, ElemTail, [Elem | Acc]);
delete_path_by_element_id(_Path, [], Acc) ->
    lists:reverse(Acc).


delete_path_by_element_key(Key, List) ->
    delete_path_by_element_key(Key, List, []).

delete_path_by_element_key(Key, [Elem | Tail], Acc) ->
    delete_path_by_element_key(Key, Tail, [lists:keydelete(Key, 1, Elem) | Acc]);
delete_path_by_element_key(_Key, [], Acc) ->
    lists:reverse(Acc).


%% @doc Deletes all entries associated with <code>Key</code> from
%% <code>List</code>.
-spec delete_value(Key :: key(), List :: kvlist()) -> kvlist().
delete_value(Key, List) ->
    lists:keydelete(Key, 1, List).


%% @doc Compares two lists and returns a boolean indicating whether both lists
%% are equal. Two kvlists are equal when they have the same keys with the same
%% values, independently of the position of each key in the list.
-spec equal(List1 :: kvlist(), List2 :: kvlist()) -> boolean().
equal([{Key, Value1} | Tail1], [{Key, Value2} | Tail2]) ->
    Equal = if
                is_list(Value1), is_list(Value2) -> equal(Value1, Value2);
                true                             -> Value1 =:= Value2
            end,
    if
        Equal -> equal(Tail1, Tail2);
        true -> false
    end;
equal([{Key, Value1} | Tail1], List2) ->
    case lists:keytake(Key, 1, List2) of
        {value, {Key, Value2}, Tail2} when is_list(Value1), is_list(Value2) ->
            case equal(Value1, Value2) of
                true  -> equal(Tail1, Tail2);
                false -> false
            end;
        {value, {Key, Value1}, Tail2} when not is_list(Value1) ->
            equal(Tail1, Tail2);
        {value, {Key, _Value2}, _NewList2} ->
            false;
        false ->
            false
    end;
equal(Element, Element) ->
    true;
equal(_List1, _List2) ->
    false.



%% @doc Performs the lookup of a <code>Path</code> (list of keys) over a nested
%% <code>List</code> of key/value pairs. Each <code>path_key()</code> can
%% either be a name (<code>atom()</code> or <code>binary()</code>); a
%% positive integer (using 1-based indexing) or an element identifier
%% (<code>{Key, ElementId}</code>). If no value is found corresponding
%% to the <code>Path</code> then <code>[]</code> is returned.
%%
%% @see set_path/3
-spec get_path(Path :: path(), List :: kvlist()) -> value().
get_path([Index | Tail], [Elem | _] = List) when is_integer(Index); is_tuple(Elem) ->
    %% Lookups on lists of key/value pairs.
    case get_path_value(Index, List) of
        [_ | _] = Value        -> get_path(Tail, Value);
        Other when Tail =:= [] -> Other;
        _                      -> []
    end;
get_path([PathKey | Tail], [Elem | _] = List) when is_list(Elem) ->
    %% Lookups on lists of lists of key/value pairs.
    get_path(Tail, get_path_value(PathKey, List));
get_path(PathKey, List) when not is_list(PathKey) ->
    %% Scalar key lookups.
    get_path_value(PathKey, List);
get_path([], List) ->
    List;
get_path([_ | _], _List) ->
    [].

get_path_value(PathKey, List) when is_integer(PathKey) ->
    %% Integer (1-based position) keys.
    try lists:nth(PathKey, List) of
        Value -> Value
    catch
        _:_   -> []
    end;
get_path_value({_Key, _ElemId} = PathKey, List) ->
    get_path_by_element_id(PathKey, List);
get_path_value(Key, [Elem | _Tail] = List) when is_list(Elem) ->
    get_path_by_element_key(Key, List);
get_path_value(Key, List) ->
    %% Named (atom/binary) keys.
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Value;
        false        -> []
    end.

get_path_by_element_id({Key, ElemId} = PathKey, [Elem | Tail]) when is_list(Elem) ->
    case lists:keyfind(Key, 1, Elem) of
        {Key, ElemId} -> Elem;
        _Other        -> get_path_by_element_id(PathKey, Tail)
    end;
get_path_by_element_id(PathKey, [_Elem | Tail]) ->
    get_path_by_element_id(PathKey, Tail);
get_path_by_element_id(_PathKey, []) ->
    [].

get_path_by_element_key(Key, List) ->
    get_path_by_element_key(Key, List, []).

get_path_by_element_key(Key, [List | Tail], Acc) when is_list(List) ->
    NewAcc = case lists:keyfind(Key, 1, List) of
                 {Key, Value} -> [Value | Acc];
                 false        -> Acc
             end,
    get_path_by_element_key(Key, Tail, NewAcc);
get_path_by_element_key(Key, [_Elem | Tail], Acc) ->
    get_path_by_element_key(Key, Tail, Acc);
get_path_by_element_key(_Key, [], Acc) ->
    lists:reverse(Acc).


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
        {_Key, Value} -> Value;
        false         -> Default
    end.


%% @doc Returns the list of values corresponding to the different <code>Keys</code>
%% in <code>List</code>. If the entry in <code>Keys</code> is found in the
%% <code>List</code>, this function returns the corresponding value. If the
%% entry is not found and it's a <code>{Key, Default}</code> tuple,
%% <code>Default</code> is added to the returned list in its place and if the
%% entry is just a key, then <code>undefined</code> is added to the returned list.
%%
%% @see get_value/2
%% @see set_values/2
-spec get_values([Key :: key() | {Key :: key(), Default :: value()}],
                 List :: kvlist()) -> Values :: [value()].
get_values(Keys, List) ->
    get_values(Keys, List, []).

get_values([{Key, Default} | Tail], List, Acc) ->
    get_values(Tail, List, [get_value(Key, List, Default) | Acc]);
get_values([Key | Tail], List, Acc) ->
    get_values(Tail, List, [get_value(Key, List) | Acc]);
get_values([], _List, Acc) ->
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
%% <code>binary()</code>); indexes (1-based) or element identifiers
%% (<code>{Key, ElementId}</code>).
%%
%% @see get_path/2
-spec set_path(Path :: path(), Value :: value(), List :: kvlist()) -> kvlist().
set_path([], Value, _List) ->
    Value;
set_path(PathKey, Value, List) when not is_list(PathKey) ->
    set_path_value(PathKey, Value, List);
set_path([PathKey], Value, List) ->
    set_path_value(PathKey, Value, List);
set_path([{_Key, _ElemId} | _PathTail] = Path, Value, List) ->
    %% Set path on element of a list whose Key matches the ElemId.
    set_path_by_element_id(Path, Value, List);
set_path([PathKey | PathTail], Value, List) ->
    Elem = case get_path(PathKey, List) of
               Elem1 when is_list(Elem1) -> Elem1;
               _                         -> []
           end,
    set_path_value(PathKey, set_path(PathTail, Value, Elem), List).


set_path_value(Index, Value, List0) when is_integer(Index) ->
    List = if
               is_list(List0) -> List0;
               true           -> [List0]
           end,
    %% Integer (1-based position) keys.
    set_nth(Index, Value, List);
set_path_value(PathKey, Value, List) when is_tuple(PathKey), is_list(List) ->
    %% Set path on element of a list whose Key matches the ElemId.
    set_path_by_element_id([PathKey], Value, List);
set_path_value(PathKey, Value, [Head | _Tail] = List) when is_list(Head) ->
    %% Set the value on multiple (identical) keys at the same time.
    set_path_by_element_key(PathKey, Value, List);
set_path_value(Key, Value, List) when not is_list(Key) ->
    %% Named (atom/binary) keys.
    lists:keystore(Key, 1, List, {Key, Value}).


%% @doc Set path on element of a list choosing it by element ID (i.e. when a
%% Key matches the ElemId).
set_path_by_element_id(Path, Value, List) ->
    set_path_by_element_id(Path, Value, List, []).

set_path_by_element_id([{Key, ElemId} | PathTail] = Path, Value, [Elem | ElemTail], Acc) when is_list(Elem) ->
    case lists:keyfind(Key, 1, Elem) of
        {Key, ElemId} -> lists:reverse(Acc, [set_path(PathTail, Value, Elem) | ElemTail]);
        _             -> set_path_by_element_id(Path, Value, ElemTail, [Elem | Acc])
    end;
set_path_by_element_id(Path, Value, [Elem | ElemTail], Acc) ->
    set_path_by_element_id(Path, Value, ElemTail, [Elem | Acc]);
set_path_by_element_id([{_Key, _ElemId} = PathKey | PathTail], Value, [], Acc) ->
    %% If no element was found with the correct ID, we add an element at the
    %% of the list with the given ElemId.
    lists:reverse([set_path(PathTail, Value, [PathKey]) | Acc]).


set_path_by_element_key(Key, Value, List) when is_list(Value) ->
    set_path_by_element_key_on_list(Key, Value, List, []);
set_path_by_element_key(Key, Value, List) ->
    set_path_by_element_key_on_scalar(Key, Value, List, []).

set_path_by_element_key_on_list(Key, [Value | ValueTail], [Elem | ElemTail], Acc) ->
    NewAcc = [lists:keyreplace(Key, 1, Elem, {Key, Value}) | Acc],
    set_path_by_element_key_on_list(Key, ValueTail, ElemTail, NewAcc);
set_path_by_element_key_on_list(Key, [Value | ValueTail], [], Acc) ->
    set_path_by_element_key_on_list(Key, ValueTail, [], [[{Key, Value}] | Acc]);
set_path_by_element_key_on_list(_Key, [], List, Acc) ->
    lists:reverse(Acc, List).

set_path_by_element_key_on_scalar(Key, Value, [Elem | Tail], Acc) ->
    NewAcc = [lists:keyreplace(Key, 1, Elem, {Key, Value}) | Acc],
    set_path_by_element_key_on_scalar(Key, Value, Tail, NewAcc);
set_path_by_element_key_on_scalar(_Key, _Value, [], Acc) ->
    lists:reverse(Acc).


%% @doc Adds a property to the <code>List</code> with the corresponding
%% <code>Key</code> and <code>Value</code>.
%%
%% @see get_value/2
%% @see get_value/3
-spec set_value(Key :: key(), Value :: value(), List :: kvlist()) -> kvlist().
set_value(Key, Value, List) ->
    %% Named (atom/binary) keys.
    lists:keystore(Key, 1, List, {Key, Value}).


%% @doc Sets each <code>Key</code> in <code>List</code> to its corresponding
%% <code>Value</code>.
%%
%% @see get_values/2
%% @see set_value/3
-spec set_values([{Key :: key(), Value :: value()}], List :: kvlist()) ->
                        NewList :: kvlist().
set_values([{Key, Value} | Tail], List) ->
    set_values(Tail, set_value(Key, Value, List));
set_values([], List) ->
    List.


%% @doc Return a <code>NewList</code> where the <code>Key</code> of each
%% element is present in the list of <code>Keys</code>.
%%
%% @see without/2
-spec with(Keys :: [key()], List :: kvlist()) -> NewList :: kvlist().
with(Keys, List) ->
    lists:filter(fun (Elem) -> with_filter(Elem, Keys) end, List).


%% @doc Return a <code>NewList</code> where the <code>Key</code> of each
%% element is not present in the list of <code>Keys</code>.
%%
%% @see with/2
-spec without(Keys :: [key()], List :: kvlist()) -> NewList :: kvlist().
without(Keys, List) ->
    lists:filter(fun (Elem) -> not with_filter(Elem, Keys) end, List).


with_filter({Key, _Value}, Keys) -> lists:member(Key, Keys);
with_filter(_Elem, _Keys)        -> false.
