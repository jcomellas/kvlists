# kvlists


## Overview

[kvlists](https://github.com/jcomellas/kvlists) is a module that can manipulate
lists of key/value pairs in Erlang. It should be quite useful when dealing with
medium to large-sized nested property lists or decoded JSON documents (in the
format used by [jsx](https://github.com/talentdeficit/jsx)). Its interface
is similar to that of the `proplists` module, with the addition of nested key
(*path*) retrieval and modification (loosely inspired by Bob Ippolito's great
[kvc](https://github.com/etrepum/kvc) library, which itself was inspired by
Apple's [NSKeyValueCoding](https://developer.apple.com/library/ios/documentation/Cocoa/Reference/Foundation/Protocols/NSKeyValueCoding_Protocol/Reference/Reference.html)
protocol from Objective-C). `kvlists` provides functionality that is similar to
that of [XPath](http://www.w3.org/TR/xpath/), but with a syntax specifically
adapted to Erlang. It supports lists of key/value pairs where the keys are
either **atoms** or **binaries** and their type specification is:
```erlang
[{Key :: atom() | binary(), Value :: term()}]
```
Where `Value` can also be a nested list of key/value tuples.


## Requirements

You should use a recent version of Erlang/OTP (the project has only been tested
with Erlang R16B so far). You also need [GNU make](https://www.gnu.org/software/make/)
and a recent version of [rebar](https://github.com/rebar/rebar) in the system path.


## Installation

Pull the project from GitHub by running:

    git pull https://github.com/jcomellas/kvlists.git

To compile the module you simply run `make` from the project's directory and to
execute the unit tests you run `make test`.  To build the (very) limited
documentation run `make doc`.

If you want to have the `kvlists` application available after the module
is compiled, insert it into the Erlang lib directory (e.g. by symlinking or
copying) by running:

    sudo ln -s . /usr/lib/erlang/lib/kvlists-`git describe --tags`


## Status

This application has been lightly tested so far and is still in a development
stage. It has a test suite that covers most of the functionality but it has not
been used in production yet.


## Type Specifications

The type specifications exported by the module are:
```erlang
-type element_id() :: atom() | binary().
-type key()        :: atom() | binary().
-type value()      :: term().
-type kv()         :: {key(), value()}.
-type kvlist()     :: [kv()].
-type path_key()   :: key() | non_neg_integer() | {key(), element_id()}.
-type path()       :: [path_key()] | path_key().
```


## Functions

The `kvlists` module also provides the following functions:

  * [delete_path/2](#delete_path2)
  * [delete_value/2](#delete_value2)
  * [get_path/2](#get_path2)
  * [get_value/2](#get_value2)
  * [get_value/3](#get_value3)
  * [get_values/2](#get_values2)
  * [member/2](#member2)
  * [set_path/3](#set_path3)
  * [set_value/3](#set_value3)
  * [set_values/2](#set_values2)
  * [with/2](#with2)
  * [without/2](#without2)

-------------

### delete_path/2
Deletes the element that matches a `Path` (list of nested keys) in a nested
`List` of key/value pairs. Each key in the `Path` can be a name (`atom()` or
`binary()`); a positive integer (using 1-based indexing); or a tuple that
looks like `{Key, ElementId}`. If the latter path key is used, then the
function will try to match the element (tuple) in a list whose `Key` has the
value `ElementId` and continue with the following path key. If no value is
found corresponding to the `Path` then `List` is returned. If the `Path` is
set to `[]` then `[]` will be returned.

#### Specification
```erlang
-spec delete_path(Path :: path(), List :: kvlist()) -> value().
```
#### Examples

Given:
```erlang
1> List = [{transactions,
            [{period, <<"3 months">>},
             {total, 3659},
             {completed, 3381},
             {canceled, 278},
             {ratings, [[{type, positive}, {percent, 99}],
                        [{type, negative}, {percent, 0}],
                        [{type, neutral}, {percent, 1}]]}]}].
```
Delete an empty key:
```erlang
2> kvlists:delete_path([], List).
[]
```
Delete a key with a single (scalar) value:
```erlang
3> kvlists:delete_path([transactions, total], List).
[{transactions,[{period,<<"3 months">>},
                {completed,3381},
                {canceled,278},
                {ratings,[[{type,positive},{percent,99}],
                          [{type,negative},{percent,0}],
                          [{type,neutral},{percent,1}]]}]}]
```
Delete a non-existent key:
```erlang
4> kvlists:delete_path(invalid_key, List).
[{transactions,[{period,<<"3 months">>},
                {total,3659},
                {completed,3381},
                {canceled,278},
                {ratings,[[{type,positive},{percent,99}],
                          [{type,negative},{percent,0}],
                          [{type,neutral},{percent,1}]]}]}]
```
Delete a nested key with a list of key/value pair lists:
```erlang
5> kvlists:delete_path([transactions, ratings], List).
[{transactions,[{period,<<"3 months">>},
                {total,3659},
                {completed,3381},
                {canceled,278}]}]
```
Delete some key/value pairs associated to a nested key by name and index:
```erlang
6> kvlists:delete_path([transactions, ratings, 2], List).
[{transactions,[{period,<<"3 months">>},
                {total,3659},
                {completed,3381},
                {canceled,278},
                {ratings,[[{type,positive},{percent,99}],
                          [{type,neutral},{percent,1}]]}]}]
```
Delete a single value associated to a nested key by name and index:
```erlang
7> kvlists:delete_path([transactions, ratings, 3, percent], List).
[{transactions,[{period,<<"3 months">>},
                {total,3659},
                {completed,3381},
                {canceled,278},
                {ratings,[[{type,positive},{percent,99}],
                          [{type,negative},{percent,0}],
                          [{type,neutral}]]}]}]
```
Delete multiple values associated to a nested key present in several elements
of a list:
```erlang
8> kvlists:delete_path([transactions, ratings, percent], List).
[{transactions,[{period,<<"3 months">>},
                {total,3659},
                {completed,3381},
                {canceled,278},
                {ratings,[[{type,positive}],
                          [{type,negative}],
                          [{type,neutral}]]}]}]
```
Delete an element in a list by element ID:
```erlang
9> kvlists:delete_path([transactions, ratings, {type, negative}], List).
[{transactions,[{period,<<"3 months">>},
                {total,3659},
                {completed,3381},
                {canceled,278},
                {ratings,[[{type,positive},{percent,99}],
                          [{type,neutral},{percent,1}]]}]}]
```
Delete a value from an element in a list by element ID:
```erlang
10> kvlists:delete_path([transactions, ratings, {type, neutral}, percent], List).
[{transactions,[{period,<<"3 months">>},
                {total,3659},
                {completed,3381},
                {canceled,278},
                {ratings,[[{type,positive},{percent,99}],
                          [{type,negative},{percent,0}],
                          [{type,neutral}]]}]}]
```

-------------

### delete_value/2
Deletes all entries associated with `Key` from `List`.

#### Specification
```erlang
-spec delete_value(Key :: key(), List :: kvlist()) -> kvlist().
```
#### Example
```erlang
1> List = [{abc, 123}, {def, 456}, {ghi, 789}].
2> kvlists:delete_value(def, List).
[{abc,123},{ghi,789}]
```

-------------

### get_path/2
Performs the lookup of a `Path` (list of nested keys) over a nested `List` of
key/value pairs. Each key in the `Path` can be a name (`atom()` or `binary()`);
a positive integer (using 1-based indexing); or a tuple that looks like
`{Key, ElementId}`. If the latter path key is used, then the function will try
to match the element (tuple) in a list whose `Key` has the value `ElementId`
and continue the lookup with the following path key. If no value is found
corresponding to the `Path` then `[]` is returned.

#### Specification
```erlang
-spec get_path(Path :: path(), List :: kvlist()) -> value().
```
#### Examples

Given:
```erlang
1> List = [{transactions,
            [{period, <<"3 months">>},
             {total, 3659},
             {completed, 3381},
             {canceled, 278},
             {ratings, [[{type, positive}, {percent, 99}],
                        [{type, negative}, {percent, 0}],
                        [{type, neutral}, {percent, 1}]]}]}].
```
Retrieve an empty key:
```erlang
2> kvlists:get_path([], List).
[{transactions,[{period,<<"3 months">>},
                {total,3659},
                {completed,3381},
                {canceled,278},
                {ratings,[[{type,positive},{percent,99}],
                          [{type,negative},{percent,0}],
                          [{type,neutral},{percent,1}]]}]}]
```
Retrieve a key with a single (scalar) value:
```erlang
3> kvlists:get_path([transactions, total], List).
3659
```
Retrieve a non-existent key:
```erlang
4> kvlists:get_path(invalid_key, List).
[]
```
Retrieve a nested key with a list of key/value pair lists:
```erlang
5> kvlists:get_path([transactions, ratings], List).
[[{type, positive}, {percent, 99}],
 [{type, negative}, {percent, 0}],
 [{type, neutral}, {percent, 1}]].
```
Retrieve some key/value pairs associated to a nested key by name and index:
```erlang
6> kvlists:get_path([transactions, ratings, 2], List).
[{type, negative}, {percent, 0}].
```
Retrieve a single value associated to a nested key by name and index:
```erlang
7> kvlists:get_path([transactions, ratings, 3, percent], List).
1
```
Retrieve multiple values associated to a nested key present in several elements
of a list:
```erlang
8> kvlists:get_path([transactions, ratings, percent], List).
[99,0,1]
```
Retrieve multiple values associated to a nested key present in several elements
of a list:
```erlang
9> kvlists:get_path([transactions, ratings, type], List).
[positive,negative,neutral]
```
Retrieve an element in a list by element ID:
```erlang
10> kvlists:get_path([transactions, ratings, {type, negative}], List).
[{type,negative},{percent,0}]
```
Retrieve a value from an element in a list by element ID:
```erlang
11> kvlists:get_path([transactions, ratings, {type, neutral}, percent], List).
1
```

-------------

### get_value/2
Equivalent to `get_value(Key, List, undefined)`.

#### Specification
```erlang
-spec get_value(Key :: key(), List :: kvlist()) -> value() | undefined.
```
#### Example
```erlang
1> List = [{abc, 123}, {def, 456}, {ghi, 789}].
2> kvlists:get_value(ghi, List).
789
3> kvlists:get_value(jkl, List).
undefined
```

-------------

### get_value/3
Returns the value of a simple key/value property in `List`. If the `Key` is
found in the list, this function returns the corresponding `Value`, otherwise
`Default` is returned.

#### Specification
```erlang
-spec get_value(Key :: path_key(), List :: kvlist(), Default :: value()) -> value().
```
#### Example
```erlang
1> List = [{abc, 123}, {def, 456}, {ghi, 789}].
2> kvlists:get_value(ghi, List, 100).
789
3> kvlists:get_value(jkl, List, 100).
100
```

-------------

### get_values/2
Returns the list of values corresponding to the different `Keys` in `List`.
If the entry in `Keys` is found in the `List`, this function returns the
corresponding value. If the entry is not found and it's a `{Key, Default}`
tuple, `Default` is added to the returned list in its place and if the entry
is just a key, then `undefined` is added to the returned list.

#### Specification
```erlang
-spec get_values([Key :: path_key() | {Key :: path_key(), Default :: value()}],
                 List :: kvlist()) -> Values :: [value()].
```
#### Example
```erlang
1> List = [{abc, 123}, {def, 456}, {ghi, 789}].
2> kvlists:get_values([ghi], List).
[789]
3> kvlists:get_values([abc, {def, 100}, ghi, {jkl, 200}], List).
[123, 456, 789, 200]
```

-------------

### member/2
Returns `true` if there is an entry in `List` whose key is equal to `Key`,
otherwise `false`.

#### Specification
```erlang
-spec member(Key :: key(), List :: kvlist()) -> boolean().
```
#### Example
```erlang
1> List = [{abc, 123}, {def, 456}, {ghi, 789}].
2> kvlists:member(ghi, List).
true
3> kvlists:member(jkl, List).
false
```

-------------

### set_path/3
Assigns a `Value` to the element in a `List` of key/value pairs corresponding to
the `Path` that was passed. The `Path` can be a sequence of: names (`atom()` or
`binary()`); indexes (1-based); or a tuple that looks like `{Key, ElementId}`.
If the latter path key is used, then the function will try to match the element
(tuple) in a list whose `Key` has the value `ElementId` and continue the lookup
with the following path key.

#### Specification
```erlang
-spec set_path(Path :: path(), Value :: value(), List :: kvlist()) -> kvlist().
```
#### Examples

Given:
```erlang
1> List = [{transactions,
            [{period, <<"3 months">>},
             {total, 3659},
             {completed, 3381},
             {canceled, 278},
             {ratings, [[{type, positive}, {percent, 99}],
                        [{type, negative}, {percent, 0}],
                        [{type, neutral}, {percent, 1}]]}]}].
```
Set a value with an empty path:
```erlang
2> kvlists:set_path([], <<"6 months">>, List).
<<"6 months">>
```
Set a value by key name:
```erlang
3> kvlists:set_path([transactions, period], <<"6 months">>, List).
[{transactions,[{period,<<"6 months">>},
                {total,3659},
                {completed,3381},
                {canceled,278},
                {ratings,[[{type,positive},{percent,99}],
                          [{type,negative},{percent,0}],
                          [{type,neutral},{percent,1}]]}]}]
```
Set individual value by key name and index in list:
```erlang
4> kvlists:set_path([transactions, ratings, 2, percent], 55, List).
[{transactions,[{period,<<"3 months">>},
                {total,3659},
                {completed,3381},
                {canceled,278},
                {ratings,[[{type,positive},{percent,99}],
                          [{type,negative},{percent,55}],
                          [{type,neutral},{percent,1}]]}]}]
```
Set multiple entries in a list with a single value:
```erlang
5> kvlists:set_path([transactions, ratings, percent], 123, List).
[{transactions,[{period,<<"3 months">>},
                {total,3659},
                {completed,3381},
                {canceled,278},
                {ratings,[[{type,positive},{percent,123}],
                          [{type,negative},{percent,123}],
                          [{type,neutral},{percent,123}]]}]}]
```
Set multiple entries in a list with a multiple values:
```erlang
6> kvlists:set_path([transactions, ratings, percent], [10, 20, 30, 40], List).
[{transactions,[{period,<<"3 months">>},
                {total,3659},
                {completed,3381},
                {canceled,278},
                {ratings,[[{type,positive},{percent,10}],
                          [{type,negative},{percent,20}],
                          [{type,neutral},{percent,30}],
                          [{percent,40}]]}]}]
```
Set a single value in list by element ID:
```erlang
7> kvlists:set_path([transactions, ratings, {type, positive}, percent], 1000, List).
[{transactions,[{period,<<"3 months">>},
                {total,3659},
                {completed,3381},
                {canceled,278},
                {ratings,[[{type,positive},{percent,1000}],
                          [{type,negative},{percent,0}],
                          [{type,neutral},{percent,1}]]}]}]
```
Replace an element by element ID:
```erlang
8> kvlists:set_path([transactions, ratings, {type, negative}], [{type, unknown}, {value, 100}], List).
[{transactions,[{period,<<"3 months">>},
                {total,3659},
                {completed,3381},
                {canceled,278},
                {ratings,[[{type,positive},{percent,99}],
                          [{type,unknown},{value,100}],
                          [{type,neutral},{percent,1}]]}]}]
```
Add a new element by element ID:
```erlang
9> kvlists:set_path([transactions, ratings, {type, unknown}, percent], 50, List).
[{transactions,[{period,<<"3 months">>},
                {total,3659},
                {completed,3381},
                {canceled,278},
                {ratings,[[{type,positive},{percent,99}],
                          [{type,negative},{percent,0}],
                          [{type,neutral},{percent,1}],
                          [{type,unknown},{percent,50}]]}]}]
```

-------------

### set_value/3
Adds a property to the `List` with the corresponding `Key` and `Value`.

#### Specification
```erlang
-spec set_value(Key :: path_key(), Value :: value(), List :: kvlist()) -> kvlist().
```
#### Example
```erlang
1> List = [{abc, 123}, {def, 456}, {ghi, 789}].
2> kvlists:set_value(def, 200, List).
[{abc, 123}, {def, 200}, {ghi, 789}]
```

-------------

### set_values/2
Sets each `Key` in `List` to its corresponding `Value`.

#### Specification
```erlang
-spec set_values([{Key :: path_key(), Value :: value()}], List :: kvlist()) ->
                        NewList :: kvlist().
```
#### Example
```erlang
1> List = [{abc, 123}, {def, 456}, {ghi, 789}].
2> kvlists:set_values([{abc, 100}, {jkl, <<"JKL">>}], List).
[{abc, 100}, {def, 456}, {ghi, 789}, {jkl, <<"JKL">>}]
```

-------------

### with/2
Return a `NewList` where the `Key` of each element is present in the list
of `Keys`.

#### Specification
```erlang
-spec with(Keys :: [key()], List :: kvlist()) -> NewList :: kvlist().
```
#### Example
```erlang
1> List = [{abc, 123}, {def, 456}, {ghi, 789}].
2> kvlists:with([abc, ghi], List).
[{abc, 123}, {ghi, 789}]
```

-------------

### without/2
Return a `NewList` where the `Key` of each element is not present in the list
of `Keys`.

#### Specification
```erlang
-spec without(Keys :: [key()], List :: kvlist()) -> NewList :: kvlist().
```
#### Example
```erlang
1> List = [{abc, 123}, {def, 456}, {ghi, 789}].
2> kvlists:without([abc, ghi], List).
[{def, 456}]
```
