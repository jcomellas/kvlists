%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2013 Juan Jose Comellas
%%% @doc Tests for lists of key-value pairs
%%% @end
%%%-------------------------------------------------------------------
-module(kvlists_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").


suite() ->
    [{ct_hooks, [cth_surefire]}, {timetrap, {seconds, 120}}].

init_per_suite(Config) ->
    Config.


end_per_suite(_Config) ->
    ok.


init_per_group(_GroupName, Config) ->
    Config.


end_per_group(_GroupName, _Config) ->
    ok.


init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) ->
    ok.


groups() ->
    [{kvlists, [parallel],
      [
       t_delete_path,
       t_delete_value,
       t_get_path,
       t_get_value,
       t_get_values,
       t_member,
       t_set_path,
       t_set_value,
       t_set_values
      ]}
     ].

all() ->
    [{group, kvlists}].


%% Test cases

t_delete_path(_Config) ->
    Check = fun (Path, List) ->
                    false = (kvlists:get_path(Path, List) =:= []),
                    [] = kvlists:get_path(Path, kvlists:delete_path(Path, List))
            end,
    %% Kvlist with atoms as keys
    AtomList = [{id, 25679280},
                {nickname, <<"MIPCSTORE">>},
                {registration_date, {{2010, 3, 15}, {13, 17, 41}}},
                {country_id, <<"AR">>},
                {address, [{state, <<"AR-C">>}, {city, <<"Recoleta">>}]},
                {user_type, <<"normal">>},
                {tags, [<<"normal">>]},
                {logo, null},
                {points, 22049},
                {site_id, <<"MLA">>},
                {permalink, <<"http://perfil.mercadolibre.com.ar/MIPCSTORE">>},
                {seller_reputation,
                 [{level_id, <<"5_green">>},
                  {power_seller_status, <<"platinum">>},
                  {transactions,
                   [{period, <<"3 months">>},
                    {total, 3659},
                    {completed, 3381},
                    {canceled, 278},
                    {ratings, [[{type, positive}, {percent, 99}],
                               [{type, negative}, {percent, 0}],
                               [{type, neutral}, {percent, 1}]]}]}]},
                {status,[{site_status,<<"active">>}]}],
    [] = kvlists:delete_path([], AtomList),
    Check(id, AtomList),
    Check([nickname], AtomList),
    Check([address, city], AtomList),
    Check([seller_reputation, transactions, ratings, 1, percent], AtomList),
    Check([seller_reputation, transactions, ratings, 2, type], AtomList),
    Check([seller_reputation, transactions, ratings, 3, percent], AtomList),
    Check([status, site_status], AtomList),
    Check([seller_reputation, transactions, ratings, percent], AtomList),
    Check([seller_reputation, transactions, ratings, {type, negative}], AtomList),
    Check([seller_reputation, transactions, ratings, {type, negative}, percent], AtomList),
    %% Kvlist with binaries as keys
    BinList = [{<<"id">>, 25679280},
               {<<"nickname">>, <<"MIPCSTORE">>},
               {<<"registration_date">>, {{2010, 3, 15}, {13, 17, 41}}},
               {<<"country_id">>, <<"AR">>},
               {<<"address">>, [{<<"state">>, <<"AR-C">>}, {<<"city">>, <<"Recoleta">>}]},
               {<<"user_type">>, <<"normal">>},
               {<<"tags">>, [<<"normal">>]},
               {<<"logo">>, null},
               {<<"points">>, 22049},
               {<<"site_id">>, <<"MLA">>},
               {<<"permalink">>, <<"http://perfil.mercadolibre.com.ar/MIPCSTORE">>},
               {<<"seller_reputation">>,
                [{<<"level_id">>, <<"5_green">>},
                 {<<"power_seller_status">>, <<"platinum">>},
                 {<<"transactions">>,
                  [{<<"period">>, <<"3 months">>},
                   {<<"total">>, 3659},
                   {<<"completed">>, 3381},
                   {<<"canceled">>, 278},
                   {<<"ratings">>, [[{<<"type">>, positive}, {<<"percent">>, 99}],
                              [{<<"type">>, negative}, {<<"percent">>, 0}],
                              [{<<"type">>, neutral}, {<<"percent">>, 1}]]}]}]},
               {<<"status">>,[{<<"site_status">>,<<"active">>}]}],
    Check(<<"id">>, BinList),
    Check([<<"nickname">>], BinList),
    Check([<<"address">>, <<"city">>], BinList),
    Check([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 1, <<"percent">>], BinList),
    Check([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 2, <<"type">>], BinList),
    Check([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 3, <<"percent">>], BinList),
    Check([<<"status">>, <<"site_status">>], BinList),
    Check([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, <<"percent">>], BinList),
    Check([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, {<<"type">>, negative}], BinList),
    Check([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, {<<"type">>, negative}, <<"percent">>], BinList).


t_delete_value(_Config) ->
    [] = kvlists:delete_value(abc, []),
    [] = kvlists:delete_value(<<"abc">>, []),
    AtomList = [{abc, 123}, {def, 456}, {ghi, 789}],
    [{def, 456}, {ghi, 789}] = kvlists:delete_value(abc, AtomList),
    [{abc, 123}, {ghi, 789}] = kvlists:delete_value(def, AtomList),
    [{abc, 123}, {def, 456}] = kvlists:delete_value(ghi, AtomList),
    BinList = [{<<"abc">>, 123}, {<<"def">>, 456}, {<<"ghi">>, 789}],
    [{<<"def">>, 456}, {<<"ghi">>, 789}] = kvlists:delete_value(<<"abc">>, BinList),
    [{<<"abc">>, 123}, {<<"ghi">>, 789}] = kvlists:delete_value(<<"def">>, BinList),
    [{<<"abc">>, 123}, {<<"def">>, 456}] = kvlists:delete_value(<<"ghi">>, BinList).


t_get_path(_Config) ->
    %% Kvlist with atoms as keys
    AtomList = [{id, 25679280},
                {nickname, <<"MIPCSTORE">>},
                {registration_date, {{2010, 3, 15}, {13, 17, 41}}},
                {country_id, <<"AR">>},
                {address, [{state, <<"AR-C">>}, {city, <<"Recoleta">>}]},
                {user_type, <<"normal">>},
                {tags, [<<"normal">>]},
                {logo, null},
                {points, 22049},
                {site_id, <<"MLA">>},
                {permalink, <<"http://perfil.mercadolibre.com.ar/MIPCSTORE">>},
                {seller_reputation,
                 [{level_id, <<"5_green">>},
                  {power_seller_status, <<"platinum">>},
                  {transactions,
                   [{period, <<"3 months">>},
                    {total, 3659},
                    {completed, 3381},
                    {canceled, 278},
                    {ratings, [[{type, positive}, {percent, 99}],
                               [{type, negative}, {percent, 0}],
                               [{type, neutral}, {percent, 1}]]}]}]},
                {status,[{site_status,<<"active">>}]}],
    [a, b, c]         = kvlists:get_path([], [a, b, c]),
    25679280          = kvlists:get_path(id, AtomList),
    <<"MIPCSTORE">>   = kvlists:get_path([nickname], AtomList),
    <<"Recoleta">>    = kvlists:get_path([address, city], AtomList),
    [99, 0, 1]        = kvlists:get_path([seller_reputation, transactions,
                                          ratings, percent], AtomList),
    99                = kvlists:get_path([seller_reputation, transactions,
                                          ratings, 1, percent], AtomList),
    negative          = kvlists:get_path([seller_reputation, transactions,
                                          ratings, 2, type], AtomList),
    1                 = kvlists:get_path([seller_reputation, transactions,
                                          ratings, 3, percent], AtomList),
    <<"active">>      = kvlists:get_path([status, site_status], AtomList),
    0                 = kvlists:get_path([seller_reputation, transactions,
                                          ratings, {type, negative}, percent], AtomList),
    []                = kvlists:get_path([seller_reputation, transactions,
                                          ratings, 1, 1, 1], AtomList),
    %% Kvlist with binaries as keys
    BinList = [{<<"id">>, 25679280},
               {<<"nickname">>, <<"MIPCSTORE">>},
               {<<"registration_date">>, {{2010, 3, 15}, {13, 17, 41}}},
               {<<"country_id">>, <<"AR">>},
               {<<"address">>, [{<<"state">>, <<"AR-C">>}, {<<"city">>, <<"Recoleta">>}]},
               {<<"user_type">>, <<"normal">>},
               {<<"tags">>, [<<"normal">>]},
               {<<"logo">>, null},
               {<<"points">>, 22049},
               {<<"site_id">>, <<"MLA">>},
               {<<"permalink">>, <<"http://perfil.mercadolibre.com.ar/MIPCSTORE">>},
               {<<"seller_reputation">>,
                [{<<"level_id">>, <<"5_green">>},
                 {<<"power_seller_status">>, <<"platinum">>},
                 {<<"transactions">>,
                  [{<<"period">>, <<"3 months">>},
                   {<<"total">>, 3659},
                   {<<"completed">>, 3381},
                   {<<"canceled">>, 278},
                   {<<"ratings">>, [[{<<"type">>, positive}, {<<"percent">>, 99}],
                              [{<<"type">>, negative}, {<<"percent">>, 0}],
                              [{<<"type">>, neutral}, {<<"percent">>, 1}]]}]}]},
               {<<"status">>,[{<<"site_status">>,<<"active">>}]}],
    25679280          = kvlists:get_path(<<"id">>, BinList),
    <<"MIPCSTORE">>   = kvlists:get_path([<<"nickname">>], BinList),
    <<"Recoleta">>    = kvlists:get_path([<<"address">>, <<"city">>], BinList),
    [99, 0, 1]        = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>,
                                          <<"ratings">>, <<"percent">>], BinList),
    99                = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>,
                                          <<"ratings">>, 1, <<"percent">>], BinList),
    negative          = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>,
                                          <<"ratings">>, 2, <<"type">>], BinList),
    1                 = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>,
                                          <<"ratings">>, 3, <<"percent">>], BinList),
    <<"active">>      = kvlists:get_path([<<"status">>, <<"site_status">>], BinList),
    0                 = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>,
                                          <<"ratings">>, {<<"type">>, negative}, <<"percent">>], BinList),
    []                = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>,
                                          <<"ratings">>, 1, 1, 1], BinList).


t_get_value(_Config) ->
    undefined = kvlists:get_value(abc, []),
    undefined = kvlists:get_value(<<"abc">>, []),
    AtomList = [{abc, 123}, {def, 456}, {ghi, 789}],
    123 = kvlists:get_value(abc, AtomList),
    456 = kvlists:get_value(def, AtomList),
    789 = kvlists:get_value(ghi, AtomList),
    BinList = [{<<"abc">>, "123"}, {<<"def">>, "456"}, {<<"ghi">>, "789"}],
    "123" = kvlists:get_value(<<"abc">>, BinList),
    "456" = kvlists:get_value(<<"def">>, BinList),
    "789" = kvlists:get_value(<<"ghi">>, BinList).


t_get_values(_Config) ->
    AtomList = [{abc, 123}, {def, 456}, {ghi, 789}],
    [] = kvlists:get_values([], AtomList),
    [undefined] = kvlists:get_values([jkl], AtomList),
    [100] = kvlists:get_values([{jkl, 100}], AtomList),
    [123] = kvlists:get_values([abc], AtomList),
    [456] = kvlists:get_values([def], AtomList),
    [789] = kvlists:get_values([ghi], AtomList),
    [123, 456, 789, 200] = kvlists:get_values([abc, {def, 100}, ghi, {jkl, 200}], AtomList),
    BinList = [{<<"abc">>, "123"}, {<<"def">>, "456"}, {<<"ghi">>, "789"}],
    [] = kvlists:get_values([], BinList),
    [undefined] = kvlists:get_values([<<"jkl">>], BinList),
    ["100"] = kvlists:get_values([{<<"jkl">>, "100"}], BinList),
    ["123"] = kvlists:get_values([<<"abc">>], BinList),
    ["456"] = kvlists:get_values([<<"def">>], BinList),
    ["789"] = kvlists:get_values([<<"ghi">>], BinList),
    ["123", "456", "789", "200"] = kvlists:get_values([<<"abc">>, {<<"def">>, "100"},
                                                       <<"ghi">>, {<<"jkl">>, "200"}], BinList).


t_member(_Config) ->
    false = kvlists:member(abc, []),
    false = kvlists:member(<<"abc">>, []),
    false = kvlists:member(abc, [{<<"abc">>, 123}]),
    false = kvlists:member(<<"abc">>, [{abc, 123}]),
    true = kvlists:member(abc, [{abc, 123}]),
    true = kvlists:member(<<"abc">>, [{<<"abc">>, 123}]),
    AtomList = [{abc, 123}, {def, 456}, {ghi, 789}],
    true = kvlists:member(abc, AtomList),
    true = kvlists:member(def, AtomList),
    true = kvlists:member(ghi, AtomList),
    false = kvlists:member(<<"abc">>, AtomList),
    false = kvlists:member(jkl, AtomList),
    BinList = [{<<"abc">>, 123}, {<<"def">>, 456}, {<<"ghi">>, 789}],
    true = kvlists:member(<<"abc">>, BinList),
    true = kvlists:member(<<"def">>, BinList),
    true = kvlists:member(<<"ghi">>, BinList),
    false = kvlists:member(abc, BinList),
    false = kvlists:member(<<"jkl">>, BinList).


t_set_path(_Config) ->
    Check = fun (Path, Value, List) ->
                    Value = kvlists:get_path(Path, kvlists:set_path(Path, Value, List))
            end,
    %% Kvlist with atoms as keys
    AtomList = [{id, 25679280},
                {nickname, <<"MIPCSTORE">>},
                {registration_date, {{2010, 3, 15}, {13, 17, 41}}},
                {country_id, <<"AR">>},
                {address, [{state, <<"AR-C">>}, {city, <<"Recoleta">>}]},
                {user_type, <<"normal">>},
                {tags, [<<"normal">>]},
                {logo, null},
                {points, 22049},
                {site_id, <<"MLA">>},
                {permalink, <<"http://perfil.mercadolibre.com.ar/MIPCSTORE">>},
                {seller_reputation,
                 [{level_id, <<"5_green">>},
                  {power_seller_status, <<"platinum">>},
                  {transactions,
                   [{period, <<"3 months">>},
                    {total, 3659},
                    {completed, 3381},
                    {canceled, 278},
                    {ratings, [[{type, positive}, {percent, 99}],
                               [{type, negative}, {percent, 0}],
                               [{type, neutral}, {percent, 1}]]}]}]},
                {status,[{site_status,<<"active">>}]}],
    100 = kvlists:set_path([], 100, AtomList),
    Check(id, 1000, AtomList),
    Check([nickname], <<"MYNAME">>, AtomList),
    Check([address, city], <<"Other City">>, AtomList),
    Check([seller_reputation, transactions, ratings, 1, percent], 199, AtomList),
    Check([seller_reputation, transactions, ratings, 2, type], unknown, AtomList),
    Check([seller_reputation, transactions, ratings, 3, percent], 11, AtomList),
    Check([status, site_status], <<"inactive">>, AtomList),
    Check([seller_reputation, transactions, ratings, {type, negative}, percent], 20, AtomList),
    %% Test setting multiple values at the same time
    AtomList1 = kvlists:set_path([seller_reputation, transactions, ratings, percent], [10, 20, 30, 40], AtomList),
    10 = kvlists:get_path([seller_reputation, transactions, ratings, 1, percent], AtomList1),
    20 = kvlists:get_path([seller_reputation, transactions, ratings, 2, percent], AtomList1),
    30 = kvlists:get_path([seller_reputation, transactions, ratings, 3, percent], AtomList1),
    40 = kvlists:get_path([seller_reputation, transactions, ratings, 4, percent], AtomList1),
    AtomList2 = kvlists:set_path([seller_reputation, transactions, ratings, percent], 123, AtomList),
    123 = kvlists:get_path([seller_reputation, transactions, ratings, 1, percent], AtomList2),
    123 = kvlists:get_path([seller_reputation, transactions, ratings, 2, percent], AtomList2),
    123 = kvlists:get_path([seller_reputation, transactions, ratings, 3, percent], AtomList2),
    []  = kvlists:get_path([seller_reputation, transactions, ratings, 4, percent], AtomList2),
    %% Kvlist with binaries as keys
    BinList = [{<<"id">>, 25679280},
               {<<"nickname">>, <<"MIPCSTORE">>},
               {<<"registration_date">>, {{2010, 3, 15}, {13, 17, 41}}},
               {<<"country_id">>, <<"AR">>},
               {<<"address">>, [{<<"state">>, <<"AR-C">>}, {<<"city">>, <<"Recoleta">>}]},
               {<<"user_type">>, <<"normal">>},
               {<<"tags">>, [<<"normal">>]},
               {<<"logo">>, null},
               {<<"points">>, 22049},
               {<<"site_id">>, <<"MLA">>},
               {<<"permalink">>, <<"http://perfil.mercadolibre.com.ar/MIPCSTORE">>},
               {<<"seller_reputation">>,
                [{<<"level_id">>, <<"5_green">>},
                 {<<"power_seller_status">>, <<"platinum">>},
                 {<<"transactions">>,
                  [{<<"period">>, <<"3 months">>},
                   {<<"total">>, 3659},
                   {<<"completed">>, 3381},
                   {<<"canceled">>, 278},
                   {<<"ratings">>, [[{<<"type">>, positive}, {<<"percent">>, 99}],
                              [{<<"type">>, negative}, {<<"percent">>, 0}],
                              [{<<"type">>, neutral}, {<<"percent">>, 1}]]}]}]},
               {<<"status">>,[{<<"site_status">>,<<"active">>}]}],
    Check(<<"id">>, 25679280, BinList),
    Check([<<"nickname">>], <<"MYNAME">>, BinList),
    Check([<<"address">>, <<"city">>], <<"Other City">>, BinList),
    Check([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 1, <<"percent">>], 199, BinList),
    Check([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 2, <<"type">>], unknown, BinList),
    Check([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 3, <<"percent">>], 11, BinList),
    Check([<<"status">>, <<"site_status">>], <<"active">>, BinList),
    %% Test setting multiple values at the same time
    BinList1 = kvlists:set_path([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, <<"percent">>],
                                 [10, 20, 30, 40], BinList),
    10 = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 1, <<"percent">>], BinList1),
    20 = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 2, <<"percent">>], BinList1),
    30 = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 3, <<"percent">>], BinList1),
    40 = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 4, <<"percent">>], BinList1),
    BinList2 = kvlists:set_path([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, <<"percent">>], 123, BinList),
    123 = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 1, <<"percent">>], BinList2),
    123 = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 2, <<"percent">>], BinList2),
    123 = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 3, <<"percent">>], BinList2),
    []  = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 4, <<"percent">>], BinList2).


t_set_value(_Config) ->
    [{abc, 123}] = kvlists:set_value(abc, 123, []),
    [{<<"abc">>, 123}] = kvlists:set_value(<<"abc">>, 123, []),
    AtomList = [{abc, 123}, {def, 456}, {ghi, 789}],
    [{abc, 100}, {def, 456}, {ghi, 789}] = kvlists:set_value(abc, 100, AtomList),
    [{abc, 123}, {def, 200}, {ghi, 789}] = kvlists:set_value(def, 200, AtomList),
    [{abc, 123}, {def, 456}, {ghi, 300}] = kvlists:set_value(ghi, 300, AtomList),
    [{abc, 123}, {def, 456}, {ghi, 789}, {jkl, <<"JKL">>}] = kvlists:set_value(jkl, <<"JKL">>, AtomList),
    BinList = [{<<"abc">>, 123}, {<<"def">>, 456}, {<<"ghi">>, 789}],
    [{<<"abc">>, 100}, {<<"def">>, 456}, {<<"ghi">>, 789}] = kvlists:set_value(<<"abc">>, 100, BinList),
    [{<<"abc">>, 123}, {<<"def">>, 200}, {<<"ghi">>, 789}] = kvlists:set_value(<<"def">>, 200, BinList),
    [{<<"abc">>, 123}, {<<"def">>, 456}, {<<"ghi">>, 300}] = kvlists:set_value(<<"ghi">>, 300, BinList),
    [{<<"abc">>, 123}, {<<"def">>, 456}, {<<"ghi">>, 789}, {<<"jkl">>, jkl}] = kvlists:set_value(<<"jkl">>, jkl, BinList).


t_set_values(_Config) ->
    [{abc, 123}] = kvlists:set_values([], [{abc, 123}]),
    [{abc, 100}] = kvlists:set_values([{abc, 100}], [{abc, 123}]),
    AtomList = [{abc, 123}, {def, 456}, {ghi, 789}],
    [{abc, 100}, {def, 456}, {ghi, 789}] = kvlists:set_values([{abc, 100}], AtomList),
    [{abc, 123}, {def, 200}, {ghi, 789}] = kvlists:set_values([{def, 200}], AtomList),
    [{abc, 123}, {def, 456}, {ghi, 300}] = kvlists:set_values([{ghi, 300}], AtomList),
    [{abc, 100}, {def, 456}, {ghi, 789}, {jkl, <<"JKL">>}] = kvlists:set_values([{abc, 100}, {jkl, <<"JKL">>}], AtomList),
    BinList = [{<<"abc">>, 123}, {<<"def">>, 456}, {<<"ghi">>, 789}],
    [{<<"abc">>, 100}, {<<"def">>, 456}, {<<"ghi">>, 789}] = kvlists:set_values([{<<"abc">>, 100}], BinList),
    [{<<"abc">>, 123}, {<<"def">>, 200}, {<<"ghi">>, 789}] = kvlists:set_values([{<<"def">>, 200}], BinList),
    [{<<"abc">>, 123}, {<<"def">>, 456}, {<<"ghi">>, 300}] = kvlists:set_values([{<<"ghi">>, 300}], BinList),
    [{<<"abc">>, 100}, {<<"def">>, 456}, {<<"ghi">>, 789}, {<<"jkl">>, 'JKL'}] = kvlists:set_values([{<<"abc">>, 100}, {<<"jkl">>, 'JKL'}], BinList).
