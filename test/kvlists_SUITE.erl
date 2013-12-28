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
       t_delete,
       t_get_value,
       t_get_path,
       t_member,
       t_set_value
      ]}
     ].

all() ->
    [{group, kvlists}].


%% Test cases

t_delete(_Config) ->
    [] = kvlists:delete(abc, []),
    [] = kvlists:delete(<<"abc">>, []),
    AtomList = [{abc, 123}, {def, 456}, {ghi, 789}],
    [{def, 456}, {ghi, 789}] = kvlists:delete(abc, AtomList),
    [{abc, 123}, {ghi, 789}] = kvlists:delete(def, AtomList),
    [{abc, 123}, {def, 456}] = kvlists:delete(ghi, AtomList),
    BinList = [{<<"abc">>, 123}, {<<"def">>, 456}, {<<"ghi">>, 789}],
    [{<<"def">>, 456}, {<<"ghi">>, 789}] = kvlists:delete(<<"abc">>, BinList),
    [{<<"abc">>, 123}, {<<"ghi">>, 789}] = kvlists:delete(<<"def">>, BinList),
    [{<<"abc">>, 123}, {<<"def">>, 456}] = kvlists:delete(<<"ghi">>, BinList).


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
    25679280          = kvlists:get_path(id, AtomList),
    <<"MIPCSTORE">>   = kvlists:get_path([nickname], AtomList),
    <<"Recoleta">>    = kvlists:get_path([address, city], AtomList),
    [99, 0, 1]        = kvlists:get_path([seller_reputation, transactions, ratings, percent], AtomList),
    99                = kvlists:get_path([seller_reputation, transactions, ratings, 1, percent], AtomList),
    negative          = kvlists:get_path([seller_reputation, transactions, ratings, 2, type], AtomList),
    1                 = kvlists:get_path([seller_reputation, transactions, ratings, 3, percent], AtomList),
    <<"active">>      = kvlists:get_path([status, site_status], AtomList),
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
    [99, 0, 1]        = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, <<"percent">>], BinList),
    99                = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 1, <<"percent">>], BinList),
    negative          = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 2, <<"type">>], BinList),
    1                 = kvlists:get_path([<<"seller_reputation">>, <<"transactions">>, <<"ratings">>, 3, <<"percent">>], BinList),
    <<"active">>      = kvlists:get_path([<<"status">>, <<"site_status">>], BinList).


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
