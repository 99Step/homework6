-module(homework6_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
  [homework6_tests].

init_per_suite(Config) ->
  ok = application:start(homework6),
  Config.

homework6_tests(_Config) ->
  ok = homework6:create(tab),
  ok = homework6:insert(tab, 1, <<"1">>),
  ok = homework6:insert(tab, 2, <<"2">>),
  ok = homework6:insert(tab, 3, <<"3">>, 40),
  ok = homework6:insert(tab, 4, <<"4">>, 50),
  <<"1">> = homework6:lookup(tab, 1),
  <<"3">> = homework6:lookup(tab, 3),
  <<"4">> = homework6:lookup(tab, 4),
  timer:sleep(45000),
  <<"1">> = homework6:lookup(tab, 1),
  undefined = homework6:lookup(tab, 3),
  [{4, <<"4">>, _Time}] = ets:lookup(tab, 4),
  timer:sleep(20000),
  <<"1">> = homework6:lookup(tab, 1),
  [] = ets:lookup(tab, 4).

end_per_suite(Config) ->
  ok = application:stop(homework6),
  Config.