-module(homework6).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([create/1, insert/3, insert/4, lookup/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).

-record(homework6_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #homework6_state{}} | {ok, State :: #homework6_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #homework6_state{}}.

handle_call({create, TableName}, _From, State) ->
  ets:new(TableName, [named_table, public]),
  erlang:send_after(60000, self(), {delete, TableName}),
  {reply, ok, State};
handle_call({insert, TableName, Value}, _From, State) ->
  ets:insert(TableName, Value),
  {reply, ok, State};
handle_call({lookup, TableName, Key}, _From, State) ->
  Res =
    case ets:lookup(TableName, Key) of
      [{Key, Value}] -> Value;
      [{Key, Value, Timer}] -> check(TableName, Key, Value, Timer);
      _ -> undefined
    end,
  {reply, Res, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({delete, TableName}, State) ->
  delete_obsolete(TableName),
  erlang:send_after(60000, self(), {delete, TableName}),
  {noreply, State};
handle_info(_, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create(TableName) ->
  gen_server:call(?MODULE, {create, TableName}).

insert(TableName, Key, Value) ->
  gen_server:call(?MODULE, {insert, TableName, {Key, Value}}).
insert(TableName, Key, Value, Timer) ->
  NewTimer = erlang:system_time() + 1000000000 * Timer,
  gen_server:call(?MODULE, {insert, TableName, {Key, Value, NewTimer}}).

lookup(TableName, Key) ->
  gen_server:call(?MODULE, {lookup, TableName, Key}).

check(TableName, Key, Value, Timer) ->
  case Timer >= erlang:system_time() of
    true -> Value;
    _ -> ets:delete(TableName, Key),
      undefined
  end.

delete_obsolete(TableName) ->
  case ets:first(TableName) of
    '$end_of_table' -> ok;
    Key -> delete_obsolete(TableName, Key)
  end.

delete_obsolete(_, '$end_of_table') ->
  ok;
delete_obsolete(TableName, Key) ->
  NewKey = ets:next(TableName, Key),
  case ets:lookup(TableName, Key) of
    [{Key, Value, Timer}] -> check(TableName, Key, Value, Timer);
    _ -> ok
  end,
  delete_obsolete(TableName, NewKey).