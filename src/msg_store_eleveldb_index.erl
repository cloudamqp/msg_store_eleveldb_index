-module(msg_store_eleveldb_index).

-behaviour(rabbit_msg_store_index).

-rabbit_boot_step({msg_store_eleveldb_index,
                   [{description, "eLevelDB backend for rabbit_msg_store_index"},
                    {mfa, {application, set_env,
                           [rabbit, msg_store_index_module, ?MODULE]}},
                    {enables, recovery}]}).

-export([new/1, recover/1,
         lookup/2, insert/2, update/2, update_fields/3, delete/2,
         delete_object/2, delete_by_file/2, terminate/1]).

-include_lib("rabbit_common/include/rabbit_msg_store_index.hrl").
-include_lib("eleveldb/include/eleveldb.hrl").

-define(ELEVELDB_DIR, "eleveldb_data").

new(Dir) ->
  Path = get_path(Dir),
  {ok, Ref} = init(Path),
  Ref.

recover(Dir) ->
  Path = get_path(Dir),
  {ok, Ref} = init(Path),
  {ok, Ref}.

get_path(Dir) ->
  filename:join(Dir, ?ELEVELDB_DIR).

init(Dir) ->
    {ok, Ref} = eleveldb:open(Dir,
                              [{create_if_missing, true}]),
    Ref.

%% Key is MsgId which is binary already
lookup(Key, Eleveldb) ->
  case eleveldb:get(Eleveldb, Key) of
    {ok, Value} -> #msg_location{} = binary_to_term(Value);
    _ -> not_found
  end.

insert(Obj = #msg_location{ msg_id = MsgId }, Eleveldb) ->
  ok = eleveldb:put(Eleveldb, MsgId, term_to_binary(Obj)),
  ok.

update(Obj = #msg_location{ msg_id = MsgId }, Eleveldb) ->
  ok = eleveldb:put(Eleveldb, MsgId, term_to_binary(Obj)),
  ok.

update_fun({Position, NewValue}, ObjAcc) ->
  setelement(Position, ObjAcc, NewValue).

update_fields(Key, Updates, Eleveldb) ->
  case eleveldb:get(Eleveldb, Key) of
    {ok, Value} ->
      Obj = #msg_location{} = binary_to_term(Value),
      NewObj =
      case is_list(Updates) of
        true -> lists:foldl(fun update_fun/2, Obj, Updates);
        false -> update_fun(Updates, Obj)
      end,
      ok = eleveldb:put(Eleveldb, Key, term_to_binary(NewObj)),
      ok;
    _ -> not_found
  end,
  ok.

delete(Key, Eleveldb) ->
  ok = eleveldb:delete(Eleveldb, Key),
  ok.

delete_object(Obj = #msg_location{ msg_id = MsgId }, Eleveldb) ->
  case eleveldb:get(Eleveldb, MsgId) of
    {ok, Value} ->
      case Obj =:= binary_to_term(Value) of
        true ->
          ok = eleveldb:delete(Eleveldb, MsgId),
          ok;
        _ ->
          not_found
      end;
    _ -> not_found
  end.

delete_by_file(File, Eleveldb) ->
  ok = eleveldb:fold(Eleveldb,
                     fun(Key, Obj, Acc) ->
                         case (binary_to_term(Obj))#msg_location.file of
                           File ->
                             eleveldb:delete(Eleveldb, Key),
                             Acc;
                           _ ->
                             Acc
                         end
                     end, ok),
  ok.

terminate(Eleveldb) ->
  ok = eleveldb:close(Eleveldb),
  ok.
