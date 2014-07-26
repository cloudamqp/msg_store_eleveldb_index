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

-include_lib("rabbit_common/include/rabbit_msg_store.hrl").

-define(ELEVELDB_DIR, "eleveldb_msg_index").

new(Dir) ->
  Path = get_path(Dir),
  case eleveldb:open(Path, [{create_if_missing, true},
                            {error_if_exists, false},
                            {verify_compactions, true},
                            {use_bloomfilter, true}]) of
    {ok, Ref} -> Ref;
    {error, Reason} ->
      lager:error("Leveldb backend error ~s\n", [Reason]),
      error
  end.

recover(Dir) ->
  Path = get_path(Dir),
  eleveldb:open(Path, [{create_if_missing, false}]).

get_path(Dir) ->
  filename:join(Dir, ?ELEVELDB_DIR).

%% Key is MsgId which is binary already
lookup(Key, Ref) ->
  case eleveldb:get(Ref, Key, []) of
    {ok, Value} -> #msg_location{} = binary_to_term(Value);
    _ -> not_found
  end.

insert(Obj = #msg_location{ msg_id = MsgId }, Ref) ->
  ok = eleveldb:put(Ref, MsgId, term_to_binary(Obj), []),
  ok.

update(Obj = #msg_location{ msg_id = MsgId }, Ref) ->
  ok = eleveldb:put(Ref, MsgId, term_to_binary(Obj), []),
  ok.

update_fun({Position, NewValue}, ObjAcc) ->
  setelement(Position, ObjAcc, NewValue).

update_fields(Key, Updates, Ref) ->
  case eleveldb:get(Ref, Key, []) of
    {ok, Value} ->
      Obj = #msg_location{} = binary_to_term(Value),
      NewObj =
      case is_list(Updates) of
        true -> lists:foldl(fun update_fun/2, Obj, Updates);
        false -> update_fun(Updates, Obj)
      end,
      ok = eleveldb:put(Ref, Key, term_to_binary(NewObj), []),
      ok;
    _ -> not_found
  end,
  ok.

delete(Key, Ref) ->
  ok = eleveldb:delete(Ref, Key, []),
  ok.

delete_object(Obj = #msg_location{ msg_id = MsgId }, Ref) ->
  case eleveldb:get(Ref, MsgId, []) of
    {ok, Value} ->
      case Obj =:= binary_to_term(Value) of
        true ->
          ok = eleveldb:delete(Ref, MsgId, []),
          ok;
        _ ->
          not_found
      end;
    _ -> not_found
  end.

delete_by_file(File, Ref) ->
  eleveldb:fold(Ref,
                fun({Key, Obj}, Acc) ->
                    case (binary_to_term(Obj))#msg_location.file of
                      File ->
                        eleveldb:delete(Ref, Key, []),
                        Acc;
                      _ ->
                        Acc
                    end
                end, [], []),
  ok.

terminate(Ref) ->
  eleveldb:close(Ref).
