-module(msg_store_eleveldb_index).
-include_lib("rabbit_common/include/rabbit_msg_store.hrl").
-behaviour(rabbit_msg_store_index).

-rabbit_boot_step({msg_store_eleveldb_index,
                   [{description, "eLevelDB backed message index storage"},
                    {mfa, {application, set_env,
                           [rabbit, msg_store_index_module, ?MODULE]}},
                    {enables, recovery}]}).

-export([new/1, recover/1,
         lookup/2, insert/2, update/2, update_fields/3, delete/2,
         delete_object/2, delete_by_file/2, terminate/1]).

-define(ELEVELDB_DIR, "eleveldb_msg_index").

new(Dir) ->
  Path = get_path(Dir),
  Store = filename:basename(Dir),
  ok = del_dir(Path),
  rabbit_log:info("~s: creating new eLevelDB~n", [Store]),
  case eleveldb:open(Path, [{create_if_missing, true},
                            {error_if_exists, true},
                            {paranoid_checks, false},
                            {compression, false},
                            {verify_compactions, false},
                            {use_bloomfilter, true}]) of
    {ok, Ref} -> Ref;
    {error, Reason} ->
      rabbit_log:error("~s: error recovering eLevelDB~n~w~n", [Store, Reason]),
      {error, Reason}
  end.

recover(Dir) ->
  Path = get_path(Dir),
  Store = filename:basename(Dir),
  rabbit_log:info("~s: recovering eLevelDB~n", [Store]),
  case eleveldb:open(Path, [{create_if_missing, false},
                            {error_if_exists, false},
                            {paranoid_checks, false},
                            {compression, false},
                            {verify_compactions, true},
                            {use_bloomfilter, true}]) of
    {ok, Ref} -> {ok, Ref};
    {error, Reason} -> 
      rabbit_log:error("~s: error recovering eLevelDB~n~w~n", [Store, Reason]),
      {error, Reason}
  end.


get_path(Dir) ->
  filename:join(Dir, ?ELEVELDB_DIR).

del_dir(Dir) ->
  case file:del_dir(Dir) of
    ok -> ok;
    {error, enoent} -> ok;
    {error, eexist} ->
      {ok, FilesInDir} = file:list_dir_all(Dir),
      {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
                                         Path = filename:join(Dir, F),
                                         case filelib:is_dir(Path) of
                                           true -> {Fs, [Path | Ds]};
                                           false -> {[Path | Fs], Ds}
                                         end
                                     end, {[],[Dir]}, FilesInDir),
      [ok = file:delete(F) || F <- Files],
      [ok = file:del_dir(D) || D <- Dirs],
      ok;
    {error, Reason} -> {error, Reason}
  end.

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
      NewObj = case is_list(Updates) of
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
  DeleteKeys = eleveldb:fold(Ref,
                             fun({Key, Obj}, Acc) ->
                                 case (binary_to_term(Obj))#msg_location.file of
                                   File -> [{delete, Key} | Acc];
                                   _ -> Acc
                                 end
                             end, [], []),
  ok = eleveldb:write(Ref, DeleteKeys, []),
  ok.

terminate(Ref) ->
  rabbit_log:info("Terminating eLevelDB~n", []),
  ok = eleveldb:close(Ref).

