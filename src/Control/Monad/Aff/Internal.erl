-module(control_monad_aff_internal@foreign).
-export(['_makeVar'/1, '_takeVar'/0, '_tryTakeVar'/0, '_peekVar'/0, '_tryPeekVar'/0, '_putVar'/0, '_killVar'/0]).

avar(Contents) ->
  receive
    {put, X} -> avar(X);
    {get, From} -> From ! Contents, avar(empty); % NO
    {peek, From} -> From ! Contents, avar(Contents);
    die -> exit
  end.

'_makeVar'(C) ->
  fun (Succ, Err) ->
    Pid = spawn(fun () -> avar(empty) end),
    Succ(Pid),
    C
  end.

'_takeVar'() ->
  fun (C,AVar) ->
    fun (Succ, Err) ->
      AVar ! { get, self() },
      receive
        V -> Succ(V)
      end,
      C
    end
  end.
'_tryTakeVar'() ->
  fun (Nothing,Just,C,AVar) ->
    fun (Succ, Err) ->
      AVar ! { get, self() },
      receive
        V -> Succ(Just(V))
      after
        0 -> Succ(Nothing)
      end,
      C
    end
  end.

'_peekVar'() ->
  fun (C,AVar) ->
    fun (Succ, Err) ->
      AVar ! { peek, self() },
      receive
        V -> Succ(V)
      end,
      C
    end
  end.
'_tryPeekVar'() ->
  fun (Nothing,Just,C,AVar) ->
    fun (Succ, Err) ->
      AVar ! { peek, self() },
      receive
        V -> Succ(Just(V))
      after
        0 -> Succ(Nothing)
      end,
      C
    end
  end.

'_putVar'() ->
    fun (C,AVar,V) ->
      fun (Succ, Err) ->
        AVar ! { put, V },
        Succ(unit)
      end
    end.
'_killVar'() ->
  fun (C,AVar,B) ->
    fun (Succ, Err) ->
      AVar ! die,
      C
    end
  end.
