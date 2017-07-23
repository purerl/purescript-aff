-module(control_monad_aff@foreign).
-export(['_cancelWith'/0, '_delay'/0, '_unsafeInterleaveAff'/1, '_forkAff'/0, '_forkAll'/0, '_makeAff'/1, '_pure'/1, '_throwError'/0, '_fmap'/0, '_bind'/0, '_attempt'/0, '_runAff'/0, '_liftEff'/0, '_tailRecM'/0]).


% _cancelWith :: forall e a. Fn3 (Canceler e) (Aff e a) (Canceler e) (Aff e a)
'_cancelWith'() -> error("not implemented"). %fun (C,A,C) -> error("not implemented") end.

'_delay'() -> fun (C,N) ->
  fun (Succ, Err) ->
    receive
    after N ->
      Succ(unit),
      fun (_E) -> fun (S, C) -> false end end
  end
end.

'_unsafeInterleaveAff'(A) -> A.

'_forkAff'() -> fun (C,Aff) ->
  VoidF = fun (_) -> z end,
  fun (Succ, Err) ->
    Canceler = Aff(VoidF, VoidF),
    Succ(Canceler),
    C
  end
end.

% _forkAll :: forall f e a b. Fn3 (Canceler e) ((b -> a -> b) -> b -> f a -> b) (f (Aff e a)) (Aff e (Canceler e))
'_forkAll'() -> error("not implemented"). %fun (C, X, Y) -> error("not implemented") end.

'_makeAff'(Cb) -> fun (Succ, Err) ->
  StaticCanceler = fun (_E) -> fun (S,C) -> false end end,
  io:put_chars("makeAff\n"),
  try
    ( (Cb(fun (E) ->  io:put_chars("makeAff cb E1\n"), fun () ->  io:put_chars("makeAff cb E2\n"), Err(E) end end))
        (fun (V) -> io:put_chars("makeAff cb S1\n"), fun () -> io:put_chars("makeAff cb S2\n"), Succ(V) end end)
    )()

  of
    E -> io:put_chars("makeAff res\n"), StaticCanceler
  catch
    error:Error -> io:format("makeAff err~p~n", [Error]), Err(Error)
  end,
  StaticCanceler
end.

'_pure'(V) ->
  fun (Succ, Err) ->
    Succ(V),
    fun (_E) -> fun (S, C) -> false end end
  end.

'_throwError'() -> fun (C, E) ->
  fun (Succ, Err) ->
    Err(E),
    C
  end
end.

'_fmap'() -> fun (F, A) ->
  fun (Succ, Err) ->
    A(fun (V) -> Succ(F(V)) end, Err)
  end
end.

'_bind'() -> fun (C, A, F) ->
  fun (Succ, Err) ->
    % TODO canceller

    A(fun (V) ->
      (F(V))(Succ, Err),
      C
    end, Err),
    C

  end
end.

% exports._bind = function (alwaysCanceler, aff, f) {
%   return function (success, error) {
%     var canceler1, canceler2;
%
%     var isCanceled    = false;
%     var requestCancel = false;
%
%     var onCanceler = function () {};
%
%     canceler1 = aff(function (v) {
%       if (requestCancel) {
%         isCanceled = true;
%
%         return alwaysCanceler;
%       } else {
%         canceler2 = f(v)(success, error);
%
%         onCanceler(canceler2);
%
%         return canceler2;
%       }
%     }, error);
%
%     return function (e) {
%       return function (s, f) {
%         requestCancel = true;
%
%         if (canceler2 !== undefined) {
%           return canceler2(e)(s, f);
%         } else {
%           return canceler1(e)(function (bool) {
%             if (bool || isCanceled) {
%               s(true);
%             } else {
%               onCanceler = function (canceler) {
%                 canceler(e)(s, f);
%               };
%             }
%           }, f);
%         }
%       };
%     };
%   };
% };

'_attempt'() -> fun (Left, Right, Aff) ->
  fun (Succ, Err) ->
    Aff(fun (V) ->
      Succ(Right(V))
    end, fun (E) ->
      Succ(Left(E))
    end)
  end
end.

'_runAff'() -> fun (ErrorT, SuccT, Aff) ->
  % TODO: Address complexities of JS version
  C = Aff(fun (V) ->
    (SuccT(V))()
  end, fun (E) ->
    (ErrorT(E))()
  end),
  fun () -> C end
end.

'_liftEff'() -> fun (C, E) ->
  fun (Succ, Err) ->
    try E() of
      X -> Succ(X),
      C
    catch
      error:Error -> Err(Error),
      C
    end
  end
end.

% _tailRecM :: forall e a b. Fn3 (Step a b -> Boolean) (a -> Aff e (Step a b)) a (Aff e b)
'_tailRecM'() -> error("tailrecm not implemented"). %fun (F, G, A) -> error("not implemented") end.
