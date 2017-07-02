module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, runAff, makeAff, launchAff, delay, forkAff, forkAll, Canceler(..), cancel, attempt, finally, apathize)
import Control.Monad.Aff.AVar (AVAR, makeVar, makeVar', putVar, modifyVar, takeVar, peekVar, killVar, tryTakeVar, tryPeekVar)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log) as Eff
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error, message, try)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (Step(..), tailRecM)
-- import Control.Parallel (parallel, sequential)
import Data.Either (either, fromLeft, fromRight, Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Unfoldable (replicate)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce

type Test a = forall e. Aff (console :: CONSOLE | e) a
type TestAVar a = forall e. Aff (console :: CONSOLE, avar :: AVAR | e) a

timeout :: Milliseconds → TestAVar Unit → TestAVar Unit
timeout ms aff = do
  exn <- makeVar
  clr1 <- forkAff (delay ms *> putVar exn (Just "Timed out"))
  clr2 <- forkAff (aff *> putVar exn Nothing)
  res ← takeVar exn
  log (show res)
  case res of
    Nothing -> void (clr1 `cancel` error "Done")
    Just e -> void (clr2 `cancel` error "Done") *> throwError (error e)
--
-- replicateArray :: forall a. Int -> a -> Array a
-- replicateArray = replicate
--
test_sequencing :: Int -> Test Unit
test_sequencing 0 = log "Done"
test_sequencing n = do
  delay $ Milliseconds 100.0
  log (show (n / 10) <> " seconds left")
  test_sequencing (n - 1)

foreign import synchronousUnexpectedThrowError :: forall e. Eff e Unit

test_makeAff :: Test Unit
test_makeAff = unsafePartial do
  s <- attempt $ makeAff \reject resolve -> resolve "ok"
  log $ "makeAff success is " <> fromRight s

  asyncF <- attempt $ makeAff \reject resolve -> reject (error "ok")
  case asyncF of
    Left s -> log "Left"
    Right _ -> log "Right"
  log $ "f:" <> show (fromLeft asyncF)


  -- log $ "makeAff asynchronous failure is " <> message (fromLeft asyncF)
  -- log $ "makeAff asynchronous failure is " <> show (fromLeft asyncF)


  asyncF' <- attempt $ makeAff \reject resolve -> synchronousUnexpectedThrowError

  asyncF'' <- attempt $ makeAff \reject resolve -> synchronousUnexpectedThrowError

  asyncF''' <- attempt $ makeAff \reject resolve -> synchronousUnexpectedThrowError
  -- case asyncF' of
  --   Left s -> log $ "Left" <> show s
  --   Right _ -> log "Right"
  -- let f1 = show (fromLeft asyncF'')
  -- log (f1 <> "")

  -- log $ "makeAff synchronous failure is " <> message (fromLeft asyncF')
  -- log $ "makeAff synchronous failure is " <> show (fromLeft asyncF')



  log "Success: makeAff is ok"
--
test_pure :: Test Unit
test_pure = do
  pure unit
  pure unit
  pure unit
  log "Success: Got all the way past 4 pures"

test_attempt :: Test Unit
test_attempt = do
  e <- attempt (throwError (error "Oh noes!"))
  either (const $ log "Success: Exception caught") (const $ log "Failure: Exception NOT caught!!!") e

test_apathize :: Test Unit
test_apathize = do
  apathize $ throwError (error "Oh noes!")
  log "Success: Exceptions don't stop the apathetic"

test_putTakeVar :: TestAVar Unit
test_putTakeVar = do
  v <- makeVar
  _ <- forkAff (delay (Milliseconds 0.0) *> putVar v 1.0)
  a <- takeVar v
  log ("Success: Value " <> show a)

test_peekVar :: TestAVar Unit
test_peekVar = do
  timeout (Milliseconds 1000.0) do
    v <- makeVar
    _ <- forkAff (delay (Milliseconds 0.0) *> putVar v 1.0)
    a1 <- peekVar v
    a2 <- takeVar v
    when (a1 /= a2) do
      throwError (error "Something horrible went wrong - peeked var is not equal to taken var")
    log ("Success: Peeked value not consumed")

  timeout (Milliseconds 1000.0) do
    w <- makeVar
    putVar w true
    b <- peekVar w
    when (not b) do
      throwError (error "Something horrible went wrong - peeked var is not true")
    log ("Success: Peeked value read from written var")

  timeout (Milliseconds 1000.0) do
    x <- makeVar
    res <- makeVar' 1
    _ <- forkAff do
      c <- peekVar x
      putVar x 1000
      d <- peekVar x
      modifyVar (_ + (c + d)) res
    putVar x 10
    count <- takeVar res
    e <- takeVar x
    f <- takeVar x
    when (not (count == 21 && e == 10 && f == 1000)) do
      throwError (error "Something horrible went wrong - peeked consumers/producer ordering")
    log "Success: peekVar consumer/producer order maintained"
--
-- test_killFirstForked :: Test Unit
-- test_killFirstForked = do
--   c <- forkAff (delay (Milliseconds 100.0) $> "Failure: This should have been killed!")
--   b <- c `cancel` (error "Just die")
--   log (if b then "Success: Killed first forked" else "Failure: Couldn't kill first forked")
--
-- test_killVar :: TestAVar Unit
-- test_killVar = do
--   v <- makeVar
--   killVar v (error "DOA")
--   e <- attempt $ takeVar v
--   either (const $ log "Success: Killed queue dead") (const $ log "Failure: Oh noes, queue survived!") e
--
test_tryTakeVar :: TestAVar Unit
test_tryTakeVar = do
  timeout (Milliseconds 1000.0) do
    v <- makeVar
    x <- tryTakeVar v
    case x of
      Nothing -> log $ "Success: trying take an empty var"
      Just _  -> throwError $ error $ "Failure: Oh noes, take an empty var should return Nothing"

  timeout (Milliseconds 1000.0) do
    v <- makeVar
    b <- tryTakeVar v
    putVar v 1.0
    a <- tryTakeVar v
    when (a /= Just 1.0 || a == b) do
      throwError $ error ("Failure: Oh noes, tryTakeVar should take var if it available, value: " <> show a)
    log $ "Success: value taken by tryTakeVar " <> show a

test_tryPeekVar :: TestAVar Unit
test_tryPeekVar = do
  timeout (Milliseconds 1000.0) do
    v <- makeVar
    x <- tryPeekVar v
    case x of
      Nothing -> log $ "Success: try peek var return immediately"
      Just _  -> throwError $ error $ "Failure: tryPeekVar return Just when peek an empty var"

  timeout (Milliseconds 1000.0) do
    v <- makeVar
    putVar v 100.0
    a <- tryPeekVar v
    b <- takeVar v
    when (a /= Just b) do
      throwError (error "Something horrible went wrong - peeked var is not equal to taken var")
    log ("Success: Try Peeked value not consumed")

--   either (const $ log "Success: Killing both kills it dead") (const $ log "Failure: It's alive!!!") e
--
-- test_semigroupCanceler :: Test Unit
-- test_semigroupCanceler =
--   let
--     c = Canceler (const (pure true)) <> Canceler (const (pure true))
--   in do
--     v <- cancel c (error "CANCEL")
--     log (if v then "Success: Canceled semigroup composite canceler"
--                      else "Failure: Could not cancel semigroup composite canceler")
--
-- test_cancelLater :: TestAVar Unit
-- test_cancelLater = do
--   c <- forkAff $ (do pure "Binding"
--                      _ <- later' 100 $ log ("Failure: Later was not canceled!")
--                      pure "Binding")
--   v <- cancel c (error "Cause")
--   log (if v then "Success: Canceled later" else "Failure: Did not cancel later")
--
-- test_cancelLaunchLater :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
-- test_cancelLaunchLater = do
--   c <- launchAff $ later' 100 $ log ("Failure: Later was not canceled!")
--   void $ launchAff $ (do v <- cancel c (error "Cause")
--                          log (if v then "Success: Canceled later" else "Failure: Did not cancel later"))
--
-- test_cancelRunLater :: forall e. Eff (console :: CONSOLE | e) Unit
-- test_cancelRunLater = do
--   c <- runAff (const (pure unit)) (const (pure unit)) $ later' 100 $ log ("Failure: Later was not canceled!")
--   void $ try $ launchAff $ (do v <- cancel c (error "Cause")
--                                log (if v then "Success: Canceled later" else "Failure: Did not cancel later"))
--
-- test_cancelParallel :: TestAVar Unit
-- test_cancelParallel = do
--   c  <- forkAff <<< sequential $ parallel (later' 100 $ log "Failure: #1 should not get through") <|>
--                              parallel (later' 100 $ log "Failure: #2 should not get through")
--   v  <- c `cancel` (error "Must cancel")
--   log (if v then "Success: Canceling composite of two Parallel succeeded"
--                    else "Failure: Canceling composite of two Parallel failed")
--
-- test_cancelRaceLeft :: TestAVar Unit
-- test_cancelRaceLeft = do
--   var <- makeVar
--   c  <- sequential
--     $ parallel (later' 250 $ putVar var true)
--     <|> parallel (later' 100 $ pure unit)
--   later' 500 $ putVar var false
--   l <- takeVar var
--   when l $ throwError (error "Failure: left side ran even though it lost the race")
--
-- test_cancelRaceRight :: TestAVar Unit
-- test_cancelRaceRight = do
--   var <- makeVar
--   c  <- sequential
--     $ parallel (later' 100 $ pure unit)
--     <|> parallel (later' 250 $ putVar var true)
--   later' 500 $ putVar var false
--   l <- takeVar var
--   when l $ throwError (error "Failure: right side ran even though it lost the race")
--
-- test_syncTailRecM :: TestAVar Unit
-- test_syncTailRecM = do
--   v <- makeVar' false
--   _ <- forkAff $ tailRecM go { n: 1000000, v }
--   b <- takeVar v
--   log (if b then "Success: Synchronous tailRecM resolved synchronously"
--             else "Failure: Synchronous tailRecM resolved asynchronously")
--   where
--   go { n: 0, v } = do
--     modifyVar (const true) v
--     pure (Done 0)
--   go { n, v } = pure (Loop { n: n - 1, v })
--
-- loopAndBounce :: forall eff. Int -> Aff (console :: CONSOLE | eff) Unit
-- loopAndBounce n = do
--   res <- tailRecM go n
--   log $ "Done: " <> show res
--   where
--   go 0 = pure (Done 0)
--   go k | mod k 30000 == 0 = do
--     later' 10 (pure unit)
--     pure (Loop (k - 1))
--   go k = pure (Loop (k - 1))
--
-- all :: forall eff. Int -> Aff (console :: CONSOLE, avar :: AVAR | eff) Unit
-- all n = do
--   var <- makeVar' 0
--   forkAll $ replicateArray n (modifyVar (_ + 1) var)
--   count <- takeVar var
--   log ("Forked " <> show count)
--
-- cancelAll :: forall eff. Int -> Aff (console :: CONSOLE, avar :: AVAR | eff) Unit
-- cancelAll n = do
--   canceler <- forkAll $ replicateArray n (later' 100000 (log "oops"))
--   canceled <- cancel canceler (error "bye")
--   log ("Cancelled all: " <> show canceled)
--
main :: Eff (console :: CONSOLE, avar :: AVAR, exception :: EXCEPTION) Unit
main =
  -- Eff.log "Testing kill of later launched in separate Aff"
  -- test_cancelLaunchLater
  --
  -- Eff.log "Testing kill of later run in separate Aff"
  -- test_cancelRunLater

  void $ runAff throwException (const (pure unit)) $ do
    -- makeAff
    -- (\e s -> do
    --   Eff.log "runAff cb"
    --   s 42)
    --   pure (unsafeCoerce (\_ -> unit)))
      -- Eff.log "runAff cb"
      -- s unit)
    log "Testing sequencing"
    test_sequencing 13

    log "Testing pure"
    test_pure

    log "Testing makeAff"
    test_makeAff

    log "Testing attempt"
    test_attempt

    log "Testing delay"
    delay (Milliseconds 0.0)
    log "Success: It happened later"

    -- log "Testing kill of later"
    -- test_cancelLater
    --
    -- log "Testing kill of first forked"
    -- test_killFirstForked
    --
    log "Testing apathize"
    test_apathize
    --
    -- log "Testing semigroup canceler"
    -- test_semigroupCanceler
    --
    log "Testing AVar - putVar, takeVar"
    test_putTakeVar

    log "Testing AVar - peekVar"
    test_peekVar
    
    -- log "Testing AVar killVar"
    -- test_killVar
    --
    -- log "Testing finally"
    -- test_finally
    --
    -- log "Test Parallel (*>)"
    -- test_parError
    --
    -- log "Testing Parallel (<|>)"
    -- test_parRace
    --
    -- log "Testing Parallel (<|>) - kill one"
    -- test_parRaceKill1
    --
    -- log "Testing Parallel (<|>) - kill two"
    -- test_parRaceKill2
    --
    -- log "Testing cancel of Parallel (<|>)"
    -- test_cancelParallel
    --
    -- log "Testing cancel of left branch in parallel (<|>)"
    -- test_cancelRaceLeft
    --
    -- log "Testing cancel of right branch in parallel (<|>)"
    -- test_cancelRaceRight
    --
    -- log "Testing synchronous tailRecM"
    -- test_syncTailRecM
    --
    log "pre-delay"
    delay (Milliseconds 1000.0)
    log "post-delay"
    --
    -- loopAndBounce 1000000
    --
    -- all 100000
    --
    -- cancelAll 100000

    log "Done testing"
