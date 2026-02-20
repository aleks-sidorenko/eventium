module Eventium.CommandHandlerSpec (spec) where

import Control.Concurrent.STM
import Control.Exception (evaluate)
import Eventium.Codec (Codec (..), DecodeError (..))
import Eventium.CommandHandler
import Eventium.Projection
import Eventium.Store.Class
import Eventium.Store.Memory
import Eventium.Testkit (allCommandHandlerStates)
import Eventium.UUID
import Test.Hspec

-- Helper to create IO-compatible stores from a TVar
makeIOStore :: TVar (EventMap event) -> (VersionedEventStoreWriter IO event, VersionedEventStoreReader IO event)
makeIOStore tvar =
  ( runEventStoreWriterUsing atomically (tvarEventStoreWriter tvar),
    runEventStoreReaderUsing atomically (tvarEventStoreReader tvar)
  )

-- Simple counter domain for testing
data CounterError = Overflow | Underflow
  deriving (Show, Eq)

type CounterHandler = CommandHandler Int Int Int CounterError

counterHandler :: CounterHandler
counterHandler = CommandHandler decide counterProj
  where
    counterProj = Projection 0 (+)
    decide state cmd
      | state + cmd > 100 = Left Overflow
      | state + cmd < 0 = Left Underflow
      | otherwise = Right [cmd]

spec :: Spec
spec = do
  describe "allCommandHandlerStates" $ do
    it "should advance state on accepted commands" $ do
      allCommandHandlerStates counterHandler [10, 20, 30]
        `shouldBe` [0, 10, 30, 60]

    it "should leave state unchanged on rejected commands" $ do
      allCommandHandlerStates counterHandler [50, 60, 10]
        `shouldBe` [0, 50, 50, 60]
    -- 60 is rejected (50+60>100), state stays at 50

    it "should handle mixed accept/reject" $ do
      allCommandHandlerStates counterHandler [30, -50, 20, 80]
        `shouldBe` [0, 30, 30, 50, 50]
  -- -50 rejected (30-50<0), 80 rejected (50+80>100)

  describe "applyCommandHandler" $ do
    it "should return Right events on success" $ do
      tvar <- eventMapTVar
      let (writer, reader) = makeIOStore tvar
          uuid = uuidFromInteger 1

      result <- applyCommandHandler writer reader counterHandler uuid 10
      result `shouldBe` Right [10]

      -- Verify events are stored
      events <- getEvents reader (allEvents uuid)
      map streamEventEvent events `shouldBe` [10]

    it "should return CommandRejected on domain error" $ do
      tvar <- eventMapTVar
      let (writer, reader) = makeIOStore tvar
          uuid = uuidFromInteger 1

      result <- applyCommandHandler writer reader counterHandler uuid 150
      result `shouldBe` Left (CommandRejected Overflow)

      -- Verify no events stored
      events <- getEvents reader (allEvents uuid)
      events `shouldBe` []

    it "should return ConcurrencyConflict on write failure" $ do
      tvar <- eventMapTVar
      let (_, reader) = makeIOStore tvar

      -- Use a writer that always fails to simulate concurrency conflict
      let failWriter = EventStoreWriter $ \_ _ _ ->
            return $ Left (EventStreamNotAtExpectedVersion (42 :: EventVersion))

      result <- applyCommandHandler failWriter reader counterHandler (uuidFromInteger 1) 5
      result `shouldBe` Left (ConcurrencyConflict (EventStreamNotAtExpectedVersion 42))

    it "should apply multiple commands sequentially" $ do
      tvar <- eventMapTVar
      let (writer, reader) = makeIOStore tvar
          uuid = uuidFromInteger 1

      result1 <- applyCommandHandler writer reader counterHandler uuid 30
      result1 `shouldBe` Right [30]

      result2 <- applyCommandHandler writer reader counterHandler uuid 40
      result2 `shouldBe` Right [40]

      -- Third command should be rejected (30+40+50=120 > 100)
      result3 <- applyCommandHandler writer reader counterHandler uuid 50
      result3 `shouldBe` Left (CommandRejected Overflow)

      -- Verify only two events stored
      events <- getEvents reader (allEvents uuid)
      map streamEventEvent events `shouldBe` [30, 40]

  describe "codecCommandHandler" $ do
    it "should map through codecs on success" $ do
      let eventCodec = Codec show (Just . read)
          cmdCodec = Codec show (Just . read)
          wrapped = codecCommandHandler eventCodec cmdCodec counterHandler

      commandHandlerDecide wrapped 0 "10" `shouldBe` Right ["10"]

    it "should error when command decoding fails" $ do
      let eventCodec = Codec show (Just . read)
          cmdCodec = Codec (show :: Int -> String) (\_ -> Nothing :: Maybe Int)
          wrapped = codecCommandHandler eventCodec cmdCodec counterHandler

      evaluate (commandHandlerDecide wrapped 0 "anything") `shouldThrow` (\(DecodeError ctx _) -> ctx == "codecCommandHandler")

    it "should propagate domain errors through codec" $ do
      let eventCodec = Codec show (Just . read)
          cmdCodec = Codec show (Just . read)
          wrapped = codecCommandHandler eventCodec cmdCodec counterHandler

      commandHandlerDecide wrapped 0 "150" `shouldBe` Left Overflow
