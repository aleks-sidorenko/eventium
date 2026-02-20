module Bank.Models.AccountSpec (spec) where

import Bank.Models.Account
import Eventium
import Eventium.Testkit (allCommandHandlerStates)
import Test.Hspec

spec :: Spec
spec = do
  describe "Account projection" $ do
    it "should handle a series of account events" $ do
      let events =
            [ AccountOpenedAccountEvent $ AccountOpened nil 10,
              AccountDebitedAccountEvent $ AccountDebited 5 "ATM",
              AccountCreditedAccountEvent $ AccountCredited 10 "Paycheck"
            ]
          states =
            [ Account 0 Nothing [],
              Account 10 (Just nil) [],
              Account 5 (Just nil) [],
              Account 15 (Just nil) []
            ]
      allProjections accountProjection events `shouldBe` states

    it "should handle successful account transfers" $ do
      let transferUuid = read "754d7bd9-fd0b-4006-b33a-f41fd5c3ca5e" :: UUID
          targetAccount = read "44e9fd39-0179-4050-8706-d5a1d2c6d093" :: UUID
          events =
            [ AccountOpenedAccountEvent $ AccountOpened nil 10,
              AccountTransferStartedAccountEvent $ AccountTransferStarted transferUuid 6 targetAccount
            ]
          states =
            [ Account 0 Nothing [],
              Account 10 (Just nil) [],
              Account 10 (Just nil) [PendingAccountTransfer transferUuid 6 targetAccount]
            ]
      allProjections accountProjection events `shouldBe` states

      let stateAfterStarted = latestProjection accountProjection events

      accountAvailableBalance stateAfterStarted `shouldBe` 4
      commandHandlerDecide accountCommandHandler stateAfterStarted (DebitAccountAccountCommand (DebitAccount 9 "blah"))
        `shouldBe` Left (InsufficientFunds 4)

      let events' = events ++ [AccountTransferCompletedAccountEvent $ AccountTransferCompleted transferUuid]
          completedState = latestProjection accountProjection events'

      completedState `shouldBe` Account 4 (Just nil) []

  describe "Account commandHandler" $ do
    it "should handle a series of commands" $ do
      let commands =
            [ OpenAccountAccountCommand $ OpenAccount nil 100,
              DebitAccountAccountCommand $ DebitAccount 150 "ATM",
              CreditAccountAccountCommand $ CreditAccount 200 "Check",
              OpenAccountAccountCommand $ OpenAccount nil 200
            ]
          results =
            [ Account 0 Nothing [],
              Account 100 (Just nil) [],
              Account 100 (Just nil) [],
              Account 300 (Just nil) [],
              Account 300 (Just nil) []
            ]
      allCommandHandlerStates accountCommandHandler commands `shouldBe` results
