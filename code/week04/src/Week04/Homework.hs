{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text, unpack)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet
import Control.Monad.Freer.Extras as Extras
import Data.Void                  (Void)

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams
--added error handling and logging to the contract
payContract :: Contract () PaySchema Text ()
payContract = do
    Contract.logInfo @String "hello from the contract"
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    payContract

--Created a wrapper contact to catch and handle the exception
--The expected behavior is to catch the exception and continue
--this contract returns Void which means it does not throw any errors 
payContractExceptionHandler :: Contract () PaySchema Void ()
payContractExceptionHandler = do
    Contract.handleError
        (\err -> Contract.logError $ "caught: " ++ unpack err)
        payContract
    payContractExceptionHandler

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace x y = do
    Extras.logInfo $ "in pay trace x=" ++ show x  ++ " y=" ++ show y
    h1 <- activateContractWallet (Wallet 1) payContractExceptionHandler
    callEndpoint @"pay" h1 $ PayParams
        { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
          , ppLovelace  = x
        } 
    void $ Emulator.waitNSlots 1
    callEndpoint @"pay" h1 $ PayParams
        { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
          , ppLovelace  = y
        }
    void $ Emulator.waitNSlots 1
    xs <- observableState h1
    Extras.logInfo $ show xs

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000
