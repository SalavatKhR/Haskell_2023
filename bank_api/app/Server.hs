{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.List
import Network.Wai.Handler.Warp (run)
import Servant
import API

type BankState = TVar [(UserId, Money)]

server :: BankState -> Server BankAPI
server state = registerUser
  :<|> deleteUser
  :<|> openAccount
  :<|> closeAccount
  :<|> deposit
  :<|> withdraw
  :<|> transfer
  :<|> transferToUser
  where
    registerUser :: UserRegistration -> Handler User
    registerUser userReg = liftIO $ do
      atomically $ do
        bank <- readTVar state
        let userId = generateUserId bank
        let userAccount = generateUserAccount userId
        writeTVar state ((userId, 0) : bank)
        return userId

    deleteUser :: UserId -> Handler ()
    deleteUser userId = liftIO $ do
      atomically $ do
        bank <- readTVar state
        if hasAccount userId bank
          then throwError $ err400 { errBody = "User has active accounts" }
          else writeTVar state (deleteUserAccounts userId bank)
          where
            hasAccount :: UserId -> [(UserId, Money)] -> Bool
            hasAccount userId' = any (\(uId, _) -> userId' == uId)
            
            deleteUserAccounts :: UserId -> [(UserId, Money)] -> [(UserId, Money)]
            deleteUserAccounts userId' = filter (\(uId, _) -> userId' /= uId)

    openAccount :: UserAccount -> Handler Account
    openAccount userAccount = liftIO $ do
      atomically $ return userAccount

    closeAccount :: UserId -> Handler ()
    closeAccount userId = liftIO $ do
      atomically $ do
        bank <- readTVar state
        let updatedBank = map (\(uId, balance) -> if uId == userId && balance == 0 then (uId, balance - 1) else (uId, balance)) bank
        writeTVar state updatedBank

    deposit :: UserId -> Money -> Handler ()
    deposit userId money = liftIO $ do
      atomically $ do
        bank <- readTVar state
        let updatedBank = map (\(uId, balance) -> if uId == userId then (uId, balance + money) else (uId, balance)) bank
        writeTVar state updatedBank

    withdraw :: UserId -> Money -> Handler ()
    withdraw userId money = liftIO $ do
      atomically $ do
        bank <- readTVar state
        case lookup userId bank of
          Nothing -> throwError $ err404 { errBody = "User not found" }
          Just balance ->
            if balance >= money
              then writeTVar state $ map (\(uId, balance') -> if uId == userId then (uId, balance' - money) else (uId, balance')) bank
              else throwError $ err400 { errBody = "Insufficient balance" }

    transfer :: UserId -> UserId -> Money -> Handler ()
    transfer fromUserId toUserId money = liftIO $ do
      atomically $ do
        bank <- readTVar state
        case (lookup fromUserId bank, lookup toUserId bank) of
          (Just fromBalance, Just toBalance) ->
            if fromBalance >= money
              then writeTVar state $ map (\(uId, balance) ->
                      if uId == fromUserId then (uId, balance - money)
                      else if uId == toUserId then (uId, balance + money)
                      else (uId, balance)
                    ) bank
              else throwError $ err400 { errBody = "Insufficient balance" }
          _ -> throwError $ err404 { errBody = "User not found" }

    transferToUser :: UserId -> UserId -> Money -> Handler ()
    transferToUser fromUserId toUserId money = liftIO $ do
      atomically $ do
        bank <- readTVar state
        case lookup fromUserId bank of
          Nothing -> throwError $ err404 { errBody = "User not found" }
          Just fromBalance ->
            if fromBalance >= money
              then writeTVar state $ map (\(uId, balance) ->
                      if uId == fromUserId then (uId, balance - money)
                      else if uId == toUserId then (uId, balance + money)
                      else (uId, balance)
                    ) bank
              else throwError $ err400 { errBody = "Insufficient balance" }

generateUserId :: [(UserId, Money)] -> UserId
generateUserId bank = show $ length bank + 1  -- Простая генерация нового идентификатора пользователя

generateUserAccount :: UserId -> UserAccount
generateUserAccount userId = "acc_" ++ userId  -- Генерация нового счета пользователя

bankServer :: BankState -> Application
bankServer state = serve bankAPI (server state)

runServer :: IO ()
runServer = do
  bankState <- newTVarIO [] -- Создание пустого банка
  run 5000 (bankServer bankState)