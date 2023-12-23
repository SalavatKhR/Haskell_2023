{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant

type BankAPI =
  "register" :> ReqBody '[JSON] UserRegistration :> Post '[JSON] User
  :<|> "delete" :> Capture "userId" UserId :> Delete '[JSON] ()
  :<|> "openAccount" :> ReqBody '[JSON] UserAccount :> Post '[JSON] Account
  :<|> "closeAccount" :> Capture "userId" UserId :> Delete '[JSON] ()
  :<|> "deposit" :> Capture "userId" UserId :> "add" :> ReqBody '[JSON] Money :> Post '[JSON] ()
  :<|> "withdraw" :> Capture "userId" UserId :> "withdraw" :> ReqBody '[JSON] Money :> Post '[JSON] ()
  :<|> "transfer" :> Capture "fromUserId" UserId :> Capture "toUserId" UserId :> ReqBody '[JSON] Money :> Post '[JSON] ()
  :<|> "transferToUser" :> Capture "fromUserId" UserId :> Capture "toUserId" UserId :> ReqBody '[JSON] Money :> Post '[JSON] ()

type UserRegistration = String
type UserId = String
type User = String
type UserAccount = String
type Account = String
type Money = Double

bankAPI :: Proxy BankAPI
bankAPI = Proxy