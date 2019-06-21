module Main where

import           Data.Fallible
import qualified Data.List     as L

main :: IO ()
main = test
  where
    test = run "Alice" "dummy" False

run :: String -> Token -> Bool -> IO ()
run targetName token verbose = evalContT $ do
  users    <- lift (getUsers token) !?= exit . logDebug'
  targetId <- userId <$> L.find isTarget users ??? exit (logDebug' emsg)
  channels <- lift (getChannels token) !?= exit . logDebug'
  lift $ mapM_ (logDebug' . channelName) $
    filter (elem targetId . channelMembers) channels
  where
    logDebug' = logDebug verbose
    emsg = "user not found: " ++ targetName
    isTarget user = userName user == targetName

logDebug :: Bool -> String -> IO ()
logDebug verbose msg = if verbose then putStrLn msg else pure ()

-- Dummy API

type UserId = String

data User = User
  { userId    :: UserId
  , userName  :: String
  , userEmail :: String
  , userAdmin :: Bool
  } deriving (Show, Eq)

data Channel = Channel
  { channelId      :: String
  , channelName    :: String
  , channelMembers :: [UserId]
  , channelPrivate :: Bool
  } deriving (Show, Eq)

type Error = String

type Token = String

getUsers :: Token -> IO (Either Error [User])
getUsers ""     = pure (Left "invalid token")
getUsers _token = pure (Right _users)

_users :: [User]
_users =
  [ User "u123456" "Alice" "alice@example.com" False
  , User "u123457" "Bob"   "bob@example.com"   False
  , User "u123458" "Curry" "curry@example.com" True
  ]

getChannels :: Token -> IO (Either Error [Channel])
getChannels ""     = pure (Left "invalid token")
getChannels _token = pure (Right _channels)

_channels :: [Channel]
_channels =
  [ Channel "c123456" "general" (map userId _users) False
  , Channel "c123457" "random"  (map userId _users) False
  , Channel "c123458" "owners"  (map userId $ filter userAdmin _users) True
  , Channel "c123459" "secret"  (map userId $ take 2 _users) True
  ]
