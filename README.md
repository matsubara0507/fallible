# fallible

Interface for fallible data type like Maybe and Either.

## Example

ref [example/Main.hs](example/Main.hs):

```Haskell
import           Data.Fallible
import qualified Data.List     as L

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
```

exec with ghci

```
$ stack ghci
>>> :l example/Main.hs
*Main> run "Alice" "dummy" True
general
random
secret
*Main> run "Curry" "dummy" True
general
random
owners
*Main> run "Haskell" "dummy" True
user not found: Haskell
*Main> run "Haskell" "" True
invalid token
```

## Usage

### With Slack

write to slack.yaml:

```yaml
extra-deps:
  github: matsubara0507/fallible
  commit: xxx
```
