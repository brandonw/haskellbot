import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Network
import Options.Applicative
import System.Exit
import System.IO
import Text.Printf

data Options = Options
            { server      :: String
            , port        :: Int
            , channel     :: String
            , nick        :: String
            , lagInterval :: Int
            , timeout     :: Int
            , retry       :: Int
            } deriving (Show, Eq)

options :: Parser Options
options = Options
          <$> strOption
            (  long "server"
            <> short 's'
            <> metavar "HOST"
            <> help "The hostname to connect to.")
          <*> option
            (  long "port"
            <> short 'p'
            <> value 6667
            <> metavar "PORT"
            <> help "The port to use.")
          <*> strOption
            (  long "channel"
            <> short 'c'
            <> metavar "CHANNEL"
            <> help "The channel to join. Make sure to escape as needed.")
          <*> strOption
            (  long "nick"
            <> short 'n'
            <> value "haskell-bot"
            <> metavar "NICK"
            <> help "The nick to identify the bot with.")
          <*> option
            (  long "lag-interval"
            <> short 'l'
            <> value 60
            <> metavar "SECONDS"
            <> help "The number of seconds before PING to check for timeout.")
          <*> option
            (  long "timeout"
            <> short 't'
            <> value 10
            <> metavar "SECONDS"
            <> help "The number of seconds before timing out after PING is sent.")
          <*> option
            (  long "retry"
            <> short 'r'
            <> value 60
            <> metavar "SECONDS"
            <> help "The number of seconds to wait between successive re-connection attempts.")

data Bot = Bot { opts     :: Options
               , socket   :: Handle
               , sentNick :: Bool
               , sentJoin :: Bool
               } deriving (Show, Eq)

type Net = StateT Bot IO

io :: IO a -> Net a
io = liftIO

main :: IO ()
main = execParser o >>= start
  where
    o = info (helper <*> options)
      ( fullDesc
     <> Options.Applicative.progDesc "Starts an IRC bot."
     <> header "haskell-bot - An irc bot written in Haskell." )

start :: Options -> IO ()
start o = bracket (connect o) disconnect loop
  where
    disconnect = hClose . socket
    loop       = evalStateT listen

connectToServer :: String -> Int -> Int -> IO (Maybe Handle)
connectToServer s p r = do
  printf "Connecting to %s...\n" s >> hFlush stdout
  result <- try $ connectTo s (PortNumber (fromIntegral p))
  case result of
      Left (SomeException _) -> do
        printf "Failed. Waiting %d seconds before retrying...\n" r >> hFlush stdout
        threadDelay (r * 1000000)
        return Nothing
      Right h                -> do
        hSetBuffering h NoBuffering
        putStrLn "Connected successfully!"
        return (Just h)

connect :: Options -> IO Bot
connect o = do
    h <- untilJust $ connectToServer s p (retry o)
    return Bot
        { opts     = o
        , socket   = h
        , sentNick = False
        , sentJoin = False
        }
  where
    s      = server o
    p      = port o 

writeIO :: Handle -> String -> String -> IO ()
writeIO h s t = do
  hPrintf h "%s %s\r\n" s t
  printf    "> %s %s\n" s t

write :: String -> String -> Net ()
write s t = do
    h <- gets socket
    io $ writeIO h s t

privmsg :: String -> String -> Net ()
privmsg c m = write "PRIVMSG" (c ++ " :" ++ m)

timeoutAndPing :: Handle -> Int -> Int -> String -> IO (Maybe String)
timeoutAndPing h lag time msg = do
  threadDelay (lag * 1000000)
  ping h msg
  threadDelay (time * 1000000)
  putStrLn "Timed out..."
  return Nothing

reconnect :: Net ()
reconnect = do
  h <- gets socket
  o <- gets opts
  io $ hClose h
  b <- io $ connect o
  put b

ping :: Handle -> String -> IO ()
ping h = writeIO h "PING"

pong :: String -> Net ()
pong = write "PONG"

getNextMessage :: Handle -> IO (Maybe String)
getNextMessage h = do
  r <- try $ init `fmap` hGetLine h
  case r of
      Right l                -> return (Just l)
      Left (SomeException _) -> return Nothing

processMessage :: String -> Net ()
processMessage msg =
  let g = extract msg
  in do
    io (putStrLn msg)
    case g of
      (_, "PING", m)   -> pong m
      (_, "NOTICE", _) -> do n <- gets sentNick; unless n sendNick
      (_, "001", _)    -> do j <- gets sentJoin; unless j sendJoin
      _                -> eval g
  where
    extract s           = (prefix s, cmd s, params s)
    prefix l@(':' : _)  = Just $ takeWhile (/= ' ') l
    prefix _            = Nothing
    cmd l@(':' : _) = takeWhile (/= ' ') . tail . dropWhile (/= ' ') $ l
    cmd xs          = takeWhile (/= ' ') xs
    params l@(':' : _)  = tail . dropWhile (/= ' ') . tail . dropWhile (/= ' ') $ l
    params xs           = tail . dropWhile (/= ' ') $ xs

listen :: Net ()
listen = forever $ do
    o  <- gets opts
    h  <- gets socket
    a1 <- io $ async $ getNextMessage h
    a2 <- io $ async $ timeoutAndPing h (lagInterval o) (timeout o) (nick o)
    r  <- io $ waitAnyCancel [a1,a2]
    case r of
        (_, Just m)  -> processMessage m
        (_, Nothing) -> reconnect

updateBotSentNick :: Bot -> Bot
updateBotSentNick b = b { sentNick = True }

updateBotSentJoin :: Bot -> Bot
updateBotSentJoin b = b { sentJoin = True }

sendNick :: Net ()
sendNick = do o <- gets opts
              let n = nick o
                  in do write "NICK" n
                        write "USER" (n ++ " 0 * :haskellbot")
                        modify updateBotSentNick

sendJoin :: Net ()
sendJoin = do o <- gets opts
              let c = channel o
                in do write "JOIN" c
                      modify updateBotSentJoin

eval :: (Maybe String, String, String) -> Net ()
eval (Just prefix, "PRIVMSG", params) = evalPM (tail prefix)
                                          (takeWhile (/= ' ') params)
                                          (drop 2 . dropWhile (/= ' ') $ params)
eval (_, _, _) = return ()

evalPM :: String -> String -> String -> Net ()
evalPM src dest ('!' : cmdMsg) = evalCommand src
                                             dest
                                             (takeWhile (/= ' ') cmdMsg)
                                             (extractCommandParams cmdMsg)
evalPM src dest msg            = evalGeneric src dest msg

extractCommandParams :: String -> Maybe String
extractCommandParams msg = if suffix == "" || suffix == " "
                           then Nothing
                           else Just (tail suffix)
  where
    suffix = dropWhile (/= ' ') msg

evalCommand :: String -> String -> String  -> Maybe String -> Net ()
evalCommand _ _ "quit" (Just args)  = write "QUIT" (':' : args) >> io System.Exit.exitSuccess
evalCommand _ _ "quit" Nothing      = write "QUIT" ""           >> io System.Exit.exitSuccess
evalCommand _ dest "id" (Just args) = privmsg dest args
evalCommand _ _ _ _                 = return ()

evalGeneric :: String -> String -> String -> Net ()
evalGeneric _ _ _ = return ()
