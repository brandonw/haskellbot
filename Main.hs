import System.Console.GetOpt
import System.Environment
import Network
import System.IO
import Text.Printf
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import Control.Monad.State.Lazy

data Options = Options
            { optServer  :: String
            , optPort    :: Int
            , optChannel :: String
            , optNick    :: String
            } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
  { optServer = ""
  , optPort = 6667
  , optChannel = ""
  , optNick = "haskell-bot"
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "s" ["server"]  (ReqArg (\arg opt -> opt { optServer  = arg }) "irc.foo.com")
    "The hostname to connect to as the IRC server."
  , Option "p" ["port"]    (ReqArg (\arg opt -> opt { optPort    = read arg }) "6667")
    "The port to use."
  , Option "c" ["channel"] (ReqArg (\arg opt -> opt { optChannel = arg }) "\"#test\"")
    "The channel to join, including the #. Make sure to escape '#' or quote the name."
  , Option "n" ["nick"]    (ReqArg (\arg opt -> opt { optNick    = arg }) "haskell-bot")
    "The nick to call the bot."
  ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[])   -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: bot -s SERVER -c \"#CHANNEL\" [-n NICK] [-p PORT]"

data Bot = Bot { socket :: Handle
               , nick :: String
               , channel :: String
               , sentNick :: Bool
               , sentJoin :: Bool
               } deriving (Show, Eq)

type Net = StateT Bot IO

io :: IO a -> Net a
io = liftIO

main :: IO ()
main = do
  args               <- System.Environment.getArgs
  (opts, nonOptions) <- compilerOpts args
  bracket
    (connect (optServer opts) (optPort opts) (optNick opts) (optChannel opts))
    disconnect
    loop
  where
    disconnect = hClose . socket
    loop       = evalStateT run

connect :: String -> Int -> String -> String -> IO Bot
connect s p n c = notify $ do
    h <- connectTo s (PortNumber (fromIntegral p))
    hSetBuffering h NoBuffering
    return Bot
        { socket = h
        , nick = n
        , channel = c
        , sentNick = False
        , sentJoin = False
        }
  where
    notify = bracket_
        (printf "Connecting to %s ... " s >> hFlush stdout)
        (putStrLn "Done.")

run :: Net ()
run = gets socket >>= listen

write :: String -> String -> Net ()
write s t = do
    h <- gets socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

privmsg :: String -> String -> Net ()
privmsg c m = write "PRIVMSG" (c ++ " :" ++ m)

listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    let g = extract s
    io (putStrLn s)
    case g of
        (_, "PING", msg) -> pong msg
        (_, "NOTICE", _) -> do n <- gets sentNick; unless n sendNick
        (_, "MODE", _)   -> do j <- gets sentJoin; unless j sendJoin
        _                -> eval g
  where
    extract s           = (prefix s, command s, params s)
    prefix l@(':' : _)  = Just $ takeWhile (/= ' ') l
    prefix _            = Nothing
    command l@(':' : _) = takeWhile (/= ' ') . tail . dropWhile (/= ' ') $ l
    command xs          = takeWhile (/= ' ') xs
    params l@(':' : _)  = tail . dropWhile (/= ' ') . tail . dropWhile (/= ' ') $ l
    params xs           = tail . dropWhile (/= ' ') $ xs
    pong                = write "PONG"

updateBotSentNick :: Bot -> Bot
updateBotSentNick b = b { sentNick = True }

updateBotSentJoin :: Bot -> Bot
updateBotSentJoin b = b { sentJoin = True }

sendNick :: Net ()
sendNick = do n <- gets nick
              write "NICK" n
              write "USER" (n ++ " 0 * :haskellbot")
              modify updateBotSentNick

sendJoin :: Net ()
sendJoin = do c <- gets channel
              write "JOIN" c
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
evalCommand _ _ "quit" (Just args)  = write "QUIT" $ ':' : args
evalCommand _ _ "quit" Nothing      = write "QUIT" ""
evalCommand _ dest "id" (Just args) = privmsg dest args
evalCommand _ _ _ _                  = return ()

evalGeneric :: String -> String -> String -> Net ()
evalGeneric _ _ _ = return ()
