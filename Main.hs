import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Network
import Options.Applicative
import System.IO
import Text.Printf

data Options = Options String Int String String deriving (Show)

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
main = execParser opts >>= start
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> Options.Applicative.progDesc "Starts an IRC bot."
     <> header "bot - An irc bot written in Haskell." )

start :: Options -> IO()
start (Options s p c n) = bracket (connect s p n c) disconnect loop
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
