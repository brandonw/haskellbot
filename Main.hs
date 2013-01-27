import Network
import System.IO
import Text.Printf
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import Control.Monad.State.Lazy

server :: String
server = "irc.esper.net"

port :: Int
port = 6667

channel :: String
channel = "#triangle"

nick :: String
nick = "haskellbot"

data Bot = Bot { socket :: Handle
               , sentNick :: Bool
               , sentJoin :: Bool }

type Net = StateT Bot IO

io :: IO a -> Net a
io = liftIO

main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop       = evalStateT run

connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h False False)
  where
    notify = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
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
        (_, "NOTICE", _) -> sendNick
        (_, "MODE", _)   -> sendJoin
        _                -> eval g
  where
    extract s           = (prefix s, command s, params s)
    prefix l@(':' : _)  = takeWhile (/= ' ') l
    prefix _            = ""
    command l@(':' : _) = takeWhile (/= ' ') . tail . dropWhile (/= ' ') $ l
    command xs          = takeWhile (/= ' ') xs
    params l@(':' : _)  = tail . dropWhile (/= ' ') . tail . dropWhile (/= ' ') $ l
    params xs           = tail . dropWhile (/= ' ') $ xs
    pong                = write "PONG"

updateBotSentNick :: IO Bot -> IO Bot
updateBotSentNick b = do b' <- b
                         return $ Bot (socket b') True (sentJoin b')

updateBotSentJoin :: IO Bot -> IO Bot
updateBotSentJoin b = do b' <- b
                         return $ Bot (socket b') (sentNick b') True

sendNick :: Net ()
sendNick = do write "NICK" nick
              write "USER" (nick ++ " 0 * :haskellbot")
              modify updateBotSentNick

sendJoin :: Net ()
sendJoin = do write "JOIN" channel
              modify updateBotSentJoin

eval :: (String, String, String) -> Net ()
eval (prefix, "PRIVMSG", params) = evalPM (tail prefix)
                                          (takeWhile (/= ' ') params)
                                          (drop 2 . dropWhile (/= ' ') $ params)
eval (_, _, _) = return ()

evalPM :: String -> String -> String -> Net ()
evalPM src dest msg@('!' : _) = evalCommand src dest
                                            (takeWhile (/= ' ') msg)
                                            ((tail . dropWhile (/= ' ')) msg)
evalPM src dest msg           = evalGeneric src dest msg

evalCommand :: String -> String -> String  -> String -> Net ()
evalCommand _ _ "!quit" args  = write "QUIT" $ ':' : args
evalCommand _ dest "!id" args = privmsg dest args

evalGeneric :: String -> String -> String -> Net ()
evalGeneric _ _ _ = return ()
