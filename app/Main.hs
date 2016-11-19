{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Client.TLS
import Network.HTTP.Client
import Text.HTML.TagSoup
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (any)
import Data.Maybe (catMaybes, isJust, fromJust, isNothing, fromMaybe)
import qualified Debug.Trace as DT
import Control.Concurrent.STM.TChan

import Control.Exception
import Control.Monad (void)
import Control.Concurrent.STM
import Control.Concurrent (forkIO)

-- form action
-- a href

userAgent :: B.ByteString
userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36"

magicQueueString = "currently at position"

containsLink :: Tag Text -> Bool
containsLink (TagOpen rawTagName attrs) = correctTagName && correctAttr
  where tagName        = T.toLower rawTagName
        correctTagName = tagName == "form" || tagName == "a"
        correctAttr    = any (\(attrName, _) -> attrName == "action" || attrName == "href") attrs
containsLink _ = False

toLink :: Attribute Text -> Maybe Text
toLink (attrName, attrBody) = 
  if attrName == "action" || attrName == "href" then Just attrBody else Nothing

joinedQueue :: [Tag Text] -> Bool
joinedQueue = any isJoinedNode

isJoinedNode :: Tag Text -> Bool
isJoinedNode (TagText str) = T.count magicQueueString (T.toLower str) > 0
isJoinedNode _ = False

extractLinks :: Tag Text -> [Text]
extractLinks (TagOpen _ attrs) = catMaybes $ map toLink attrs
extractLinks _ = []

findLinks :: [Tag Text] -> [Text]
findLinks = concat . map extractLinks . filter containsLink

newtype Recursive = Recursive Bool

makeRequest_ :: Manager -> Recursive -> Maybe CookieJar -> String -> IO (Bool, CookieJar)
makeRequest_ manager (Recursive isRecursive) inputJar url = do
  let cookieJar' = fromMaybe (createCookieJar []) inputJar
  case parseRequest url :: Maybe Request of
    Nothing -> return (False, cookieJar')
    Just request -> do
      let request' = request { 
          requestHeaders = [("user-agent", userAgent)]
        , cookieJar = Just cookieJar'
      }
      response <- httpLbs request' manager
      let responseCookies = responseCookieJar response
          body            = decodeUtf8 . toStrict . responseBody $ response
          parsedBody      = parseTags body
          hasJoinedQueue  = joinedQueue parsedBody

      if isRecursive 
        then do
          let links = findLinks parsedBody
          cookieJars <- mapM (makeRequest manager (Recursive False) (Just responseCookies) . T.unpack) links
          return (or $ hasJoinedQueue:(map fst $ cookieJars), mconcat . map snd $ cookieJars) 
        else return (hasJoinedQueue, responseCookies)

makeRequest :: Manager -> Recursive -> Maybe CookieJar -> String -> IO (Bool, CookieJar)
makeRequest manager recursive inputJar url = catch (makeRequest_ manager recursive inputJar url) $ 
  \e -> return (e :: SomeException) >> do
    putStrLn $ "request failing for url " ++ url
    return (False, fromMaybe (createCookieJar []) inputJar)

type PrettyCookie = (B.ByteString, B.ByteString)

prettyPrintCookie :: Cookie -> PrettyCookie
prettyPrintCookie c = (cookie_name c, cookie_value c)

reqLoop :: TChan [PrettyCookie] -> Manager -> IO ()
reqLoop chan manager = do
  (inQueue, cookiez) <- makeRequest manager (Recursive True) Nothing "https://tickets.events.ccc.de/33c3/"
  if inQueue
    then void . atomically . writeTChan chan . map prettyPrintCookie . destroyCookieJar $ cookiez
    else return ()

  reqLoop chan manager

readChan :: TChan [PrettyCookie] -> IO ()
readChan chan = do
  headers <- atomically $ readTChan chan
  putStrLn $ show headers
  readChan chan

main :: IO ()
main = do
  queueChan <- newTChanIO
  manager <- newManager tlsManagerSettings
  mapM (\_ -> forkIO $ reqLoop queueChan manager) [0..howManyConcurrentCrawls]
  readChan queueChan

howManyConcurrentCrawls = 10
