{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Client.TLS
import Network.HTTP.Client
import Text.HTML.TagSoup
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (any)
import Data.Maybe (catMaybes, isJust, fromJust, isNothing, fromMaybe)
import qualified Debug.Trace as DT
import Control.Concurrent.STM.TChan

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

import Control.Exception
import Control.Monad (void)
import Control.Concurrent.STM
import Control.Concurrent (forkIO)

-- form action
-- a href

userAgent :: B.ByteString
userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36"

magicQueueString = "currently at position"

cccAddress = "https://tickets.events.ccc.de/33c3/"
addrRoot = "https://tickets.events.ccc.de"
--cccAddress = "http://127.0.0.1:8080/33c3/"
--addrRoot = "http://127.0.0.1:8080"

containsLink :: Tag Text -> Bool
containsLink (TagOpen rawTagName attrs) = correctTagName && correctAttr
  where tagName        = T.toLower rawTagName
        correctTagName = tagName == "form" || tagName == "a"
        correctAttr    = any (\(attrName, _) -> attrName == "action" || attrName == "href") attrs
containsLink _ = False

data JoinLink = Get Text | Post Text | NoUrl
  deriving (Eq, Ord, Show)

toLink :: Map Text Text -> JoinLink
toLink attrMap = case M.lookup "action" attrMap of
  Just actionUrl -> case M.lookup "method" attrMap of
    Just method -> case T.toLower method of
      "post" -> Post actionUrl
      "get"  -> Get actionUrl
      _      -> NoUrl
    Nothing -> NoUrl
  Nothing -> case M.lookup "href" attrMap of
    Just actionUrl -> Get actionUrl
    Nothing -> NoUrl

urlToAbsolute :: Text -> Text
urlToAbsolute link = case (T.count "http://" link > 0) || (T.count "https://" link > 0) of
  True -> link
  False -> T.append addrRoot link

linkToAbsolute :: JoinLink -> JoinLink
linkToAbsolute (Get linkUrl) = Get $ urlToAbsolute linkUrl
linkToAbsolute (Post linkUrl) = Post $ urlToAbsolute linkUrl
linkToAbsolute NoUrl = NoUrl

joinedQueue :: [Tag Text] -> Bool
joinedQueue = any isJoinedNode

isJoinedNode :: Tag Text -> Bool
isJoinedNode (TagText str) = T.count magicQueueString (T.toLower str) > 0
isJoinedNode _ = False

filterNoops :: [JoinLink] -> [JoinLink]
filterNoops = filter (\link -> case link of
  Get _  -> True
  Post _ -> True
  NoUrl  -> False)

extractLinks :: Tag Text -> [JoinLink]
extractLinks (TagOpen _ attrs) = map linkToAbsolute . filterNoops $ [toLink attrMap]
  where attrMap = M.fromList attrs
extractLinks _ = []

findLinks :: [Tag Text] -> [JoinLink]
findLinks = concat . map extractLinks . filter containsLink

newtype Recursive = Recursive Bool

makeRequest_ :: TChan [PrettyCookie] -> Manager -> Recursive -> Maybe CookieJar -> JoinLink -> IO ()
makeRequest_ chan manager (Recursive isRecursive) inputJar wrappedUrl = do
  let cookieJar' = fromMaybe (createCookieJar []) inputJar
      (reqUrl, method) = case wrappedUrl of
        Get actionUrl -> (actionUrl, "GET")
        Post actionUrl -> (actionUrl, "POST")
        NoUrl          -> ("", "")

  case (parseRequest $ T.unpack reqUrl) :: Maybe Request of
    Nothing -> return ()
    Just request -> do
      let request' = request {
          method = method
        , requestHeaders = [("user-agent", userAgent), ("referer", encodeUtf8 cccAddress)]
        , cookieJar = Just cookieJar'
      }
      response <- httpLbs request' manager
      let responseCookies = responseCookieJar response
          body            = decodeUtf8 . toStrict . responseBody $ response
          parsedBody      = parseTags body
          hasJoinedQueue  = joinedQueue parsedBody

      if hasJoinedQueue
        then void . atomically . writeTChan chan . map prettyPrintCookie . destroyCookieJar $ responseCookies
        else return ()

      if isRecursive
        then do
          let links = findLinks parsedBody
          void $ mapM (makeRequest chan manager (Recursive False) (Just responseCookies)) links
        else return ()

makeRequest :: TChan [PrettyCookie] -> Manager -> Recursive -> Maybe CookieJar -> JoinLink -> IO ()
makeRequest chan manager recursive inputJar url = catch (makeRequest_ chan manager recursive inputJar url) $
  \e -> return (e :: SomeException) >> do
    putStrLn $ "request failing for url " ++ (show url)

type PrettyCookie = (B.ByteString, B.ByteString)

prettyPrintCookie :: Cookie -> PrettyCookie
prettyPrintCookie c = (cookie_name c, cookie_value c)


reqLoop :: TChan [PrettyCookie] -> Manager -> IO ()
reqLoop chan manager = do
  makeRequest chan manager (Recursive True) Nothing $ Get cccAddress
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

howManyConcurrentCrawls = 20
