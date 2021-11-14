{-# LANGUAGE OverloadedStrings #-}

module Quote (getQuote) where

import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.String (IsString (fromString))
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock.System (SystemTime (systemSeconds), getSystemTime)
import Network.HTTP.Simple (addRequestHeader, getResponseBody, httpBS, setRequestQueryString)
import Text.HTML.TagSoup (Tag (TagOpen, TagText), isTagText, parseTags, (~==))

getQuote :: IO (String, String)
getQuote = do
    seconds <- (fromString <$> show) . systemSeconds <$> getSystemTime
    tags <- httpBS (getRequest seconds url) <&> getResponseBody <&> parseTags
    let [quote, author] = map toUtf8String . filter isTagText . take 3 . drop 16 . dropWhile (not . isQuote) $ tags
    return (removeTrailingDashes quote, author)
        where
            url = "https://zh.wikiquote.org"
            getRequest query = addRequestHeader "Accept-Language" "zh-TW" . setRequestQueryString [(query, Nothing)]
            isQuote = (~== TagOpen ("div" :: ByteString) [("id", "mp-everyday-quote")])
            toUtf8String (TagText bs) = unpack $ decodeUtf8 bs
            toUtf8String _ = error "Should not happen"
            removeTrailingDashes = takeWhile (/= '—')