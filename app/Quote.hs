{-# LANGUAGE OverloadedStrings #-}

module Quote (getQuote) where

import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Simple (getResponseBody, httpBS)
import Text.HTML.TagSoup (Tag (TagOpen, TagText), isTagText, parseTags, (~==))

getQuote :: IO (String, String)
getQuote = do
    tags <- httpBS "https://zh.wikiquote.org" <&> getResponseBody <&> parseTags
    let [quote, author] = map toUtf8String . filter isTagText . take 3 . drop 16 . dropWhile (not . isQuote) $ tags
    return (removeTrailingDashes quote, author)
        where
            isQuote = (~== TagOpen ("div" :: ByteString) [("id", "mp-everyday-quote")])
            toUtf8String (TagText bs) = unpack $ decodeUtf8 bs
            toUtf8String _ = error "Should not happen"
            removeTrailingDashes = init . init