{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod.Core
import Net.Stocks
import Net.IEX.Quote
import Text.Printf

import Text.Blaze (preEscapedString)

pformat :: Double -> String
pformat = printf "%+.2f%%" . (100*)

loveHate :: Double -> String
loveHate p = if p > 0 then "love" else "hate"

getHomeR :: Handler TypedContent
getHomeR = do
    mAmd <- liftIO $ getQuote "AMD"
    case mAmd of
        Just amd -> selectRep $ do
            provideRep $ defaultLayout' amd
            provideJson $ object ["dan" .= loveHate (changePercent amd)
                                 ,"changePercent" .= changePercent amd
                                 ]
        Nothing -> selectRep $ do
            provideRep $ failLayout
            provideJson $ object ["error" .= ("Couldn't retrieve AMD price data." :: String)]
  where
    defaultLayout' amd' = defaultLayout $ do
        setTitle . preEscapedString $ "WTF I " ++ (loveHate (changePercent amd')) ++ " Dan now"
        [whamlet|
            $with delta <- changePercent amd'
                <p>WTF I #{loveHate delta} Dan now
                <p>AMD #{pformat delta} today
        |]

    failLayout = defaultLayout $ do
        setTitle "Couldn't retrieve AMD price data."
        [whamlet|Couldn't retrieve AMD price data.|]

