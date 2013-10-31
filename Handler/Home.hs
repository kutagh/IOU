{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

-- * Home

getHomeR :: Handler Html
getHomeR = do
    muser <- maybeAuth
    defaultLayout $ do
        setTitle "IOU"
        [whamlet|
            <p>Welcome!
            <ul>
                <li>
                $maybe user <- muser
                    You're currently logged in as: #{userIdent (entityVal user)}
                    <a href=@{AuthR LogoutR}>Log out
                $nothing
                    <a href=@{AuthR LoginR}>Login
                <li>
                    <a href=@{AllUsersR}>Show all users
                <li>
                    <a href=@{AllReceiptsR}>Show all receipts
                <li>
                    <a href=@{PaymentsR}>Show all payments
        |]
