{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.User where

import Import
import Text.Shakespeare.Text

getAllUsersR :: Handler Html
getAllUsersR = do
    users <- runDB $ selectList [] [Asc UserIdent]
    html <- printAllUsers users
    renderer <- getUrlRenderParams
    return (html renderer)
        
printAllUsers users = return [hamlet|
    $if null users
        <p>There are no users registered.
    $else
        <p>Overview of registered users:
        $forall Entity userID user <- users
            <li><a href=@{UserR userID}>#{userIdent user}</a>
        |]
        
getUserR :: UserId -> Handler Html
getUserR userId = do
    user <- runDB $ get userId
    case user of
        Just user' -> do
            receipts <- runDB $ selectList [ReceiptPaidBy ==. user'] []
            html <- printAllReceipts receipts (userIdent user')
            renderer <- getUrlRenderParams
            return (html renderer)
        Nothing -> return [shamlet|User not found|]

printAllReceipts receipts paidBy = return [hamlet|
    $if null receipts
        <p>You have not entered any receipts into the system.
    $else
        <p>Overview of receipts created/paid by #{paidBy}:
        $forall Entity receiptId receipt <- receipts
            <li><a href=@{ReceiptR receiptId}>#{show receiptId}</a>
        |]