{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.User where

import Import
import Data.Maybe

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
            receiptsByUser <- runDB $ selectList [ReceiptPaidBy ==. user'] []
            rbuHtml <- printAllReceipts receiptsByUser (userIdent user')
            composed <- composer [rbuHtml]
            renderer <- getUrlRenderParams
            return (composed renderer)
        Nothing -> return [shamlet|User not found|]

        
printAllReceipts receipts paidBy = return [hamlet|
    $if null receipts
        <p>#{paidBy} has not entered any receipts into the system.
    $else
        <p>Overview of receipts created/paid by #{paidBy}:
        $forall Entity receiptId receipt <- receipts
            <li><a href=@{ReceiptR receiptId}>#{show receiptId}</a>
        |]
            
composer toCompose = return [hamlet|
    $if null toCompose
        <p>Error: Tried to compose zero items
    $else
        $forall element <- toCompose
            ^{element}
    |]