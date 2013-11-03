{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.User where

import Import
import Data.Maybe
import Data.Map hiding (null, map)
import Text.Shakespeare.Text

getAllUsersR :: Handler Html
getAllUsersR = do
    users <- runDB $ selectList [] [Asc UserIdent]
    let html = printAllUsers users
    renderer <- getUrlRenderParams
    return (html renderer)
        
printAllUsers users = [hamlet|
    $if null users
        <p>There are no users registered.
    $else
        <p>Overview of registered users:
        $forall Entity userID user <- users
            <li><a href=@{UserR userID}>#{userIdent user}</a>
        |]
        
getUserR :: UserId -> Handler Html
getUserR id = do
    user' <- runDB $ get id
    case user' of
        (Just user) -> do
            receiptsByUser <- runDB $ selectList [ReceiptPaidBy ==. id] []
            debtsOfUser <- getDebtsByUser id
            paidByUser <- getPaymentsMadeByUser id
            sentByUser <- getPaymentsReceivedByUser id
            rbuHtml <- printAllReceipts receiptsByUser (userIdent user)
            douHtml <- printAllDebts debtsOfUser (userIdent user)
            pbuHtml <- printAllPayments 
                ("Payments made by" :: Text) 
                ("No payments made by" :: Text)
                (userIdent user) 
                ("transferred" :: Text) 
                ("to" :: Text) 
                paidByUser
            sbuHtml <- printAllPayments
                ("Payments received by" :: Text) 
                ("No payments received by" :: Text)
                (userIdent user) 
                ("received" :: Text) 
                ("from" :: Text) 
                sentByUser
            composed <- composer [rbuHtml, douHtml, pbuHtml, sbuHtml]
            renderer <- getUrlRenderParams
            return (composed renderer)
        Nothing -> return [shamlet|User not found|]

getDebtsByUser = join receiptUserReceipt ReceiptUserUser
getPaymentsMadeByUser = join paymentTo PaymentFrom
getPaymentsReceivedByUser = join paymentFrom PaymentTo
       
printAllReceipts receipts paidBy = return [hamlet|
    $if null receipts
        <p>#{paidBy} has not entered any receipts into the system.
    $else
        <p>Overview of receipts created/paid by #{paidBy}:
        $forall Entity receiptId receipt <- receipts
            <li><a href=@{ReceiptR receiptId}>#{show receiptId}</a>
        |]

printAllDebts debts debtor = return [hamlet|
    $if null debts
        <p>#{debtor} is not a debtor for any receipts in our system
    $else
        <p>Overview of receipts for which #{debtor} is a debtor:
        $forall (_,(Entity receiptId _)) <- debts
            <li><a href=@{ReceiptR receiptId}>#{show receiptId}</a>
        |]

printAllPayments header empty user action direction payments = return [hamlet|
    $if null payments
        <p>#{empty} #{user}
    $else
        <p>#{header} #{user}
        $forall (Entity pid (Payment time from to amount), Entity tid tu) <- payments
            <p>Payment #{show pid} made at #{show time}, #{action} #{show amount} #{direction} <a href=@{UserR tid}>#{userIdent tu}</a>.
    |]
    
paymentsYetToReceive userId = do
    receipts <- runDB $ selectList [ReceiptPaidBy ==. userId] []
    joined <- joiner receipts
    converted <- test joined empty
    return receipts
    where
        joiner ((Entity rid r):rs) = do
            debts <- selectList [ReceiptUserReceipt ==. rid] []
            tail <- joiner rs
            return ((Entity rid r, debts):tail)
        test ((Entity rid r, users):xs) datamap = updater users (test xs datamap) where
            updater ((Entity ruid ru):rus) dm
                | member (receiptUserUser ru) dm = adjust (+avgDebt) (receiptUserUser ru) dm
                | otherwise = Data.Map.insert (receiptUserUser ru) avgDebt dm
            updater [] datamap = datamap
            avgDebt = div (receiptPaidTotal r) (length users)
        test [] datamap = datamap

    
composer toCompose = return [hamlet|
    $if null toCompose
        <p>Error: Tried to compose zero items
    $else
        $forall element <- toCompose
            ^{element}
    |]