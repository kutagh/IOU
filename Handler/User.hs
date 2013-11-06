{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.User where

import Import
import Data.Maybe
import Data.Map hiding (null, map, foldr)
import Data.Set hiding (null, map, foldr, member, empty)
import Text.Shakespeare.Text

-- Generate a list of all users registered in the system
getAllUsersR :: Handler Html
getAllUsersR = do
    users <- runDB $ selectList [] [Asc UserIdent]
    let html = printAllUsers users
    renderer <- getUrlRenderParams
    return (html renderer)

-- User page generation, showing the following elements:
-- Overview of receipts created and paid by the user
-- Overview of receipts for which said user is a debtor
-- Overview of payments made and received by said user
-- Overview of the outstanding debts and credits
getUserR :: UserId -> Handler Html
getUserR id = do
    user' <- runDB $ get id
    case user' of
        (Just user) -> do
            receiptsByUser <- runDB $ selectList [ReceiptPaidBy ==. id] []
            debtsOfUser <- getDebtsByUser id
            paidByUser <- getPaymentsMadeByUser id
            sentByUser <- getPaymentsReceivedByUser id
            yetToReceive <- paymentsYetToReceive id
            yetToMake <- paymentsYetToMake id
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
            ytrHtml <- printYet
                ("Payments yet to receive" :: Text)
                ("No payments yet to be received" :: Text)
                ("Expecting" :: Text)
                ("from" :: Text)
                (Data.Map.toList yetToReceive)
            ytmHtml <- printYet
                ("Payments yet to make" :: Text)
                ("No payments to be made" :: Text)
                ("Yet to pay" :: Text)
                ("to" :: Text)
                (Data.Map.toList yetToMake)
            composed <- 
                composer [rbuHtml, douHtml, pbuHtml, sbuHtml, ytrHtml, ytmHtml]
            renderer <- getUrlRenderParams
            return (composed renderer)
        Nothing -> return [shamlet|User not found|]

-- Convenience table join functions
getDebtsByUser = join receiptUserReceipt ReceiptUserUser
getPaymentsMadeByUser = join paymentTo PaymentFrom
getPaymentsReceivedByUser = join paymentFrom PaymentTo

-- Join several HTML elemens together.
composer toCompose = return [hamlet|
    $if null toCompose
        <p>Error: Tried to compose zero items
    $else
        $forall element <- toCompose
            ^{element}
    |]

-- Pretty print a list of users    
printAllUsers users = [hamlet|
    $if null users
        <p>There are no users registered.
    $else
        <p>Overview of registered users:
        $forall Entity userID user <- users
            <li><a href=@{UserR userID}>#{userIdent user}</a>
        |]

-- Pretty print a list of receipts
printAllReceipts receipts paidBy = return [hamlet|
    $if null receipts
        <p>#{paidBy} has not entered any receipts into the system.
    $else
        <p>Overview of receipts created/paid by #{paidBy}:
        $forall Entity receiptId receipt <- receipts
            <li><a href=@{ReceiptR receiptId}>Receipt of #{receiptPaidTotal receipt}</a>
        |]

-- Pretty print a list of debts        
printAllDebts debts debtor = return [hamlet|
    $if null debts
        <p>#{debtor} is not a debtor for any receipts in our system
    $else
        <p>Overview of receipts for which #{debtor} is a debtor:
        $forall (_,(Entity receiptId receipt)) <- debts
            <li><a href=@{ReceiptR receiptId}>Receipt of #{receiptPaidTotal receipt}</a>
        |]

-- Pretty print a list of payments
printAllPayments header empty user action direction payments = return [hamlet|
    $if null payments
        <p>#{empty} #{user}
    $else
        <p>#{header} #{user}
        $forall (Entity pid (Payment time from to amount), Entity tid tu) <- payments
            <p>Payment made at #{show time}, #{action} #{show amount} #{direction} <a href=@{UserR tid}>#{userIdent tu}</a>.
    |]

-- Pretty print a list of yet to receive/make payments
printYet header empty keyword direction datamap = do
    return [hamlet|
        $if null datamap
            <p>#{empty}
        $else
            <p>#{header}
            $forall (id, amount) <- datamap
                <li>#{keyword} #{amount} #{direction} <a href=@{UserR id}>this user</a>
        |]

-- Get all payments that yet have to be received, in other words:
-- The outstanding debts to you
paymentsYetToReceive userId = do
    receipts <- runDB $ selectList [ReceiptPaidBy ==. userId] []
    joined <- joiner receipts
    let 
        debtPerUser = processor joined empty
        users = Data.Set.toList (keysSet debtPerUser)
    paymentsPerUser <- getPayments userId users (return empty)
    let filtered = filterWithKey (\k _ -> k /= userId) $
            Data.Map.filter (>0) $ unionWith (-) debtPerUser $ 
            filterWithKey (\k _ -> member k debtPerUser) paymentsPerUser
    return filtered
    where
        -- Join a list of debtors to a receipt
        joiner ((Entity rid r):rs) = do
            debts <- runDB $ selectList [ReceiptUserReceipt ==. rid] []
            tail <- joiner rs
            return ((Entity rid r, debts):tail)
        joiner [] = return []
        -- Process a list of receipts and the debtors into a map of debt per user
        processor ((Entity rid r, users):xs) datamap = processor xs 
            (updater' users datamap) where
            updater' ((Entity ruid ru):rus) dm =
                updater' rus (updater (receiptUserUser ru) (+) avgDebt dm)
            updater' [] dm = dm
            avgDebt = div (receiptPaidTotal r) (1 + length users)
        processor [] datamap = datamap
        -- Get the payments from a certain userId
        getPayments id (userId:us) datamap = do
            tail <- getPayments id us datamap
            payments <- runDB $ selectList [PaymentTo ==. id, PaymentFrom ==. userId] []
            let result = updater' payments tail
            return result
            where
                updater' ((Entity pid p):ps) dm = 
                    updater' ps (updater userId (+) (paymentAmount p) dm)
                updater' [] dm = dm
        getPayments id [] dm = dm

-- Get a list of all payments yet to make, in other words:
-- Your outstanding debts to others
paymentsYetToMake userId = do
    receiptUsers <- runDB $ selectList [ReceiptUserUser ==. userId] []
    receipts <- joiner receiptUsers
    let 
        debtPerUser = processor receipts empty
        users = Data.Set.toList (keysSet debtPerUser)
    paymentsPerUser <- getPayments userId users (return empty)
    let filtered = filterWithKey (\k _ -> k /= userId) $ 
            Data.Map.filter (>0) $ unionWith (-) debtPerUser $ 
            filterWithKey (\k _ -> member k debtPerUser) paymentsPerUser
    return filtered
    where
        -- Join receipts with the total debt associated to them
        joiner ((Entity ruid ru):rus) = do
            receipt <- runDB $ getJust rid
            debtors <- runDB $ selectList [ReceiptUserReceipt ==. rid] []
            tail <- joiner rus
            return ((receipt, (div (receiptPaidTotal receipt) (1 + length debtors))):tail)
            where
                rid = receiptUserReceipt ru
        joiner [] = return []
        -- Process a list of receipts and associated debts into a map of debt to per user
        processor (x:xs) datamap = processor xs (updater' x datamap) where
            debtTo = receiptPaidBy $ fst x
            updater' (_, debt) = updater debtTo (+) debt
        processor [] datamap = datamap
        -- Get the payments to a certain userId
        getPayments id (userId:us) datamap = do
            tail <- getPayments id us datamap
            payments <- runDB $ selectList [PaymentTo ==. userId, PaymentFrom ==. id] []
            let result = updater' payments tail
            return result
            where
                updater' ((Entity pid p):ps) dm = 
                    updater' ps (updater userId (+) (paymentAmount p) dm)
                updater' [] dm = dm
        getPayments _ [] datamap = datamap

-- Updating function for Data.Map which tries to update a key with a value if 
-- it exists and otherwise inserts the key with said value.
updater key op value datamap
    | member key datamap = adjust ((op)value) key datamap
    | otherwise          = Data.Map.insert key value datamap