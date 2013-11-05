{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.User where

import Import
import Data.Maybe
import Data.Map hiding (null, map, foldr)
import Data.Set hiding (null, map, foldr, member, empty)
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
            composed <- composer [rbuHtml, douHtml, pbuHtml, sbuHtml, ytrHtml, ytmHtml]
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

printYet header empty keyword direction datamap = return [hamlet|
    $if null datamap
        <p>#{empty}
    $else
        <p>#{header}
        $forall (id, amount) <- datamap
            <li>#{keyword} #{amount} #{direction} <a href=@{UserR id}>#{show id}</a>
    |]
    
paymentsYetToReceive userId = do
    receipts <- runDB $ selectList [ReceiptPaidBy ==. userId] []
    joined <- joiner receipts
    let 
        debtPerUser = test joined empty
        users = Data.Set.toList (keysSet debtPerUser)
    paymentsPerUser <- getPayments userId users (return empty)
    let filtered = Data.Map.filter (>0) $ unionWith (-) debtPerUser $ filterWithKey (\k _ -> member k debtPerUser) paymentsPerUser
    return filtered
    where
        joiner ((Entity rid r):rs) = do
            debts <- runDB $ selectList [ReceiptUserReceipt ==. rid] []
            tail <- joiner rs
            return ((Entity rid r, debts):tail)
        joiner [] = return []
        test ((Entity rid r, users):xs) datamap = test xs (updater users datamap) where
            updater ((Entity ruid ru):rus) dm
                | member (receiptUserUser ru) dm = adjust (+avgDebt) uid (updater rus dm)
                | otherwise = Data.Map.insert uid avgDebt (updater rus dm)
                where uid = receiptUserUser ru
            updater [] dm = dm
            avgDebt = div (receiptPaidTotal r) (length users)
        test [] datamap = datamap
        getPayments id (userId:us) datamap = do
            tail <- getPayments id us datamap
            payments <- runDB $ selectList [PaymentTo ==. id, PaymentFrom ==. userId] []
            let result = updater payments tail
            return result
            where
                updater ((Entity pid p):ps) dm
                    | member userId dm = updater ps (adjust (+(paymentAmount p)) userId dm)
                    | otherwise = updater ps (Data.Map.insert userId (paymentAmount p) dm)
                updater [] dm = dm
        getPayments id [] dm = dm


paymentsYetToMake id = do
    receiptUsers <- runDB $ selectList [ReceiptUserUser ==. id] []
    receipts <- joiner receiptUsers
    let 
        debtPerUser = test receipts empty
        users = Data.Set.toList (keysSet debtPerUser)
    paymentsPerUser <- getPayments id users (return empty)
    let filtered = Data.Map.filter (>0) $ unionWith (-) debtPerUser $ filterWithKey (\k _ -> member k debtPerUser) paymentsPerUser
    return filtered
    where
        joiner ((Entity ruid ru):rus) = do
            receipt <- runDB $ getJust rid
            debtors <- runDB $ selectList [ReceiptUserReceipt ==. rid] []
            tail <- joiner rus
            return ((receipt, (div (receiptPaidTotal receipt) (length debtors))):tail)
            where
                rid = receiptUserReceipt ru
        joiner [] = return []
        test (x:xs) datamap = test xs (updater x datamap) where
            debtTo = receiptPaidBy $ fst x
            updater (receipt, debt) dm
                | member debtTo dm = adjust (+debt) debtTo dm
                | otherwise = Data.Map.insert debtTo debt dm
        test [] datamap = datamap
        getPayments id (userId:us) datamap = do
            tail <- getPayments id us datamap
            payments <- runDB $ selectList [PaymentTo ==. userId, PaymentFrom ==. id] []
            let result = updater payments tail
            return result
            where
                updater ((Entity pid p):ps) dm
                    | member userId dm = updater ps (adjust (+(paymentAmount p)) userId dm)
                    | otherwise = updater ps (Data.Map.insert userId (paymentAmount p) dm)
                updater [] dm = dm
        getPayments _ [] datamap = datamap
composer toCompose = return [hamlet|
    $if null toCompose
        <p>Error: Tried to compose zero items
    $else
        $forall element <- toCompose
            ^{element}
    |]