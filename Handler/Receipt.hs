{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Receipt where

import Import

receiptForm :: Form Receipt
receiptForm = renderDivs $ error "receiptForm not implemented yet!"

receiptUserForm :: ReceiptId -> Form ReceiptUser
receiptUserForm receiptId = renderDivs $
    error "receiptUserForm not implemented yet!"
    where
        -- Retrieve a list of users from the database that we can use
        -- to populate a list selection box.
        users = do
            entities <- runDB (selectList [] [Asc UserIdent])
            optionsPairs $ map (\user -> ( userIdent $ entityVal user
                                         , entityKey user             )) entities

-- List all receipts
getAllReceiptsR :: Handler Html
getAllReceiptsR = do
    receipts <- getReceipts
    html <- printAllReceipts receipts
    renderer <- getUrlRenderParams
    return (html renderer)
    
printAllReceipts receipts = return [hamlet|
    $if null receipts
        <p>There are no receipts entered in the system
    $else
        <p>An overview of all receipts in the system:
        $forall Entity receiptID receipt <- receipts
            <a href=@{ReceiptR receiptID}>#{show receipt}
    |]
        
getReceipts = do
    receipts <- runDB $ selectList [] [Asc ReceiptPaidBy] 
    return receipts

-- Add a new receipt
postAllReceiptsR :: Handler Html
postAllReceiptsR = do
    error "postAllReceiptsR not implemented yet!"

-- Display a receipt
getReceiptR :: ReceiptId -> Handler Html
getReceiptR receiptId = do
    receipt <- runDB $ get receiptId
    debtors <- getDebtsByReceipt receiptId
    renderer <- getUrlRenderParams
    case receipt of
        Just receipt' -> return ([hamlet|
                Receipt #{show receiptId}:
                    $with Receipt paidBy paidTotal <- receipt'
                        <p>Total cost: #{show paidTotal}
                        <p>Debtors with a debt of #{show $ div paidTotal $ length debtors}:
                        $forall (_, Entity userId user) <- debtors
                            <li><a href=@{UserR userId}>#{show $ userIdent user}</a>
                |] renderer)
        Nothing -> return [shamlet|Receipt ID not found|]
        
    
getDebtsByReceipt = join receiptUserUser ReceiptUserReceipt

-- Add a user to a receipt
postReceiptR :: ReceiptId -> Handler Html
postReceiptR receiptId = do
    error "postReceiptR not implemented yet!"
