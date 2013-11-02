{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Receipt where

import Import

receiptForm :: Form Receipt
receiptForm = renderDivs $ Receipt
    <$> lift requireAuthId
    <*> areq intField "Total paid by you:" (Just 0)

receiptUserForm :: ReceiptId -> Form ReceiptUser
receiptUserForm receiptId = renderDivs $ ReceiptUser 
    <$> pure receiptId
    <*> areq (selectField users) "Debtor to add" Nothing
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
    return html
    
printAllReceipts receipts = do 
    mu <- maybeAuth
    widget <- addReceiptForm mu
    defaultLayout $ do [whamlet|
        $if null receipts
            <p>There are no receipts entered in the system
        $else
            <p>An overview of all receipts in the system:
            $forall Entity receiptID receipt <- receipts
                <p>
                    <a href=@{ReceiptR receiptID}>#{show receiptID}
        ^{widget}
        |]

addReceiptForm mu = case mu of
    Just _ -> do 
        (widget, enctype) <- generateFormPost receiptForm
        return [whamlet|
            <p>Add a new receipt:
                <form method=post enctype=#{enctype}>
                    ^{widget}
                    <button>Submit|]
    _ -> return [whamlet|<p>You need to be logged in to add a new receipt|]

getReceipts = do
    receipts <- runDB $ selectList [] [Asc ReceiptPaidBy] 
    return receipts

-- Add a new receipt
postAllReceiptsR :: Handler Html
postAllReceiptsR = do
    ((res, receiptWidget), enctype) <- runFormPost receiptForm
    case res of
        FormSuccess receipt -> do
            id <- runDB $ insert receipt
            let html = [hamlet|
                <p>Succesfully added the receipt. 
                <a href=@{ReceiptR id}>Click here to view the receipt.
                <a href=@{HomeR}>Click here to go home.
                |]
            renderer <- getUrlRenderParams
            return (html renderer)
        _ -> return [shamlet|Failed to add the receipt.|]

-- Display a receipt
getReceiptR :: ReceiptId -> Handler Html
getReceiptR receiptId = do
    receipt <- runDB $ get404 receiptId
    debtors <- getDebtsByReceipt receiptId
    renderer <- getUrlRenderParams
    (widget, enctype) <- generateFormPost (receiptUserForm receiptId)
    mu <- maybeAuth
    defaultLayout [whamlet|
        <section>
        Receipt #{show receiptId}:
            $with Receipt paidBy paidTotal <- receipt
                <p>Total cost: #{show paidTotal}
                $if null debtors
                    <p>There are no debtors yet for this receipt
                $else
                    <p>Debtors with a debt of #{show $ div paidTotal $ length debtors}:
                    $forall (_, Entity userId user) <- debtors
                        <li><a href=@{UserR userId}>#{show $ userIdent user}</a>
            <section>
            $maybe _ <- mu
                <form method=post enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
        |]
        
    
getDebtsByReceipt = join receiptUserUser ReceiptUserReceipt

-- Add a user to a receipt
postReceiptR :: ReceiptId -> Handler Html
postReceiptR receiptId = do
    ((res, widget), enctype) <- runFormPost $ receiptUserForm receiptId
    case res of
        FormSuccess ru -> do
            id <- runDB $ insert ru
            user <- runDB $ get404 $ receiptUserUser ru
            let html = [hamlet|
                <p>Succesfully added #{userIdent user} to the receipt.
                <a href=@{ReceiptR receiptId}>Click here to view the receipt.
                <a href=@{HomeR}>Click here to go home.
                |]
            renderer <- getUrlRenderParams
            return (html renderer)
        _ -> return [shamlet|Failed to add the user to the receipt.|]