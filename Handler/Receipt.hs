{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Receipt where

import Import

-- Form for creating a receipt
receiptForm :: Form Receipt
receiptForm = renderDivs $ Receipt
    <$> lift requireAuthId
    <*> areq intField "Total paid by you:" (Just 0)

-- Form for adding an user to a receipt
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
    receipts <- runDB $ selectList [] [Asc ReceiptPaidBy] 
    html <- printAllReceipts receipts
    return html

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
    creator <- runDB $ get404 $ receiptPaidBy receipt
    debtors <- getDebtsByReceipt receiptId
    renderer <- getUrlRenderParams
    mu <- maybeAuth
    (widget, enctype) <- generateFormPost (receiptUserForm receiptId)
    defaultLayout [whamlet|
        <section>
        <p>Receipt creator: 
            <p>
                <a href=@{UserR (receiptPaidBy receipt)}>#{show (userIdent creator)}
            $with Receipt paidBy paidTotal <- receipt
                <p>Total cost: #{show paidTotal}
                $if null debtors
                    <p>There are no debtors yet for this receipt
                $else
                    <p>Debtors with a debt of #{show $ div paidTotal $ 1 + length debtors} each:
                    $forall (_, Entity userId user) <- debtors
                        <li><a href=@{UserR userId}>#{show $ userIdent user}</a>
            <section>
            $maybe Entity uid _ <- mu
                $if uid == (receiptPaidBy receipt)
                    <form method=post enctype=#{enctype}>
                        ^{widget}
                        <button>Submit
        |]

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
    
-- Pretty print a list of receipts    
printAllReceipts receipts = do 
    mu <- maybeAuth
    widget <- addReceiptForm mu
    defaultLayout $ do [whamlet|
        $if null receipts
            <p>There are no receipts entered in the system
        $else
            <p>An overview of all receipts in the system:
            $forall Entity receiptID receipt <- receipts
                <li>
                    <a href=@{ReceiptR receiptID}>Receipt
        ^{widget}
        |]

-- 'hack' to avoid a required log in to see the overview of receipts
addReceiptForm mu = case mu of
    Just _ -> do 
        (widget, enctype) <- generateFormPost receiptForm
        return [whamlet|
            <p>Add a new receipt:
                <form method=post enctype=#{enctype}>
                    ^{widget}
                    <button>Submit|]
    _ -> return [whamlet|<p>You need to be logged in to add a new receipt|]