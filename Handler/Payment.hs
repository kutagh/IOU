{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Payment where

import Import

paymentForm = renderDivs $ Payment
    <$> lift (liftIO getCurrentTime)
    <*> lift requireAuthId
    <*> areq (selectField users) "Pay to" Nothing
    <*> areq intField "Amount" (Just 0)
    where
        users = do
            entities <- runDB (selectList [] [Asc UserIdent])
            optionsPairs $ map (\user -> ( userIdent $ entityVal user
                                         , entityKey user             )) entities

getPaymentsR :: Handler Html
getPaymentsR = do
    -- A many-to-many join
    records <- runDB $ do
        payments <- selectList [] [Desc PaymentTimestamp]
        users    <- selectList [] []
        return $ joinTables3 paymentFrom paymentTo payments users users
    html <- printPayments records
    defaultLayout html

printPayments payments = do
    mu <- maybeAuth
    widget <- addPaymentForm mu
    return [whamlet|
        $if null payments
            <p>There are no payments entered in our system.
        $else
            $forall (Entity pid (Payment time from to amount), Entity fid fu, Entity tid tu) <- payments
                <p>Payment #{show pid} made at #{show time}, transferred #{show amount} from <a href=@{UserR fid}>#{userIdent fu}</a> to <a href=@{UserR tid}>#{userIdent tu}</a>.
        ^{widget}
        |]

addPaymentForm mu = case mu of
    Just _ -> do
        (widget, enctype) <- generateFormPost paymentForm
        return [whamlet|
            <p>Add a new payment:
                <form method=post enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]
    _ -> return [whamlet|<p>You need to be logged in to add a new payment|]
        
postPaymentsR :: Handler Html
postPaymentsR = do
    ((res, widget), enctype) <- runFormPost paymentForm
    case res of
        FormSuccess payment -> do
            id <- runDB $ insert payment
            let html = [hamlet|
                <p>Succesfully added the payment.
                <a href=@{HomeR}>Click here to go home.
                |]
            renderer <- getUrlRenderParams
            return (html renderer)
        _ -> return [shamlet|Failed to add the payment.|]
        