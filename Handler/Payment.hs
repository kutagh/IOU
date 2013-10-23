{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Payment where

import Import

getPaymentsR :: Handler Html
getPaymentsR = do

    -- A many-to-many join
    records <- runDB $ do
        payments <- selectList [] [Desc PaymentTimestamp]
        users    <- selectList [] []
        return $ joinTables3 paymentFrom paymentTo payments users users

    error "getPaymentsR not implemented yet!"

postPaymentsR :: Handler Html
postPaymentsR = do
    error "postPaymentsR not implemented yet!"
