{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.User where

import Import

getAllUsersR :: Handler Html
getAllUsersR = do
    users <- getUsers
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

getUsers = do
    users <- runDB $ selectList [] [Asc UserIdent]
    return users
        
getUserR :: UserId -> Handler Html
getUserR userId = do
    error "getUserR not implemented yet!"
