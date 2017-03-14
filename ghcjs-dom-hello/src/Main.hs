module Main (
    main
) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)
import Control.Concurrent (threadDelay)

import GHCJS.DOM (run, syncPoint, currentDocument)
import GHCJS.DOM.Document (getBody, getHead, createElement, createTextNode)
import GHCJS.DOM.Element (setInnerHTML, setAttribute, removeAttribute)
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.EventM (on, mouseClientXY)
import qualified GHCJS.DOM.Document as D (click)
import qualified GHCJS.DOM.Element as E (click)

import Data.IORef

-- 初期値i1, i2で、隣り合う二数の比がax^2+bx+c=0のxについての解に収束するような数列のリストを作る。
makeArr :: Rational -> Rational -> Rational -> Rational -> Rational -> [Rational]
makeArr a b c i1 i2 =
    let 
        arr = i1 : i2 : zipWith (+) (map (\x -> x * (-1) * (c / a)) arr) (map (\x -> x * (-1) * (b / a)) (tail arr))
    in 
        arr

addButton modify_func = run 3708 $ do
    width <- newIORef 100
    Just doc <- currentDocument
    Just body <- getBody doc
    Just wrapper <- createElement doc (Just "div")
    setAttribute wrapper "class" "btn_wrapper"
    Just btn <- createElement doc (Just "input")
    setAttribute btn "class" "btn"
    setAttribute btn "type" "button"
    setAttribute btn "value" "Click me!"
    tmp <- readIORef width
    setAttribute btn "style" $ "width: " ++ (show tmp) ++ "px"
    
    appendChild wrapper (Just btn)
    appendChild body (Just wrapper)
    
    on btn E.click $ liftIO $ do
        setAttribute btn "value" "Yeah!!!!"
        setAttribute btn "disabled" "disabled"
        modifyIORef width modify_func
        tmp <- readIORef width
        setAttribute btn "style" $ "width: " ++ (show tmp) ++ "px"
        threadDelay 1000
        setAttribute btn "value" "Click me!"
        removeAttribute btn "disabled"
    return ()

addHr = do
    Just doc <- currentDocument
    Just body <- getBody doc
    Just hr <- createElement doc (Just "hr")
    appendChild body (Just hr)

main = do
    addButton (+10)
    addButton (*1.1)
    addHr
    
    Just doc <- currentDocument
    Just head <- getHead doc
    Just body <- getBody doc
    
    -- style
    Just style <- createElement doc (Just "style")
    setInnerHTML style (Just "\
\.btn_wrapper {\
\    text-align: center;\
\}\
\hr {\
\    border: 0;\
\    height: 1px;\
\    background: #333;\
\    background-image: -webkit-linear-gradient(left, #ccc, #333, #ccc);\
\    background-image:    -moz-linear-gradient(left, #ccc, #333, #ccc);\
\    background-image:     -ms-linear-gradient(left, #ccc, #333, #ccc);\
\    background-image:      -o-linear-gradient(left, #ccc, #333, #ccc);\
\}")
    appendChild head (Just style)
    
    -- wrapper
    Just wrapper <- createElement doc (Just "div")
    setAttribute wrapper "class" "kazu_wrapper"
    appendChild body (Just wrapper)
    
    -- input
    Just num_input_1 <- createElement doc (Just "input")
    setAttribute num_input_1 "type" "text"
    appendChild wrapper (Just num_input_1)
    
    Just num_input_2 <- createElement doc (Just "input")
    setAttribute num_input_2 "type" "text"
    appendChild wrapper (Just num_input_2)

    Just num_input_3 <- createElement doc (Just "input")
    setAttribute num_input_3 "type" "text"
    appendChild wrapper (Just num_input_3)
    
    -- br
    
    Just br <- createElement doc (Just "br")
    appendChild wrapper (Just br)
    
    -- button
    
    v1 <- newIORef 0
    Just btn1 <- createElement doc (Just "input")
    setAttribute btn1 "type" "button"
    appendChild wrapper (Just btn1)
    on btn1 E.click $ liftIO $ do
        modifyIORef v1 (+1)

    v2 <- newIORef 0
    Just btn2 <- createElement doc (Just "input")
    setAttribute btn2 "type" "button"
    appendChild wrapper (Just btn2)
    on btn2 E.click $ liftIO $ do
        modifyIORef v2 (+1)
    
    v3 <- newIORef 0
    Just btn3 <- createElement doc (Just "input")
    setAttribute btn3 "type" "button"
    appendChild wrapper (Just btn3)
    on btn3 E.click $ liftIO $ do
        modifyIORef v3 (+1)
    
    Just btn4 <- createElement doc (Just "input")
    setAttribute btn4 "type" "button"
    appendChild wrapper (Just btn4)
    on btn4 E.click $ liftIO $ do
        tmp1 <- readIORef v1
        tmp2 <- readIORef v2
        tmp3 <- readIORef v3
        setInnerHTML wrapper $ Just $ show $ take 100 $ makeArr tmp1 tmp2 tmp3 1 1
    
    return ()