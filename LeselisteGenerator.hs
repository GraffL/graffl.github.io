{-# LANGUAGE OverloadedStrings #-}

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Control.Monad (forM_)

import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified System.IO.Encoding as UTF8.System

import CSVParser


generateBlogroll :: [[String]] -> H.Html
generateBlogroll liste = H.docTypeHtml $ do
    H.head $ do
        H.title "Mathematischer Blogroll"
        H.link H.! HA.type_ "text/css" H.! HA.rel "stylesheet" H.! HA.href "Leseliste.css" 
    H.body $ do
        H.h3 "Blogroll"
        H.ul $ forM_ (filterItemsBy 3 "Blog" liste) generateItem 


---------------------------------------------------------------

generateLeseliste :: [[String]] -> H.Html
generateLeseliste leseliste = H.docTypeHtml $ do
    H.head $ do
        H.title "Mathematische Leseliste"
        H.link H.! HA.type_ "text/css" H.! HA.rel "stylesheet" H.! HA.href "Leseliste.css" 
    H.body $ do
        H.h1 "Leseliste"
        H.div H.! HA.style "width:31%; float:left; padding:1%; border-right:1px dashed #777777;" $ do
            H.h2 "Gelesen"
            generateList $ filterItemsBy 4 "ja" leseliste  
        H.div H.! HA.style "width:31%; float:left; padding:1%; border-right:1px dashed #777777;" $ do
            H.h2 "Teilweise gelesen"
            generateList $ filterItemsBy 4 "teilweise" leseliste
        H.div H.! HA.style "width:31%; float:left; padding:1%;" $ do
            H.h2 "Noch zu lesen"
            generateList $ filterItemsBy 4 "nein" leseliste  
        H.hr H.! HA.style "clear:both;"
        
generateList :: [[String]] -> H.Html
generateList leseliste = do
        H.h3 "Paper"
        H.ul $ forM_ (filterItemsBy 3 "Paper" leseliste) generateItem 
        H.h3 "Buecher"
        H.ul $ forM_ (filterItemsBy 3 "Buch" leseliste) generateItem 
        
generateItem :: [String] -> H.Html
generateItem (_:sTitle:sType:sRead:sAuthors:sContent:sComment:sLink:sLinkComment:sTags:sISBN:sDOI:_) = do
    H.li $ do
        H.h4 $ H.toHtml sTitle
        H.em $ H.toHtml ("von " ++ sAuthors)
        H.p  $ H.toHtml sContent
        H.p  $ H.toHtml sComment
        if sLink == "" then "" 
                       else do  "("
                                H.a H.! HA.href (H.toValue sLink) $ H.toHtml sLink
                                ")"  
        if sISBN == "" then ""
                       else do  "ISBN: "
                                H.toHtml sISBN
        if sDOI == "" then ""
                       else do  "DOI: "
                                H.toHtml sDOI                                
        H.p $ do
            H.strong "Tags:"
            H.toHtml sTags

            
filterItemsBy :: Int -> String -> [[String]] -> [[String]]
filterItemsBy 3 s liste = filter (\(_:_:sType:_) -> sType == s) liste
filterItemsBy 4 s liste = filter (\(_:_:_:sRead:_) -> sRead == s) liste
            
------------------------------------------------------------------------      
        
main1 = do
    csvDataString <- readFile "Leseliste.csv"
    case parseCSV csvDataString of
        Left e -> do putStrLn "Error parsing input:"
                     print e
        Right d -> writeFile "Leseliste.html" ((UTF8.toString . renderHtml.generateLeseliste) (tail d))
        
main2 = do
    csvDataString <- readFile "Leseliste.csv"
    case parseCSV csvDataString of
        Left e -> do putStrLn "Error parsing input:"
                     print e
        Right d -> writeFile "Blogroll.html" ((UTF8.toString . renderHtml.generateBlogroll) (tail d))