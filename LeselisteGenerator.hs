{-# LANGUAGE OverloadedStrings #-}

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Control.Monad (forM_)

import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8

import CSVParser

generateSite :: [[String]] -> H.Html
generateSite leseliste = H.docTypeHtml $ do
    H.head $ do
        H.title "Mathematische Leseliste"
    H.body $ do
        H.h1 "Leseliste"
        H.h2 "Gelesen"
        generateList $ filterItemsBy 4 "ja" leseliste               
        H.h2 "Teilweise gelesen"
        generateList $ filterItemsBy 4 "teilweise" leseliste
        H.h2 "Noch zu lesen"
        generateList $ filterItemsBy 4 "nein" leseliste    
        
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
        
main = do
    csvDataString <- fmap BS.Char8.unpack $ BS.readFile "Leseliste.csv"
    case parseCSV csvDataString of
        Left e -> do putStrLn "Error parsing input:"
                     print e
        Right d -> writeFile "Leseliste.html" ((renderHtml.generateSite) (tail d))