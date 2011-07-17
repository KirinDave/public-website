{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr, second)
import Data.Monoid (mempty, mconcat)

import Hakyll

topTitle = "<span>Case Study:</span><br> An Opinionated Man"
feedConfig = FeedConfiguration { feedTitle       = "dave's blog"
                               , feedDescription = "Unified topic feed for dave's blog."
                               , feedAuthorName  = "Dave Fayram"
                               , feedRoot        = "http://dave.fayr.am" }

main :: IO ()
main = hakyll $ do

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
          >>> makeEnvironment
          >>> applyTemplateCompiler "templates/post.html"
          >>> applyTemplateCompiler "templates/toplevel.html"
          >>> relativizeUrlsCompiler

    -- Render posts list
    match "posts.html" $ do 
      route idRoute
      create "posts.html" $ constA mempty
        >>> setTitle "All posts"
        >>> makeEnvironment
        >>> requireAllA "posts/*" addPostList
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/toplevel.html"
        >>> relativizeUrlsCompiler

    -- Index
    match "index.html" $ do
      route idRoute
      create "index.html" $ constA mempty
        >>> setTitle topTitle
        >>> makeEnvironment
        >>> requireAllA "posts/*" (second (arr $ take 3 . recentFirst) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/toplevel.html"
        >>> relativizeUrlsCompiler

    -- Feed
    match "feed.xml" $ do
      route idRoute
      create "feed.xml" $ 
        constA mempty >>> requireAllA "posts/*" renderXmlFeed        

    -- Read templates
    match "templates/*" $ compile templateCompiler
    
    -- Render and compress css
    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler
    
    -- Stage media
    match "media/*" $ do
      route   idRoute
      compile copyFileCompiler
    
    -- Stage images, make sure to truncate from pretty names.
    match "images/*" $ do
      route $ gsubRoute "images/" (const "img/")
      compile copyFileCompiler

        
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
              pageListCompiler recentFirst "templates/postitem.html"

-- This is for stuff my blog specifically expects.
makeEnvironment :: Compiler (Page String) (Page String)
makeEnvironment = arr $ updateFieldInto "title" "alttitle" stripTags 

-- Utilities that really would be useful in the standard lib:
updateFieldInto :: String -> String -> (String -> String) -> Page a -> Page a
updateFieldInto key newKey updater = 
  changeField newKey updater . copyField key newKey
  
renderXmlFeed :: Compiler (Page String, [Page String]) (Page String)
renderXmlFeed = second cleanTitles 
            >>> second (arr $ take 10 . recentFirst)
            >>> setFieldA result (renderRss feedConfig) 
            >>> (arr $ copyBodyFromField result)
              where cleanTitles = arr $ fmap (changeField "title" stripTags)
                    result      = "result"
                
                 
setTitle :: String -> Compiler (Page a) (Page a)
setTitle t = arr $ (setField "title" t)

