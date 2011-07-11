{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Data.Monoid (mempty, mconcat)

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    
    match "media/*" $ do
      route   idRoute
      compile copyFileCompiler
    
    match "images/*" $ do
      route   (gsubRoute "images/" (const "img/"))
      compile copyFileCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
          >>> applyTemplateCompiler "templates/post.html"
          >>> applyTemplateCompiler "templates/toplevel.html"
          >>> relativizeUrlsCompiler

    -- Render posts list
    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "All posts")
        >>> requireAllA "posts/*" addPostList2
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/toplevel.html"
        >>> relativizeUrlsCompiler

    -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "<span>Case Study:</span><br>An Opinionated Man")
        >>> requireAllA "posts/*" (id *** arr (take 3 . recentFirst) >>> addPostList2)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/toplevel.html"
        >>> relativizeUrlsCompiler

    -- Read templates
    match "templates/*" $ compile templateCompiler

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . sortByBaseName)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody
        
addPostList2 :: Compiler (Page String, [Page String]) (Page String)
addPostList2 = setFieldA "posts" $
               pageListCompiler recentFirst "templates/postitem.html"
               
