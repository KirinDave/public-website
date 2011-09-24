{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr, second)
import Data.Monoid (mempty, mconcat)
import Data.Maybe (fromMaybe)

import Hakyll

topTitle = "<span>CASE STUDY:</span><br> An Opinionated Man"
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
        >>> requireAllA "posts/*" procureProgrammingPosts
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/toplevel.html"
        >>> relativizeUrlsCompiler

    -- Feed
    match "feed.xml" $ route idRoute
    create "feed.xml" $
      requireAll_ "posts/*"
        >>> mapCompiler (arr $ changeField "title" stripTags)
        >>> renderRss feedConfig

    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Render and compress css
    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match "css/columnal/*" $ do
      route idRoute
      compile compressCssCompiler

    -- Stage media
    match "media/*" $ do
      route   idRoute
      compile copyFileCompiler

    -- Stage images, make sure to truncate from pretty names.
    match "images/*" $ do
      route $ gsubRoute "images/" (const "img/")
      compile copyFileCompiler


--
-- -- Stuff for showing recent topic posts.
--

postsForTags :: [String] -> [Page String] -> [Page String]
postsForTags tags pages =
  let tagSet  = (tagsMap . readTags) pages
      pagesIn = fromMaybe [] . (flip lookup) tagSet  in
    tags >>= pagesIn

postsForTagsA :: [String] -> Compiler (Page String, [Page String]) (Page String, [Page String])
postsForTagsA tags = second (arr $ recentFirst . postsForTags tags)


addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = renderPageListInto "posts" "templates/postitem.html"

procureProgrammingPosts = postsForTagsA ["programming"] >>>
                          second (arr $ take 5) >>>
                          renderPageListInto "engineeringPosts" "templates/postitem.html"

-- This is for stuff my blog specifically expects.
makeEnvironment :: Compiler (Page String) (Page String)
makeEnvironment = arr $ updateFieldInto "title" "alttitle" stripTags

-- Utilities that really would be useful in the standard lib:
updateFieldInto :: String -> String -> (String -> String) -> Page a -> Page a
updateFieldInto key newKey updater =
  changeField newKey updater . copyField key newKey

setTitle :: String -> Compiler (Page a) (Page a)
setTitle t = arr $ (setField "title" t)

renderPageListInto :: String -> Identifier Template -> Compiler (Page String, [Page String]) (Page String)
renderPageListInto fieldName listTemplate = setFieldA fieldName $
                                              pageListCompiler recentFirst listTemplate
