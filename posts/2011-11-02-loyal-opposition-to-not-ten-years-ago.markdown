---
title: The Unbearable Lightness of Modern Typed Programming
date: 11-02-2011
tags: programming, static, dynamic, types
author: Dave Fayram
description: Static typing need not be an epic stone around your neck.
---

Recently I've been seeing a lot more debate online about the relative
merits of static and dynamic typing for the "average programmer". I
think it's great that we're having this conversation, but I've been
picking up on a subtle point I wanted to briefly talk about. It
started when I read [this post by Jeremy Askhenas](https://mail.mozilla.org/pipermail/es-discuss/2011-November/017872.html). It's
mostly Jeremy asking for a better tomorrow. It's a noble rallying cry, but I
can't help but note this sentiment:

<blockquote>
If I had my druthers, JS.next would generally embrace the spirit of
JavaScript's dynamism (and freedom), and try to push those aspects further
-- with better introspection and more flexibility -- instead of
compromising with more restrictive static languages and adding lock-down
keywords so that some programmers might "feel safer".
</blockquote>

This echos
[a tweet by @DHH, another strong opponent of static typing](http://twitter.com/#!/dhh/status/123773621771583488),
which says:

<blockquote>
I've never had that as a real problem in the wild. Passing "the wrong
thing" to a method is a fantasy boogeyman of the
explicits.</blockquote>

Now, maybe 5 years ago I might have read this and nodded. Before I
took my excursion into functional programming, I just sort of assumed
that all static typing is like C++ (circa 1999) and Java 1.2: agony on stilts. It's
all keywords and nonsense and repetitive cruft, right? Turns out I was
wrong, and these guys have a very dated view of what modern static
typing is.

When I sit down and write code in Haskell (something I don't get to do
very often), I am not suddenly burdened with the unbearable weight of
types. If anything, they're there as something I can lean on to help
me reason about my code. When I refactor, the types are likewise there
to help (something that only a battery of cleverly written unit tests
can give you in dynamic land).

## Modern Type Inference, With & Without Explicitly Written Types

As an example, I've excerpted a bit of code from a library I use to
manage my chef 0.9 environment. I have to switch between several
configurations and amazon keys throughout my work day. This code shows
a nice string ("ubuntu@production" or "none") explaining what my knife.rb says about
which chef environment I am currently pointed at:

~~~~~~{.haskell}
import System.IO
import Data.List
import Data.Maybe
import Control.Monad
import HSH (glob)

isQuote x = x == '"' || x == '\''
isntQuote = not . isQuote
lineWith pat strs = find (isInfixOf pat) strs

fileLines floc = do
  cH <- openFile floc ReadMode
  lines `fmap` hGetContents cH

knifeLines = do
  g <- glob "~/.chef/knife.rb"
  case g of
    a:_ -> fileLines a
    _   -> return []

getQuotedValue line =
  takeWhile isntQuote startOfValue
    where startOfValue = drop 1 $ dropWhile isntQuote line

userString user env = user ++ "@" ++ env

main = do
  kl   <- knifeLines
  let klWith = (`lineWith` kl)
      qname  = getQuotedValue `fmap` klWith "node_name"
      qenv   = getQuotedValue `fmap` klWith "builder_cluster"
      name   = liftM2 userString qname qenv
  putStrLn $ fromMaybe "none" name
~~~~~~

This code, as written, has no explicitly written types. The types of a value are
mentioned in text only once, on the final line. This code compiles and
works. Now, it's a little unrealistic to write Haskell this way. Not
because it's terribly difficult (it is not), but rather because the
way we often start thinking about writing a module is by thinking
about what we want to write...

~~~~~~{.haskell}
lineWith       :: String -> [String] -> Maybe String
fileLines      :: FilePath -> IO [String]
knifeLines     :: IO [String]
getQuotedValue :: String -> String
~~~~~~

... and then start writing from there. This is analogous to a
lightweight test-first process. We can lay out these type annotations
and then sketch the code in underneath them. Like in the dynamic
programming world, it's fairly easy not to make the mistake of passing
the wrong item to a function. Unlike the dynamic programming world, we
have the compiler watching out back and making sure we don't get
distracted and commit that subtle error.

Even better, later we can use those type signatures to help write
randomly generated tests that help us catch the edge cases we
*weren't* thinking of. So not only do the type signatures give us a
nice starting point for writing a unit of code, but they also can go
above and beyond later in the code life cycle when we start to ask hard
questions about how durable our software is.

## They Call Him, "The Straw Man"

This is what a modern type system offers for you. You will spend time
interacting with it, but more as a partner and a tool for letting you
write more expressive code (the only reason we could use liftM2 was
because we had type inference figuring out that qname and qenv are
Maybe's). And you don't need to go all the way into the Haskell world to
get these benefits; there are several languages for a variety of
runtimes that give you modern, powerful type systems that act as your
ally instead of your taskmaster. Even the venerable C++ is [adding limited type inference](http://en.wikipedia.org/wiki/C%2B%2B11#Type_inference).

So let's stop spreading the misconception that static typing is all
about "keywords" and "const" circa 1999. Sure, some statically typed
languages use these constructs. But, that's not really related to
static typing so much as their heritage. Let's stop straw-manning a very promising
branch of the programming language family that has come a long way
over the past 20 years.

And of course, I'd like to thank the folks who helped suggest how to
clarify my code on Freenode. In particular: shachaf, luite, c_wraith, Nimatek and grey_wolf.
