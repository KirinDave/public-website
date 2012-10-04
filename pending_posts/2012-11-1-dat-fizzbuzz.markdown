---
title: FizzBuzz, Out Of The Functional Jungle
date: 10-1-2012
tags: programming, haskell, fizzbuzz, mastery
author: Dave Fayram
description: Finding interesting things to say even about Fizzbuzz.
---

## Fizzbuzz, Interviews, And Overthinking.

There is sort of a running meme in programming culture that
[programmers cannot "program"](http://www.codinghorror.com/blog/2007/02/why-cant-programmers-program.html),
meaning that lots of people who are in the software industry making a
living as software engineers actually are not very proficient at
programing outside of very narrow specialties. So you hear a lot about
dreadful intervew processes that companies resort to to try and find
the best programmers. And generally, there isn't much thought given to
these problems, which is a shame sometimes.

In Jeff's article (referencing
[Reginald's advice about not overthinking Fizzbuzz](http://weblog.raganwald.com/2007/01/dont-overthink-fizzbuzz.html)),
we're basically told that Fizzbuzz is just a simple thing and even if
we can improve upon the pattern, we shouldn't. Its purpose is to weed
out people who don't have basic proficiency, and that's it. It's just effing
FizzBuzz, after all. Just a few days before I wrote this, I found a clever fellow named
c_wraith on Freenode#haskell who had a really insightful
implementation I'd like to share with you. It reminded me how valuable it is to
occasionally sit back and remind ourselves that even the simplest
specs can be koans that offer much more than they appear to.



## Fizz Fi Fo Buzz

Let's review Fizzbuzz really quick. It is originally defined as:

> Write a program that prints the numbers from 1 to 100. But for multiples of three print “Fizz” instead of the number and for the multiples of five print “Buzz”. For numbers which are multiples of both three and five print “FizzBuzz”.

So let's start with the classic implementation of Fizzbuzz:

~~~~~~{.c}
#include <stdio.h>

int main (void)
{
    int i;
    for (i = 1; i <= 100; i++)
    {
        if (!(i % 15))
            printf ("FizzBuzz");
        else if (!(i % 3))
            printf ("Fizz");
        else if (!(i % 5))
            printf ("Buzz");
        else
            printf ("%d", i);

        printf("\n");
    }
    return 0;
}
~~~~~~

And in Python we can do a bit better:

~~~~~~{.python}
for i in xrange(1, 101):
    if i % 15 == 0:
        print "FizzBuzz"
    elif i % 3 == 0:
        print "Fizz"
    elif i % 5 == 0:
        print "Buzz"
    else:
        print i
~~~~~~

<br /><br />

Simple right? And simple it should be... *But*. In the so-called "real
world" of software engineering, you'd almost never actually see code
like this. Because _in the real world of "production code", code has
to be extensible and maintainable._ So maybe it's just
FizzBuzz today, but then a client says, "It'd be amazing if we could
point out multiples of 7 with 'Baz' as well!"

Let's change that in Python then...

~~~~~{.python}
for i in xrange(1, 101):
    if i % 105 == 0: # Can't reach, but for completeness.
        print "FizzBuzzBazz"
    elif i % 35 == 0:
        print "BuzzBazz"
    elif i % 21 == 0:
        print "FizzBazz"
    elif i % 15 == 0:
        print "FizzBuzz"
    elif i % 3 == 0:
        print "Fizz"
    elif i % 5 == 0:
        print "Buzz"
    elif i % 7 == 0:
        print "Bazz"
    else:
        print i
~~~~~~

And here we pant, wipe the metaphorical sweat from our brows, and say
"Wow, that was awful. That was terrible! I was doing math in my head
and I'm confused what order I should be considering my conditional in
and..." In mid-thought, the client pops his head in and says, "Oh, and
could 11's be labeled with a Boo?"

The problem is that the structure of this solution is stupid. Stupid
to the point where for anything beyond the most basic uses it's
unacceptable. Production code (and I say this with a wave of my hand)
can't behave this way. I've actually used Fizzbuzz in an interview and
had someone give this simple answer, and I keep asking them to add
more numbers until they show me how to make the code extensible.

FizzBuzzBazz is an even more subtle problem than most developers
realize at the outset. What trips up lots of novice programmers is
that the control flow requires understanding what you've previously
done in order to make a final decision. It's very easy to get this
subtly wrong:


~~~~~~{.c}
// This version never outputs the "fizzbuzz" case even though it looks
// like it does.
void fizzbuzz0(int i) {
  if (!(i % 3)) {
    printf ("Fizz");
  }
  else if (!(i % 5)) {
    printf ("Buzz");
  }
  else if (!(i % 15)) {
    printf ("FizzBuzz");
  }
  else {
    printf ("%d", i);
  }
  printf("\n");
}

// Thanks to aristid for these examples
// this solution goes through all options with independent if
// statements, but fails to handle the fallback correctly
void fizzbuzz1(int i) {
  if (i % 3 == 0) {
    printf("fizz")
  }
  if (i % 5 == 0) {
    printf("buzz");
  }
  if (WHAT_CAN_WE_DO) {// What do you do here? You have to know which
                      // branches you took in the past.
    printf("%d");
  }
  printf("\n");
}

// this solution goes through all options alternatively, but fails to handle the fizz AND buzz condition
void fizzbuzz2(int i) {
  if (i % 3 == 0)
    printf("fizz\n");
  else if (i % 5 == 0)
    printf("buzz\n");
  else
    printf("%d\n", i);
}
~~~~~~

What we need here is a better structure to capture the control
flow. Just using naive conditionals will explode combinatorically, and
is error prone.

## A Better Model

At [CrowdFlower](http://crowdflower.com) I interviewed Rubyists, and
when I got to do the technical portion of the interview I usually
asked for simple programs and then expanded on them. I started playing
the add-one-more game with one particularly promising interviewee and
they got wise to my pattern and whipped this up:

~~~~~~~{.ruby}
class FizzBuzz

  def initialize
    @printed  = nil
    @printers = [ lambda {|x| print (@printed = "Fizz") if x % 3 == 0},
                  lambda {|x| print (@printed = "Buzz") if x % 5 == 0},
                  lambda {|x| print (@printed = "Bazz") if x % 7 == 0} ]
  end

  def for_num(i)
    @printed = nil
    @printers.each { |l| l.call(i) }
    print i unless @printed
    puts # Newline
  end

  def run(x = 100)
    (1..x).to_a.each { |i| for_num(i) }
  end
end

# Add option to count to values > 100 since 3 * 5 * 7 > 100
FizzBuzz.new.run(ARGV.first || 100)
~~~~~~~

This took him about 5 minutes and secured my recommendation for a
hire. This kind of coding is exceptionally hard to do in an interview
setting, and I was very impressed. "And now you can add as many prime
factors as you want without worrying." he stated proudly. "It's not
even that unreadable, even if it is a lot slower."

Which is true. And for an interview this is way above and beyond the
call. *But...* We've spent a lot of time at a very low level of
abstraction. Fizzbuzz/bazz as formulated really doesn't care how you
implement the control flow, it has a set of rule that add up in a way
reminicent of Pascal's Triangle. This version solves the problem, but
has a lot of machinery around it to make that happen.

This version is only obvious in its operation to programmers
experienced in both Ruby and general software engineering. It would
probably be dismissed as "overly complex" by younger programmers and
outright impenetrable to novices. This seems strange. FizzBuzz is
supposed to be simple, right?

## The Bones of FizzBuzz

What this really should be making us--as we refine this trivial
function into some sort of crazy lambda-ized "production-ready" piece
of code--is, "What is the actual logic of FizzBuzz?" It seems like
we're doing an awful lot of work defining the control flow and not a
lot of work talking about the the trivial algorithm itself. It's not even
overthinking a useless problem, FizzBuzz is made up of several common operations:

1. Iterate over a group of entities.
2. Accumulate data about that group.
3. Provide a sane alterantive if no data is available.
4. Produce output that a human can read.

There are elements of FizzBuzz's logic in nearly every program you
interact with daily. Things like UI, logging, HTTP request processing,
graphics processing, and embedded systems _all_ do things like this
all the time. So it seems sort of crazy that most languages actually
don't do a very good job of capturing these patterns at a higher
level. Indeed, many software engineers don't even have names or
definitions for these patterns. It'd be pretty useful to be able to
name and perhaps even reuse these patterns in other contexts.

## Math To The Resuce

It turns out that mathemeticians actually have noticed several of
these patterns before in their own definitions of algorithms, and when
[Category Theory](http://en.wikipedia.org/wiki/Category_theory) came
into existence a lot of these patterns got categorized and given very
general definitions. Enthusiastic Haskell programmers have dutifully
created a system that can actually leverage a lot of these constructs
in a limited fashion.

A lot of people dismiss Category Theory as a programmer's utility
because it's so broad and fairly obscure, and it's difficult to know
what's useful. But I've been off in the Haskell Jungle, and I'd like
to point out two elementary and useful concepts for your
consideration. Bear with me as I define these. It'll pay off in just a
few more paragraphs.


#### _Monoids_

Monoids generalize over anything that can be "added"
associatively without breaking and that have an "identity" value.
Fancy talk meaning anything that can be sanely added roughly the way integers
can. Integers form a monoid with (+, 0). You may recall that the
associative rule says that: `a + (b + c) = (a + b) + c`, and `a + 0 =
0`. It turns out a lot of things programmers work with on a daily basis
actually form monoids, including arrays and strings.

So strings form a monoid where the "addition" is string concatenation
and the empty string is the identity value. Hey, that's exactly what
we're doing with "Fizz", "Buzz", and more. The associative property
holds: `"hello " + ("wor" + "ld") = ("hello " + "wor") + "ld"`, and
`"hello world" + "" = "hello world"` just like we'd like.

In Haskell, the append operation is called `mappend` and the identity
value is called `mempty` and you can define anything to be a monoid in so long as you can sanely define these operations without breaking the rules.

#### _Optional Values (Maybe)_

I've [blogged about Maybe and how everyone already uses
it](http://dave.fayr.am/posts/2011-10-4-rubyists-already-use-monadic-patterns.html). It
turns out that `Maybe a` also forms a monoid! A quick refresher, a `Maybe` is
either `Just thing` or `Nothing`. Either we have an empty value or a
wrapped value. The trick here is that if the enclosed `thing`'s type forms a monoid,
then `Maybe thing` forms a monoid! Unless you're familair with
monoids, this may be tricky, so let's take a moment to consider
exactly how we define this with a list of possible values for
`mappend`:

* `mappend(Just "hello ", Just "world") = Just "hello world"`
* `mappend(Just "hello ", Nothing)      = Just "hello "`
* `mappend(Nothing, Just "world")       = Just "hello "`
* `mappend(Nothing, Nothing)            = Nothing`

So we `mappend` the inner value if it is a `Just a`, and if it's a `Nothing` we
just treat it like an empty string. The actual Haskell code isn't
important, but you can
[read it if you want](http://www.haskell.org/ghc/docs/7.4.1/html/libraries/base/src/Data-Monoid.html).

So let's *finally* get to that Haskell implementation of c_wraith's that
I promised. If you're unfamiliar with Haskell, don't worry. I'll explain:

~~~~~~~~{.haskell}
{-# LANGUAGE MonadComprehensions #-}

module Main where
import Data.Monoid (mappend)
import Data.Maybe (fromMaybe, listToMaybe, maybe)
import System.Environment (getArgs)


fizzbuzz i = fromMaybe (show i) $ mappend ["fizz" | i `rem` 3 == 0]
                                          ["buzz" | i `rem` 5 == 0]

-- mapM_ is our iterator, putStrLn writes to console.
main = mapM_ putStrLn [ fizzbuzz i | i <- [1..100] ]
~~~~~~~~

The magic is in the definition of fizzbuzz, so I'll break it down:

1. First, we use `fromMaybe (show i)`. This function helps us catch the default case where neither "fizz" nor "buzz" apply, by catching the empty value and turning it into our default value.  &nbsp;`fromMaybe` takes a default value and a Maybe of the same type. If the Maybe is None, then it
gives the default value (in this case, the string representation of
our number).
2. Next we `mappend` two append two monoid values. Because we used `fromMaybe` earlier
the compiler infers this should be a `Maybe String`. We defined how
this is a monoid above. If it does become a `Nothing`, then our fromMaybe will catch it and turn it into the string representation of our number.
3. We use the [`MonadComprehensions`](http://hackage.haskell.org/trac/ghc/wiki/MonadComprehensions) syntactic sugar to define our
`Maybe String` values. Each `[value | condition]` block returns a `Just value` if the `condition` is true, or
otherwise false. There are other ways we could have written this code
(most naturally with a helper function), but monad
comprehensions are there and they're easy to use. Astute readers will
also notice we used that syntax to generate our list of fizzbuzz's in
main. Monad Comprehensions are very flexible because they work with
the existing Monad rules, which are a bit out of scope for this post.

The main function is just the same sort of basic "please loop over
1..100" logic we see in every example; which is appropriate given that is
almost exactly how the FizzBuzz algorithm itself is defined.

Does this pass the "FooBarBazz Test?" Sure does (we'll use a synonym of
`mappend` called `<>` to make it cleaner):

~~~~~~{.haskell}
{-# LANGUAGE MonadComprehensions #-}

module Main where
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, listToMaybe, maybe)
import System.Environment (getArgs)


fizzbuzz i = fromMaybe (show i) $ ["fizz" | i `rem` 3 == 0] <>
                                  ["buzz" | i `rem` 5 == 0] <>
                                  ["bazz" | i `rem` 7 == 0]

-- Read the first argument as a number or just use 100.
main = do
  upTo <- fmap (maybe 100 read . listToMaybe) getArgs
  mapM_ putStrLn [ fizzbuzz i | i <- [1..upTo] ]
~~~~~~

<br />

Pretty, isn't it? But not just pretty, we've managed to define our
program in a way that is extensible, clear, and performant. We haven't
been forced to travel down the continuum of abstractions to lambda
lists, we're staying fairly high up and describing things in terms of
_when to concatenate_ and _how to fall back on optional values_.

The reason we can do this so naturally is that Haskell has already
captured logical patterns (monoids and Maybe) that aptly and succintly
describe our control flow. These patterns are not anymore complex than
just a few definitions, but they allow code to function at a much
higher level. Without these abstractions, we need to build our control flow out of
more basic parts that do not elegantly express our intent.

## That's Wonderful. Who Cares?

There's more to this implementation than just a deep feeling of
satisfaction and some benefits to extensibility. We've written our
concatenation in terms of a monoid, so it turns out any suitable
monoid will do. That can matter.

Here's a very real-world problem: many programming languages have a
native string type that is not actually very good for UTF-8
strings. Haskell's native string type is actually a list of
characters, which is also pretty terrible from a performance
perspective. It turns out that since we wrote our program in terms of
monoids and constant string values, we can actually replace our
default strings with better Text values:

~~~~~~~{.haskell}
{-# LANGUAGE MonadComprehensions, OverloadedStrings #-}

module Main where
import Prelude hiding (putStrLn)
import Data.Monoid (mappend, (<>))
import Data.Maybe (fromMaybe, listToMaybe, maybe)
import System.Environment (getArgs)
import Data.Text
import Data.Text.IO


fizzbuzz d i = fromMaybe (d i) $
                 ["fizz" | i `rem` 3 == 0] <>
                 ["buzz" | i `rem` 5 == 0] <>
                 ["bazz" | i `rem` 7 == 0]

main = do
  upTo <- fmap (maybe 100 read . listToMaybe) getArgs
  mapM_ putStrLn [ fizzbuzz (pack . show) i | i <- [1..upTo] ]
~~~~~~~

1. We use the newer `Data.Text` values, and use the `OverloadedString`
language extention to allow us to naturally create `Text` values instead
of `String` values when the type inferencer can determine we need to.
2. We also alter our function to take a default value generator, so we can move the decision how to produce the default out to the caller.

We could even do all sorts of potentially useful things, like instead
of using a string representation we could use a key-value mapping to
store the occurences of Fizz, Buzz, and Bazz. A monoid there would
even give us a way to combine them across iterations, building a
counter for occurences. All while using the same original function!

It turns out that the "Monoid View" of this logic is not only more
clear and succint, but it's also _more general_. It's not just Arrays
and Strings and Integers that form monoids, but also more powerful
structures like Bloom Filters, Hash Tables, nodes in a neural network,
a lot of things!

## Monoids, For Your Consideration

As I get more experience in different fields and disciplines of
programming, I'm constantly amazed how I've failed to recognize
patterns other people have caught, named, and implemented. I try my
best to integrate these into my professinal life, but it can be
tricky. I had the same kind of revelation years ago when someone
handed me a copy of (Java Concurrency in
Practice)[http://www.amazon.com/Java-Concurrency-Practice-Brian-Goetz/dp/0321349601]
and helped me realize what "good" concurrent code was.

Monoids are an example of something most functionally programmers
trivially use all the time but you almost never see explicitly called
out in imperative languages. Which is a shame, becuase it's an
incredibly powerful perspective from which to consider your logic.

Functional programmers have the advantage and disadvanage of working
at a slightly lower level of abstraction (or perhaps guidance? )
than most modern OO programmers do; they don't have the implicit structure granted by
"everything as an object" to lean on (or be constrained by), so it's
only natural that functional programming takes control flow
abstraction more seriously.

I've found myself more than once recognizing where the abstraction of
a Monoid might be useful becuase then I could start with something
simple and then replace it with something more complex later. It's
only the "appendy" property of it that I actually care about. Before I
started working with Haskell, it only dimly registered with me that
this *was* a point of abstraction. The power to generalize at this
axis is not something most programmers today recognize readily, but we
might all be better off if we did.
