---
title: Functional Programming Is Hard,<br>That's Why It's Good
date: 08-19-2011
tags: programming, functional, haskell, scala, ocaml, lisp, prescriptive
author: Dave Fayram
description: The reason FP is hard is because it requires people to learn new fundamentals of programming.
---

Odds are, you don't use a functional programming language every
day. You probably aren't getting paid to write code in Scala, Haskell,
Erlang, F#, or a Lisp Dialect. The vast majority of people in the
industry use OO languages like Python, Ruby, Java or C#–and they're
happy with them. Sure, they might occasionally use a "functional
feature" like "blocks" now and then, but they aren't writing
functional code.

And yet, for years we've been told that functional languages are
awesome. I still remember how confused I was when I first read [ESR's famous essay](http://catb.org/~esr/faqs/hacker-howto.html) about learning Lisp. Most
people are probably more familiar with [Paul Graham's "Beating The Averages"](http://www.paulgraham.com/avg.html)
which makes the case that:

> But with Lisp our development cycle was so fast that we could sometimes duplicate a new feature within a day or two of a competitor announcing it in a press release. By the time journalists covering the press release got round to calling us, we would have the new feature too.

A common thread among people proselytizing functional programming is
that learning this new, functional language is "good for you";
almost like someone prescribing 30m in the gym a day
will "make you fit." Haskell, Ocaml, and Scala are different from Lisp
in that hey have a certain notoriety for being very hard to
learn. Polite people call this "being broad & deep". Less polite
people call it "mental masturbation" or "academic wankery" or just
plain "unnecessary." I submit that this difficulty is a familiar
situation, and it's a strong indicator that learning one of these
languages will make you more productive and competent at writing
software.

## Your First Time Wasn't Gentle

I learned to code when I was 7, fiddling on my grandfather's computer
on long, boring suburban summers. I learned BASIC, made a ball dance
on screen. I learned Pascal and wrote a program to play back music on
the PC speaker. I learned C around age 10, but hit a pretty big wall there
that I dodged around until high school: pointers. Even ignoring those
damnable pointers, I worked and read and studied and practiced and
failed a whole lot. I deleted my grandfather's hard drive twice (once
by accident), and ended up reinstalling my OS more than a few times. I
failed, over and over.

Maybe your story is similar, maybe it's wildly different. But I think
nearly everyone who learned to program can relate to the difficulty of
the process. And then, after learning the basics, we again
encounter well-known conceptual thresholds like "pointers." Many
computer science professors describe pointers as a kind of
filter in their curricula. If you are going to be a good programmer, you must be
able to understand pointers. And the people who easily learn them are
few and far in between. Most people, myself included, needed practice
and examples to understand what pointers were and why they were important.

This nearly universal struggle isn't a coincidence. Pointers are a
very powerful and fundamental abstraction. Learning them makes you a
better programmer by helping you think more symbolically. Even if you
work in a language that doesn't offer you pointers, pointer-like
structures and concepts abound in the wild.

## Novelty

Once you learn a few languages, they all start to look the
same. Someone who knows Python probably won't have too many problems
learning Ruby, someone who knows Java has a leg up on learning
C#. Sure, there are hangups. A Rubyist learning Python might have a
few surprises learning for comprehensions, and the Java user might have
some problems wrapping their heads around C# [delegates](http://msdn.microsoft.com/en-us/library/ms173171(v=vs.80).aspx). Still, if you
squint, they all sort of look like one another. And I can assure you,
if you don't know this yourself, that once you learn A Lisp you start
to see the similarities in all the variants of Lisp.

That said, most people are totally unprepared when they first meet up
with Haskell or Ocaml. Heck, in Haskell _even the semicolons are
different._ This isn't just an issue of syntax; Haskell and MLs are
actually based on fundamentally different abstractions and a whole new
language of patterns. You build apps differently, you structure them
differently, and you extend them differently.

Lots of these new ideas are incredibly powerful. [Monads](http://www.haskell.org/haskellwiki/Monad) are at least
as fundamental and powerful an idea as pointers (and you probably use
them implicitly without realizing what they're called). So unlike
learning C# after learning Java, aspirants to functional languages have
to go back much further and learn much more fundamental concepts to
continue. It's like learning pointers all over again. And, just like
back when we were first setting out to learn programming, these big
concepts can be frustratingly illusive and vague until you've worked
(and failed) with them.

## Take Your Medicine, Find Your Pharmacist

Despite this downside, I submit that learning these functional
languages is good for you, professionally. I suspect some readers
are rolling their eyes at this point, being unable to imagine a world
where [monoids](http://en.wikipedia.org/wiki/Monoid) or monads are useful to them in Java or C#. To me, it's
no surprise that people balk at the prospect of learning FP; they are
trying to learn new abstractions on the same fundamental level as
pointers and recursion. That requires patience and dedication on a
level that most professionals reserve only for actually completing
clear business goals. Very few people feel comfortable failing-let
alone failing over and over- once they're out of their formative
years. We're all supposed to be professionals now, right?

To further complicate matters, a lot of language and algorithm
research takes place in functional languages (particularly Haskell).
It's very easy to get lost in an unfamiliar field of [category theory](http://en.wikipedia.org/wiki/Category_theory), [half-finished abstractions](http://okmij.org/ftp/Streams.html#iteratee), and
failed ideas. Without a clear guide (like a [good book written by a pragmatic author](http://book.realworldhaskell.org/read/)), an already-difficult task becomes even more
daunting.

This combination leads to an unsurprising result: many people are
reluctant to put the time and effort into learning FP. Justifying this
reluctance is easy, "Couldn't I be spending this time making something
as opposed to learning something?" But this line of thinking means you
never stray towards any new technologies (or only very familiar
ones). In an industry that changes as rapidly as software engineering
does, I think it's not a realistic value judgment.

## Saying is Believing

The most obvious benefit from learning a functional language is that
you will learn the pattern language for basic functional
concepts. This gives you the power to consider and mentally manipulate
surprisingly big concepts in your head very succinctly. This isn't a
magic property of FP; languages and patterns emerge to describe
certain classes of problems succinctly. It's just relevant because
FP's conceptual sweet spots has been becoming increasingly relevant in
a world where parallelism and meta-programming are important.

For example, consider a simplified, local version of the famous Google MapReduce paradigm. Describing this paradigm succinctly in a functional way with well-known functional patterns is surprisingly brief:

      mapReducer data partitioner mapper reducer =
        let partitions = partitioniner data in
          reduce reducer (map mapper partitions)

Changing this to support parallelism and distributed concurrency is
trivial (for local parallelism, many libraries support "pmap" and
"preduce"–which exploit simple properties of functional
languages–as drop in replacements). The concept of maps, partitions, generators, streams,
reductions, folds, and function chaining are all very simple patterns
that are part of the shared language of functional programming, so
anyone with a passing familiarity with Lisp, Haskell, OCaml, or even
sorta-functional languages like Python and Ruby will be able to
understand the gist of this.

Consider for a moment what it would take to succinctly describe this
framework in an OO language using only common OO patterns. At bare
minimum you'd be asked to define the spec for what a mapper and
reducer are. If you're curious, try laying out a minimal spec for "OO"
MapReduce in your ideal OO language. I found it to be quite
verbose. With a Java-like language, one might say:

        interface Mapper<A,B> {
          B map(A input);
        }

        interface Reducer<X,Y> {
          Y reduce(X a, X b);
        }

        abstract class MapReduce<X,Y,Z> {
          private Mapper<X,Y> mapper;
          private Reducer<Y,Z> reducer;

          public MapReduce(Mapper<X,Y> map, Reducer<Y,Z> reduce) {
            // ...
          }

          public run(SeqenceType<X> data) {
            // ...
          }
        }

Without even going through the loop logic, our inability to access the
common nouns and verbs of the functional paradigm makes the MapReduce
technique very expensive to define. This definition is almost
comically naive, but it helps show how much you can relate with functional
concepts. Another great example is how Scala could take the [already great Java Fork/Join library](http://gee.cs.oswego.edu/dl/papers/fj.pdf) and [easily integrate its features into Scala's natural syntax](http://stackoverflow.com/questions/3740505/scala-analogues-of-qtconcurrent).

## One Person's Work is Another Person's Pleasure

And so, I encourage everyone who wants to be a better programmer:
consider learning a functional language. Haskell and OCaml are both
great choices, and F# and Erlang are pretty good as well. It won't be
easy, but that is probably a good sign. Try and identify the difficult
concepts you encounter and see if other people are leveraging
them; frequently you can break through a mental roadblock by finding
out what the intent of an unfamiliar abstraction really is.

While you're learning, do be careful not to take it _too_ seriously.
Like anything that requires time and effort, there is a danger of
becoming over-invested in FP. Falling into this cognitive trap will
ruin your investment. It's easy to forget how many models of
computation there are out there, and even easier to forget how much
beautiful software has been written with any of them.

It's a narrow path to walk down, but on the other side, you emerge with more core concepts and
models to leverage in your everyday programming. You will almost
certainly become more comfortable with denser code, and will certainly
gain new insights into how to be a better software engineer.

## Addendum

A few of the nice people who proofread this essay asked me the same
question once they were done: "This sounds great, Dave, but which
language should I learn?" This is, of course, the toughest question
they could ask me.

I think if you're already a competent programmer then there isn't a
"right" answer other than: "Whichever one meets your needs." If you
need to work on the JVM, pick Scala or Clojure. If you want to write
big distributed software systems quickly, pick Erlang. If you want
amazing general workhorse languages with terrific compilers, pick Haskell or
OCaml. If you want a prototyping medium with more potential than Ruby
or Python, go for Scheme.

Remember, the name of the game here is practical skills _and_
self-improvement. If you can spare the time, try stepping out of your
comfort zone and challenging yourself.

Since I already knew Lisp and Erlang and have done professional work
with OCaml, I decided to tackle Haskell, which is a whole other world
unto itself. The only way I found that language penetrable was with
the helpful guides of
[Learn You A Haskell](http://learnyouahaskell.com/) and
[Real World Haskell](http://book.realworldhaskell.org/). These books
are well-written, helpful, and freely available online. Should you
choose to try your hand at Haskell, these books can be your
road map.
