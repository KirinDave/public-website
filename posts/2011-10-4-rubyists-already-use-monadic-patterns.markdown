---
title: Rubyists Already Use Monadic Patterns
date: 10-4-2011
tags: programming, functional, ruby, Haskell, monad, monadic
author: Dave Fayram
description: Some concrete examples of monadic patterns in imperative OO programming.
---

My [last post](2011-08-19-lets-go-shopping.html) and many other posts on functional programming tend to
extol the benefits of functional programming and then end with a
catch phrase like, "People use (functional) patterns all the time,
they just don't have a name for them!" But after publishing my last
post, several people asked me for examples. I looked around to see if
I could find good material, but was shocked to find how rare
it was to actually follow up that assertion with concrete examples.

So, I'd like to tell you about monadic and applicative patterns in
Ruby. This discussion is somewhat applicable to Javascript or Python or any language
with Nil/Null Punning. A language with Nil Punning has a rule that
states something like, "Every value except Null (and false) are
considered true." We can relate this to the Maybe Monad.

Before we continue, I want to point out this is *not a monad
tutorial*, and there *won't be any Haskell or Haskell Notation*. You might gain
some insight into monads from reading this, but if you really want to
learn what they are and how to use them, you should check out the
sections in [Learn You A Haskell](http://learnyouahaskell.com/chapters) that explain Monads in excellent detail.

## Let's Get This Out Of The Way: A Monad Is...

I'm not even going to try and make some elaborate metaphor like
[burritos](http://codetojoy.blogspot.com/2009/03/monads-are-burritos.html)
or
[spacesuits](http://www.iterasi.net/openviewer.aspx?sqrlitid=ixx7fcluvek_9lfolsxr_g)
here: monads are rules for chaining functions together. That's what
they are in programming. Do you have functions you want to chain
together? You can probably use a monad to do it. Unsurprisingly, there are lots
of different Monads, all reflecting the different rules you can use to
chain functions together based on what those functions return.

For our discussion, we're going to talk about the "Maybe Monad". The
Maybe monad is aptly named, it discusses how to chain functions
together when the functions *might fail.* The Maybe Monad's rules
assume you have _Something_ or _Nothing_. It assumes every function
you want to talk about returns Something or Nothing. The rules are
obvious:

   1. If I have Nothing, stop computing.
   2. If I have Something, pass it to the next function.
   3. That function may return Something or Nothing. You can go back
      to step 1 from here.

Think about it for a second; if you've got Nothing, you've got nothing
left to do. If you've got Something, you can compute. This may seem
too abstract or too basic to be useful, but let's take a look at what
it looks like in Ruby.

## The Maybe Monad in Ruby is Everything

Ruby has two slightly unusual operators, ||= and &&=, that expand like so:

~~~~~~~{.ruby}
x ||= y # becomes x = x || y
x &&= y # becomes x = x && y
~~~~~~~

The logical-or version is more familiar, but you will see many people use both. Let's start with a bit of code you can find Rubyists using:

~~~~~~~{.ruby}
def open_connection_or_nil(addr, password)
  c   = get_host_by_name_or_nil(addr)
  c &&= Connection.new(c)
  c &&= ConnectionDecrypter.new(c, password)
end
~~~~~~~

This starts by taking an address (or nil), and then progressively
transforming it over and over. First, a host name; then, we make a
connection from it *if we got a host name*. Finally, we setup a
decryption wrapper around the stream. If any one step in the process
fails, then all the subsequent steps are skipped.

An even more familiar version of this pattern involves ignoring the
return value:

~~~~~~~~~~{.ruby}
def render_page
  u = session[:user]
  if u && u.is_admin && u.is_active
    render_admin_page
  else
    render_access_denied
  end
end
~~~~~~~~~~

This code first computes a value that is Something (a user) or
Nothing (nil), then IF it is real and IF it is an admin and IF it is
active, proceeds down a branch.

Sound familiar? It should. This is exactly the same pattern that
Haskell programmers would use with the Maybe monad. Using boolean
operators and Nil punning, rubyists are using a monadic programming
pattern _all the time_.

"Now hold up, Dave," astute readers might be saying. "These are
conditionals and expressions. Didn't you say we were going to chain functions
together?" Well, yes. But what, really, is a function? It's an input
and an output. These &&= statements aren't really functions, but
that's more an artifact of Ruby's syntax than any real restriction. If
you squint, they look the same. Spoiler alert: we will squint below.

## Let's Talk About Choice

We can also talk about *monadic choice*. Some monads (like our friend,
the Maybe monad), have this idea of "Choice." What do we mean? This is
how we "choose" between multiple Maybe values:

  1. If our choice is between Nothing and Nothing? Choose Nothing.
  2. If our choice is between Nothing and Something, Choose Something.
  3. If our choice is between Something and Something, Choose The
  First Something.

What does this look like in Ruby? Instead of && and &&= (logical and),
we switch to the ||-versions (logical or):

~~~~~~~~{.ruby}
def update_or_create_user_tag(name, tag)
  u = User.get(name)
  u ||= User.get_from_facebook(name)
  u ||= User.create(:name => name)
  u.add_tag(tag)
end
~~~~~~~~

This code does the opposite of the previous code. Instead of building
on a previous result, this code tries to choose among one of three
methods to find (or create) a user and perform an action on it.

## So What's The Point?

A legitimate question to ask at this point would be, "What's the
upshot of code like this?" For starters, this is a way to write
briefer code in Ruby. Try to write these examples with explicit
boolean conditionals, and you'll rapidly watch your code scuttle over
and see a tedious "end end end" cascade.

But beyond that, this pattern of chaining functions together is more
widely applicable for a variety of rules. As a thought experiment, let's start by actually
making this pattern explicit in ruby, and call it "mbind". I want to
stress that this is _not_ necessarily good Ruby code as it stands;
just take it a learning experience.

~~~~~~~~{.ruby}
class Object
  def mbind(&k)
    k.call(self)
  end
end

class << nil
  def mbind(&k)
    nil
  end
end
~~~~~~~~

Now we can rewrite some of our old examples:

~~~~~~~~{.ruby}
def open_connectionM(addr, password)
  get_host_by_name_or_nil(addr).mbind { |ip|
    Connection.new(ip).mbind { |conn|
      ConnectionDecrypter.new(conn, password)
    }
  }
end
~~~~~~~~

Here's that squinting we were talking about. It's not the prettiest code, but we're now talking about our algorithm in
terms of monadic chaining. Monads do lots of different things, but
they all follow the same rules. If we obey those rules, then it
generally doesn't matter which monad we use. So let's introduce a new
Monad and swap it in.

My favorite Monad is the List Monad. The List Monad models
non-determinism, so it's sort of like the Maybe monad. Instead of
having functions that return Something Or Nothing, we have a List.
This list may be empty, or have many things. The chaining (and
subsequent return values) need to be flat lists for it all to work.

Before you read the code below, think about what this monad's #mbind
method might look like in Ruby. There is an everyday method that we
use to implement it.

...

Ready? It's not long,

~~~~~~~~{.ruby}
class Array
  def mbind(&k)           # We break the Maybe monad here. Sorry, Maybe!
    self.map {|v| k.call(v)}.flatten(1)
  end
end
~~~~~~~~

This method expresses the chaining rule for the List monad. It's one
rule, but it's surprisingly powerful:

1. If the list is empty, return the empty list.
2. If the list is not empty, pass each value in the list to the next
function. It will return a list (or list of lists), which we flatten.

If it doesn't seem immediately obvious what this would do, let's take
a look at some examples:

~~~~~~~~{.ruby}

# We can make lists bigger
[1,2,3].mbind {|i| [i, i*10]} # => [1,10,2,20,3,30]

# Or smaller
(1..100).to_a.mbind { |i|
  if i % 2 == 0
    i
  else
    [] # Empty list means no result.
  end
} # => [2,4,6,8..]

# Or both
[10,20,30].mbind { |i| [i-1, i, i+1] }.mbind { |i|
  i % 2 == 1 ? i : []
} # => [9,11,19,21,29,31]

~~~~~~~~

This has the effect of letting us consider many options with simple
code. So now let's make our open_connectionM function more reflective
of the real world. Instead of get_host_by_name_or_nil, we could now use
get_hosts_by_name, which returns a list of all IPs associated with a
hostname or hostname pattern. Just by using a different monad at the top of the
chain, our mbind would inject every available ip for a host try to return
us a connection to all of them. Our connection methods need only
return an empty list to signify the connection is impossible, and the
client ends up with all possible connections.

In other words, we've totally changed the mechanics of our function
but the original intent--the important parts, if you will--has been
preserved. That's the power of Monadic programming. What's even
stranger is that the functions we're using don't have to know they're
working within the context of a monad; they're just doing their thing
and returning reasonable values and the monad's chaining rules are
making sure the right result comes out.

## There And Back Again

If you take nothing else from this post, take this: Monadic
programming is a pattern that comes up in a lot of  code, just like most
patterns. Monads as a programming technique have been popularized by
Haskell (which is one of the few environments where they make sense to
model directly), but the general shape of the code and the rules they
follow are generally worth considering.

Clever readers might as, "If this sort of programming is so awesome,
how come we don't see monads explicitly modeled outside of Haskell?" It's a good question, and
there isn't one single answer. For starters, without type inference
you have to specifically mention which monad you want to use. Having
to name them explicitly instead of referring to them as a type
variable really does reduce their utility.

That said, some monads (like Maybe) are so natural in languages like
Ruby that people have decided that it is worth the time to express
them formally. One notable example of this is
[raganwald's andand framework](https://github.com/raganwald/andand), which is worth a look if the code
and concepts here interested you.

While Monads are a poster child of Haskell's school of expression, there are actually a
lot of patterns in this vein that are worth recognizing. Functors and
Monoids come to mind as other simple functional patterns lifted from
category theory that are valuable (and common). Don't be afraid to go
digging into the Haskell wiki looking for new names and rules for
patterns you already know.


## An Aside For My Friends From Haskell Land

Some people familiar with Haskell might have some objections about the
particulars of this post. "That's not a monad, that's more like an
Applicative Functor!" perhaps, or "That is not all Monads are!" I'm
happy to add errata or addenda here, if you want to reach me on
Email, FreeNode (as KirinDave), [Twitter](http://twitter.com/kirindave), or
[Google+](https://plus.google.com/u/0/117830126779727400170) and
we can talk.

And almost immediately, I got some feedback. Here are some corrections
that I think are interesting to mention:

1. Shachaf@#haskell points out that Ruby's nil-punning-to-simulate-Maybe isn't a perfect copy of the
Maybe monad. You cannot express nested maybe types, which a real Maybe
type would let you do. As an example, consider "Just Nothing".
2. @alanmalloy reminded me that flatten is too aggressive. What I wanted was #flatten(1). Thanks!
3. The entire ruby community seems to want to argue about what ||= really means. For pedanic completenes, by expansion `(x ||= y # -> x = x || y)` is only correct for locals, where it is the closest approximation to what actually happens. Ruby has a whole separate set of rules when you call it on an object, and in that case it is closer to `( x[:a] ||= y # x[:a] || x[:a] = y )`. Please keep this in mind, but in practice it seldom makes a difference to the end result of your code.
