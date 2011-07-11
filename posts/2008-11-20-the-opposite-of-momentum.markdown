---
title: The Opposite Of Momentum
date: November 20, 2008
author: Dave Fayram
tags: ruby, multicore, programming, cross-post
---

Let me start by saying that I’ve been a longtime fan of Ruby. I’ve been a member of the #caboose cabal forever, and I’ve written hundreds of thousands of lines of ruby over the course the last 6 years. I’ve drank the Ruby kool-aid, helped to start two [Rails](http://ma.gnolia.com/) [Startups](http://mog.com/) and integrated a lot of code into Ruby. I fought tooth and nail to get Lockheed Martin to include Ruby deliverables in their RSA2 standardization project (didn’t you know? Ruby helps launch rockets!)

Ruby has been awesome to me and mine, and has basically grown a career path for me that I might not otherwise have gotten. I owe my position in [Powerset](http://powerset.com/) to Ruby and the Ruby Community. In 2006, I would have rebuffed a lot of the complaints I am about to make, because they were not so egregious and the climate of the software engineering world was different. But this is 2008, nearly 2009, and things change.

So, it’s with great sadness that I agree with several recent Rubyist’s blog posts and say that Ruby is in a very bad place right now. It’s no longer cutting edge, it’s technically stagnant, is in implementation limbo, and just isn’t… well… fun, anymore.

I am not trying to troll the Ruby community, and I’d like nothing more than for these problems to go away, but I think that we need to talk about these issues. I know a lot of other Rubyists are thinking what I’m about to say. Ruby is poised on the precipice (if not already over the edge) of a negative feedback cycle that will ultimately lead to its balkanization and marginalization within a few short years if things don’t change soon.


No Particular Place To Go
-------------------------

<img src="http://idisk.mac.com/dfayram/Public/Pictures/Skitch/YouTube_-_Lonely_Man_-_Incredible_Hulk_shortened_theme_%28Family_Guy%29-20081120-165331.jpg" style="float:right; width: 50%">

Perhaps the most frustrating part about Ruby, to me, is the outrageously outdated state of the current Ruby interpreter. There is basically no way to avoid writing software that leaks memory. It will happen, you just have to make it leak as little as possible. I still remember the [_massive_ effort](http://groups.google.com/group/god-rb/browse_thread/thread/01cca2b7c4a581c2) that [Tom Preston-Werner](http://tom.preston-werner.com/) went through to get a relatively simple program like God.rb to not leak memory… and it still leaks memory!

And let’s be honest, Ruby is one of the slowest choices in the scripting language market. The Ruby interpreter is [dead last](http://shootout.alioth.debian.org/u32q/benchmark.php?test=all&lang=all) in the GCLS. Now, I know the GLCS is synthetic and not very useful, but it’s still worth noting that Ruby is so slow that the primary defense for its speed is the 37signals-esque “It Just Doesn’t Matter™”. And on top of all this, the implementation is quirky and buggy and all sorts of little gaffs show up. For example, inline blocks don’t establish a new scope, so iteration variables are left behind in the enclosing scope, making for some weird artifacts.

Basically, the Ruby 1.8 interpreter is a mess. The Ruby 1.9 interpreter is a little better, but is incompatible and not designed for real people to use (rather it’s claimed that it’s a transition release moving to 2.0). So what are we supposed to use in the meantime? YARV was supposed to be done Christmas of 2005, haha. Rubinius looked like the light and hope of the Rubyverse, but their [team has been slashed](http://blog.fallingsnow.net/2008/11/18/a-sad-day/) and they’ve scrapped their core interpreter for a rewrite in the face of several setbacks. They’re nowhere near a release. jRuby is great, but I kind of liked the illusion of a small memory footprint–a lot of Ruby use is in scripting and starting up a “java -server” instance is not really desirable for that. And does anyone even remember IronRuby (I guess most ruby developers see the CLR as a trap).

There are no good choices for a Ruby interpreter. The best choice we can make is the compatible 1.8.* series, which has the aforementioned issues. What was once a great strength of Ruby (a single choice for a canonical interpreter) is now an anchor holding back from progress.

Evolutionary, Not Revolutionary
-------------------------------

Even if you can stay the course and Ruby meets your performance and stability needs, Ruby’s future isn’t looking terribly exciting. What was once a cutting edge and very exciting scripting language is now basically stagnant for several years. And if you look at the changes 1.9 is brining, most of them are incremental and evolutionary rather than big, exciting changes that might help Ruby resume its place of the curve.

Let’s rundown some things people are excited about–and while I do this you should substitute a crowd unenthusiastically saying “yayyyyy”. We’ve got Unicode strings, sorta (yayyy). We’ve got a minor revision to the execution model to add coroutines (but you could already do this with continuations, which aren’t getting much love). We’re getting a fundamentally unchanged OO model. Blocks are getting slightly refined, but they aren’t getting any cool new features like being serializable.

Most of the really interesting new features in ruby (real threads, better continuations, type inference, restarting from crashes in third party code, better FFI in general) are all happening in the byzantine pre-alpha world of independent ruby implementations, which it’s almost guaranteed you’re not using.

Should I Stay Or Should I Go
----------------------------

All in all, I’m left with a sinking feeling every time I start a new project in Ruby. I know I’m in for a pound of trouble down the road if my project is even moderately successful, and trouble with the same annoying crap I’ve seen a dozen times now. Ruby does some awesome things, and it still is a terrific environment for prototyping and deploying a moderate-to-heavy traffic website. But the problem is that it’s essentially stagnant.

To really twist the knife, all kinds of exciting shit is going on in the world of interpreted languages. The Javascript Interpreter Arms Race had led to some of the fastest and most interesting interpreters I’ve ever seen (V8 and SquirrelFish Extreme and Tamarin-Tracing). PLT Scheme’s 4.* release cycle is chugging along making an incredibly practical and fast Scheme (for real! Practical and Scheme!). All sorts of compiled HLL technology is really coming to fruition.

And what do I have to look forward to in the Ruby world? Essentially the same promise that I heard in late 2005, “Christmas will bring a faster ruby.” I’ve got to make a decision for the next project I’ve got coming up, should I just stick with Ruby because it’s the mainstream, or should I take the path less traveled like I did back in 2002 and start the cutting-edge cycle all over again?

I wish we could change all that. I wish that we had a new, exciting Ruby engine that began to rapidly iterate, was extensible in Ruby itself, and really started to live the dream that Smalltalk failed to achieve in the enterprise: being both cutting edge and mainstream. Without decisive action in the next few months, things are going to go into a cycle the Ruby community may not be able to recover from. People will leave trying to stake out The Next Big Thing™, bleeding the Ruby talent pool dry and making it even less capable of recovering from these bad fortunes.
