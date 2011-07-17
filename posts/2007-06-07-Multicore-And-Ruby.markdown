---
title: Multicore Hardware &<br> The Future of Ruby
date: June 7, 2007
author: Dave Fayram
tags: ruby, multicore, programming, cross-post
description: An argument that Rails needs to take multicore and resource sharing very seriously. It was unheeded.
---

> This was originally posted on the [Oreilly blog](http://www.oreillynet.com/ruby/blog/2007/06/multicore_hardware_and_the_fut.html). It has been reposted here for archival.

Consider this fact: Multi-core CPUs are not only the future, they’re the only way CPUs can continue to grow at their current pace. It’s also a hotly debated subject in the software world. Multi-threaded programming is different and not seen as often as procedural programming, and therefore it’s not yet as well understood. So the question is, how can programming languages (and Ruby in particular) make it easier to harness these systems?

As Ruby struggles to graduate from its current implementation into something more powerful, we’ve already seen several projects attempt to update Ruby to help developers cope. Those who’ve been working with Ruby for awhile may remember [YARV](http://www.atdot.net/yarv/), which promises to provide more threading support. [JRuby](http://jruby.sourceforge.net/) offers all the power of Java’s threads to Ruby, if it can harness it. And Evan Phoenix’s small but rapidly growing project [Rubinius](http://rubini.us/) is attempting to be the next big contender.

No matter what implementation becomes the next de-facto Ruby platform, one thing is clear: People are interested in taking advantage of their newer, more powerful multi-core systems (as the recent surge in interest in Erlang in recent RailsConf and RubyConfs has shown). As Ruby becomes increasingly part of solutions that deal in high volumes of data processing, this demand can only increase.

That’s why it’s so very surprising to see David Heinemeier Hansson [dismiss the whole notion out of hand regarding Rails.](http://www.loudthinking.com/posts/7-multi-core-hysteria-and-the-thread-confusion) His argument seems to be that Rails already scales to multiple cores in the same way it scales to multiple machines, via UNIX process distribution. After all, isn’t this the very crux of “Share Nothing?”

But the math says something different, because for a single server “Share Nothing” doesn’t really exist. Even if the processes don’t share state, they share the same pool of resources (e.g., system memory, disk and system bus bandwidth). Each one can be a serious issue. Consider a deployed Rails application, where each Rails process (running mongrel) weighs in at about 200 real megabytes of RAM. If we wanted to take advantage of 8 cores, we’d be using a bare minimum of 1.6gb of memory–not to mention an even more dire situation with system bus bandwidth. With a dual-processor setup, you could easily see a machine with 16gb of RAM being resource starved.

David talks about welcoming a 64-core chip, but the truth is that Rails’s process-level concurrency can already barely accommodate today’s top of the line. Within 6 months we will see machines with 32 and possibly 64 cores in a dual-processor configuration as a top of the line, and today’s best being commonplace. What scales for many machines doesn’t scale for one.

It isn’t surprising that many Ruby libraries prefer to scale at the process level. The argument for process-level concurrency is a good one: It’s dead simple. We’re already doing it, and it’s worked fairly well up until now. It’s also simple because some of the many Ruby libraries that Rails uses don’t play nice with threads. Changing that requires a lot of work, and it’s work that wouldn’t immediately yield up any new features. It’s a lot of work for a status quo, which can be hard to invest time in.

The most important thing to remember when thinking about the future of Ruby is that just because we don’t have convenient methods for threading Ruby today, it doesn’t mean we shouldn’t explore all the possible avenues. YARV, JRuby, or Rubinius may come along any day and blow us away with completely new ways to think about working with concurrency. If Rails is ready for this, it can continue to be on the forefront of web toolkits. If it is not it will rapidly fall behind, because ignoring the problem at this stage is ignoring problems that well-funded startups have already encounterd.

Talking to some major Rails developers for 5 minutes, ideas like simultaneous request-processing (something Ezra Zygmuntowicz’s [Merb](http://brainspl.at/articles/2006/10/18/merb-is-useable-and-a-gem) already does), parallelized partial rendering, and really crazy out-there future-talking ideas like stateful HTTP, or trivial implementations of Comet-like technology were mentioned immediately. Imagine what could be accomplished with a real implementation to play with?

Keep an eye on these new Ruby implementations. The first people to really innovate technically with them will have an enormous advantage over their competitors.
