Published as, http://blog.risko.hu/2012/04/ann-hflags-0.html,
replicated here for redundancy.

HFlags is library for making it easier to specify and use command line
flags in Haskell programs and libraries.  It is very similar in its
concepts to Google's [gflags](http://code.google.com/p/gflags)
library.

#### TL;DR

Example:

```haskell
#!/usr/bin/env runhaskell

{-# LANGUAGE TemplateHaskell #-}

import HFlags

defineFlag "name" "Indiana Jones" "Who to greet."
defineFlag "r:repeat" (3 + 4 :: Int)
  "Number of times to repeat the message."

main = do remainingArgs <- $initHFlags "Simple program v0.1"
          sequence_ $ replicate flags_repeat greet
  where
    greet = putStrLn $ "Hello "
	                   ++ flags_name
					   ++ ", very nice to meet you!"
```

Code: https://github.com/errge/hflags<br>
Docs: http://hackage.haskell.org/packages/archive/hflags/latest/doc/html/HFlags.html<br>
More examples: https://github.com/errge/hflags/tree/master/examples

#### There are a tons of flags libraries already for Haskell, aren't there?

Yes, but none like [gflags](http://code.google.com/p/gflags)!  All of
them are like
[getopt](http://www.gnu.org/software/libc/manual/html_node/Getopt.html).
Some has fancy Template Haskell automation, some not, but in general,
they are all the same.  If you want to look into some, I recommend
[CmdArgs](http://hackage.haskell.org/package/cmdargs) and
[options](http://hackage.haskell.org/package/options).

Some properties of getopt like libraries, that I don't like:

* You can only access the flags in the `IO` monad.  This seems to be
  reasonable, at least at first look.  Flags originate from the
  environment, and accessing the environment is only safe through
  `IO`, right?  But in my opinion, they are more similar to constants,
  they should be easy to use everywhere.
* You have to pass the flags to every function where you want to use
  them.  This is also something, that is not true for simple top level
  constants, why should flags behave differently?
* Getopt makes it very hard to compose different code parts
  (e.g. libraries) that all use command line flags.  Imagine, that you
  are implementing a sendmail library, which will use the default
  `/usr/bin/sendmail` executable on the system, but gives the user the
  flexibility to change the path via command line flags.  This can be
  done via getopt like thinking, but it requires a lot of boilerplate
  in the program using your library (for an example, have a look on
  [option's import feature](http://hackage.haskell.org/packages/archive/options/0.1.1/doc/html/Options.html#g:6)
  and imagine doing that for all of the libs you use).
  [Gflags](http://code.google.com/p/gflags) solved this issue very
  nicely for C++/Java/Python, but there were no similar solution to
  Haskell.

HFlags tries to get rid of these properties and be as simple and easy
to use as possible.

#### gflags C++ example

As a motivation for HFlags, let's have a look on Google's C++ example:

```c++
#include <gflags/gflags.h>

DEFINE_bool(big_menu, true,
            "Include 'advanced' options in the menu listing");
DEFINE_string(languages, "english,french,german",
              "comma-separated list of languages to offer");

...

int main(int argc, char **argv) {
   google::ParseCommandLineFlags(&argc, &argv, true);
   ...
   if (FLAGS_big_menu) { ... }
   ...
}
```

Note, that once you called `google::ParseCommandLineFlags` you're
done.  All of the flags in every linked in C++ file gets initialized
by that call and you can access all of the flags in every file (where
they are declared) via top level, global names.

#### Achieve the same in Haskell

It was a long journey to achieve this kind of comfort in Haskell, in a
later post I'll do a code walk around `HFlags.hs`, but it's already
well commented and should be understandable for anyone who is familiar
with type classes, instances and a little bit of Template Haskell.
Instead, let's concentrate usage for now!

If you decide to give it a try, all you have to do is to
[`cabal install hflags`](http://hackage.haskell.org/package/hflags),
then
[`import HFlags`](http://hackage.haskell.org/packages/archive/hflags/latest/doc/html/HFlags.html)
in your source files where you define flags and in your main.  After
that, you can use
[`defineFlag`](http://hackage.haskell.org/packages/archive/hflags/latest/doc/html/HFlags.html#v:defineFlag)
for flags with type of `Bool`, `Double`, `Int`, `Integer` and
`String`.  If you need other types, you can look into `defineQQFlag`
and `defineCustomFlag`.

The last step is to make sure that you call
[`initHFlags`](http://hackage.haskell.org/packages/archive/hflags/latest/doc/html/HFlags.html#v:initHFlags)
as the first thing in your main.

If you are not up to coding right now, have a look at the
[simple](http://github.com/errge/hflags/blob/master/examples/SimpleExample.hs)
and the
[complex](http://github.com/errge/hflags/blob/master/examples/ComplexExample.hs)
[example](http://github.com/errge/hflags/tree/master/examples).  If
you can't believe that we can expose the flags in all the imported
modules automatically and you need a demonstration, look into
[ImportExample.hs](https://github.com/errge/hflags/blob/master/examples/ImportExample.hs).

#### Some criticism, we already heard

##### Fake pureness and usage of `unsafePerformIO` is bad!

The criticism goes like this: "This library is not pure, you are using
`unsafePerformIO`.  This is unsafe, it's in its name.  I don't know
what does that mean, but it can't be good, it's unsafe.  So unsafe.
Are you sure that this is OK?"

TL;DR: yes, we are sure, kind of.

Longer version: there are two uses of `unsafePerformIO` in our code,
one is trivial and well known, the other is a bit more tricky.

The simple one is responsible for the creation of the global `IORef`,
holding the `Map` that maps flag names to values.  Here, we used the
standard way to create a top level mutable variable, as discussed in
[the wiki](http://www.haskell.org/haskellwiki/Top_level_mutable_state).

The other one is when you define flag foobar, we create a top level
constant with the name `flags_foobar` containing the value.  This is
not a top level mutable variable, but a constant, so the wiki page
doesn't apply.  The usage of `unsafePerformIO` means that you can be
afraid of that these constants are not really constant and they change
randomly (depending on evaluation order, environment, state of the
moon) and not at all referentially transparent anymore.

To address this concern, we force evaluate all of these top level
constants at `initHFlags` time, so the thunk containing
`unsafePerformIO` gets evaluated in them and they become real
constants.  We generate a `NOINLINE` pragma for them, so they won't be
duplicated (and actually that wouldn't cause any issue either).

If you are interested to read more about these issues and other real
world issues in Haskell, I strongly recommend reading the very well
written
[Tackling the Awkward Squad from Simon Peyton Jones](http://research.microsoft.com/~simonpj/papers/marktoberdorf/mark.pdf.gz).

We are aware of these dangers, but we think that a trade-off had to be
cut to make command line flags usable, easy to manage and fun to have.
If you don't agree with the necessity of these considerations and you
believe only in totally pure solutions, this library is not for you.

Also, we are not experts on this topic, so if you still think that we
made an error somewhere and you can come up with some *real* example,
where our library screws up your program, definitely leave a comment!

##### Programs using this library will be hard to test!

If you're unittesting some code where the behavior can be seriously
changed via command line flags then this library is probably not your
biggest concern.  Those things should be system (functional) tested,
where specifying flags is totally normal.

BTW, have you heard of
[withArgs](http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-Environment.html#v:withArgs)?

#### Comments are welcome!

[![View from Thalwil train station](http://www.gergely.risko.hu/blogphotos/thalwil-20120430.jpg)](http://www.gergely.risko.hu/blogphotos/thalwil-20120430-orig.jpg)
