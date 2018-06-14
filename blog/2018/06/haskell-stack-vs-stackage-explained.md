# Haskell Stack vs. Stackage - Explained

I am sitting in the Denver Airport terminal, waiting for my flight to take home from [Lambda Conf 2018](https://lambdaconf2018.dryfta.com/en/) to Chicago. This conference was the best I have attended in recent years: the quality of the talks, the friendliness of presenters and the attendees is just something I have not seen in a long time.

I set down next to a guy ([Dan Burton](https://github.com/DanBurton) to be exact) with my lunch a couple of days ago and I started a conversation with him. When we chatted about open source, he mentioned he works on [Stackage](https://www.stackage.org). I heard about it, but I asked him to tell me more. I learned quite a bit, I figured others would benefit from having this information, hence the idea of this blog post was born.

[Stack](https://docs.haskellstack.org/en/stable/README/) came along a couple of years ago to save us all from "Cabal Hell". I never had to experience it as I started learning Haskell only two years ago, long after Stack and Stackage was born. But understanding what they provide helps me appreciate it even more.

When you start a new project with Stack, there is one line in `stack.yaml` that has significant importance:

```yaml
resolver: lts-11.11
```

This value makes the connection between Stack and Stackage.

"_What is `lts-11.11`?_" - It's the long term support version of Stackage.<br>
"_What is Stackage then?_" - It's a set of Haskell tools and libraries tested together in a snapshot making sure that the specified versions work together.<br>
"_A snapshot? What's that?" - An LTS or Nightly release of packages.<br>
"_Isn't this a lot of work? Testing all these libraries together..._" - Oh yes it is, but the good thing is that it's automated for the most part.<br>
"_How many people are working on this?_" - Maybe 7 or 8.<br>
"_How ofter are the libraries tested?_" - Every night there is a release, and every couple of weeks, there is an LTS (long term support) major release.<br>
"_Which one should I use?_" - the LTS version of course. Unless you are curious and want to see how a library is changing daily.<br>
"_But I have GHC installed globally on my computer. Is that used?_" - It depends. If the LTS version you specify in your project uses a different GHC version than what you have outside of Stack, that LTS specified GHC version will be installed.<br>
"_Give me an example!_" - Sure.<br>

First, let's see what is installed globally. When I run `which ghc` this is what I get: `/usr/local/bin/ghc`. And when I peek into this file, I see it points to my homebrew installed ghc, with version 8.4.3:

```shell
#!/bin/sh
exedir="/usr/local/Cellar/ghc/8.4.3/lib/ghc-8.4.3/bin"
exeprog="ghc-stage2"
executablename="$exedir/$exeprog"
datadir="/usr/local/Cellar/ghc/8.4.3/share"
bindir="/usr/local/Cellar/ghc/8.4.3/bin"
topdir="/usr/local/Cellar/ghc/8.4.3/lib/ghc-8.4.3"
executablename="$exedir/ghc"
exec "$executablename" -B"$topdir" ${1+"$@"}
```

Now when I run `ghc-pkg list`, I see 33 packages installed with this system-level GHC version:

```shell
% ghc-pkg list
/usr/local/Cellar/ghc/8.4.3/lib/ghc-8.4.3/package.conf.d
    Cabal-2.2.0.1
    array-0.5.2.0
    base-4.11.1.0
    binary-0.8.5.1
    ...
```

I have not installed any packages myself into this GHC version, all those 33 packages come with GHC.

I have a project where the resolver is `lts-11.11`. When I run `stack exec -- ghc-pkg list` in this project (after it was successfully built of course), the following libraries are listed. I left out the bulk of the libraries, as the key point here is the different layers and not what is in those:

```shell
% stack exec -- ghc-pkg list
/Users/adomokos/.stack/programs/x86_64-osx/ghc-8.2.2/lib/ghc-8.2.2/package.conf.d
    Cabal-2.0.1.0
    array-0.5.2.0
    base-4.10.1.0
    ...
/Users/adomokos/.stack/snapshots/x86_64-osx/lts-11.11/8.2.2/pkgdb
    Cabal-2.0.1.1
    HUnit-1.6.0.0
    StateVar-1.1.1.0
    aeson-1.2.4.0
    ...
/Users/adomokos/Projects/persistent-test/.stack-work/install/x86_64-osx/lts-11.11/8.2.2/pkgdb
    katip-0.5.5.1
    persistent-test-0.1.0.0
```

The 3 paths listed above in this shell snippet is where Haskell packages are pulled from:

1. Global - the system-level GHC packages list, Stack will never install anything into this
2. Snapshot - a database shared by all projects using the same snaphot
3. Local - Project specific database

But wait! What is GHC 8.2.2 doing there? I have version 8.4.3 installed at the system level. As it turns out, Stack, based on the LTS information uses a different version of GHC. I have GHC version 8.4.3 at the system level, but LTS-11.11 uses GHC version 8.2.2.

Let's prove that out further:

```shell
% stack exec -- which ghc
/Users/adomokos/.stack/programs/x86_64-osx/ghc-8.2.2/bin/ghc
% stack exec -- ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.2.2
```

Ha, Stack rolls its own GHC version and ignores the system-level version if it's different than what it needs. How cool is that!

When I went to Stackage's website, I noticed that a newer version of LTS was released recently. I had [LTS-11.11](https://www.stackage.org/lts-11.11) (released on 05/28/2018), but the latest version is (as of this writing of course) [LTS-11.13](https://www.stackage.org/lts-11.13) (released on 06/09/2018). I updated `stack.yaml` to use the newer version and rebuilt the project. Ran the app and everything worked properly.

What changed between the two LTS versions? Stackage.org has a very good comparison page, [this is](https://www.stackage.org/diff/lts-11.11/lts-11.13) where you can follow the diffs. It seems not many of the packages changed that I used, however, `postgresql-simple` went from 0.5.3.0 to 0.5.4.0. Since LTS-11.13 is specified in `stack.yaml` and that LTS needs `postgresql-simple` version 0.5.4.0, what happens when I specify version 0.5.3.0 in package.yaml?

I changed `package.yaml` this way:

```yaml
dependencies:
  - base >= 4.7 && < 5
  - postgresql-simple == 0.5.3.0
  ...
```

When I ran stack build, this friendly error message let me know that I'd like to use a version of a package that is not in the provided LTS version:

```shell
Error: While constructing the build plan, the following exceptions were encountered:

In the dependencies for persistent-test-0.1.0.0:
    postgresql-simple-0.5.4.0 from stack configuration
    does not match ==0.5.3.0  (latest matching version
                              is 0.5.3.0)
needed since persistent-test is a build target.

Some different approaches to resolving this:

  * Set 'allow-newer: true' to ignore all version
    constraints and build anyway.

  * Consider trying 'stack solver', which uses the cabal-install
    solver to attempt to find some working build
    configuration. This can be convenient when dealing with many
    complicated constraint errors, but results
    may be unpredictable.

  * Recommended action: try adding the following to your extra-deps
    in /Users/adomokos/Projects/persistent-test/stack.yaml:

- postgresql-simple-0.5.3.0

Plan construction failed.
```

Once I removed the version specification for the `postgresql-simple` package, it built successfully. But did it pick the correct version since I did not specify it?

```shell
% stack exec -- ghc-pkg list | grep postgresql-simple
    postgresql-simple-0.5.4.0
```

Yep, the correct, Stackage LTS-11.13 version was in fact installed.

I grabbed all the package names from LTS-11.13, I counted 2474 packages that got tested against each other for this particular LTS release. Kudos to the Stackage team for making sure we will only use packages that are playing nice with eachother!
