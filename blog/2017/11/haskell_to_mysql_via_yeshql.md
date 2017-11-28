### Haskell to MySQL via Yeshql

As I was looking for an easy and light way to talk to Postgres from Clojure, I discovered [yesql](https://github.com/krisajenkins/yesql). I wanted to find a way to talk to a MySQL database and I found [yeshql](https://github.com/tdammers/yeshql). It's a template parsing library on top of [HDBC](https://github.com/ryantm/hdbc-mysql), exatly what I needed to keep SQL and Haskell code separate.

This blog post will show you how you can easily get yeshql up and running and run queries against MySQL. I'll build a simple CRUD console app, you should be able to take that and use code snippets in your apps.

The Clojure tutorial I created for my blog posts is named [Kashmir](https://github.com/adomokos/kashmir), I'll name this project Hashmir. You can follow along the evolution of the code via commit points, I'll post a link after each section.

You will need [stack](https://docs.haskellstack.org/en/stable/README/) and ghc installed, I have stack `Version 1.5.1 x86_64 hpack-0.17.1` and ghc version 8.0.2. MySQL is also needed, I have version 5.7.20, but I won't use anything fancy as far as the database goes, if you have MySQL installed, I am sure that will do it.

#### Generate the project with stack

Generate the project with stack: `stack new hashmir`. Go inside the directory, build and deploy the app with `stack build && stack install`. Run the skeleton app:

```shell
 % ~/.local/bin/hashmir-exe
someFunc
```
[Commit point](https://github.com/adomokos/hashmir/commit/de07eebdf3b0f40b550279a58231603eef4f4809)

#### Using hpack

Since I [learned](https://academy.mondaymorninghaskell.com/p/your-first-haskell-project) about [hpack](https://github.com/sol/hpack), I never touch a Haskell project's cabal file any more. This blog post assumes you are familiar with this tool, feel free to learn about hpack more before you proceed.
This is the hpack file I add to the project to generate the various sections in the cabal file.

Add this `package.yml` to the project's root directory:

```shell
name: hashmir
version: 0.1.0.0
author: Attila Domokos <adomokos@gmail.com>
maintainer: adomokos@gmail.com
copyright: 2017 Attila Domokos
category: Console App
homepage: https://github.com/adomokos/hashmir#readme

ghc-options: -Wall

dependencies:
  - base >= 4.7 && < 5

library:
  source-dirs: src/
  exposed-modules:
    - Lib

executables:
  hashmir-exe:
    source-dirs: app/
    main: Main.hs
    dependencies:
      hashmir
    ghc-options: -threaded -rtsopts -with-rtsopts=-N
```

Feel free to use your name and Github repo in this file.
Delete and regenarate the project's cabal file with this command: `rm -f hashmir.cabal && stack build`. `stack install` should produce the same executable file.

[Commit point](https://github.com/adomokos/hashmir/commit/873bacf0c76787e9e199f994ca43d6de2f67cf3a)

