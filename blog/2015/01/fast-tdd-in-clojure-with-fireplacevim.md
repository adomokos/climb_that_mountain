## Thursday, January 1, 2015

### [Fast TDD in Clojure with Fireplace.vim](http://www.adomokos.com/2015/01/fast-tdd-in-clojure-with-fireplacevim.html)

I've been looking at Clojure for the past 18 months. I prefer learning and practicing by [writing a failing test first](http://www.adomokos.com/2014/02/learning-clojure-one-failing-test-at.html), but unfortunately, the feedback loop through TDD in Clojure is slow. How slow? Well, it's between 4-10 seconds depending on the size of the project. I am still new to the language, I want to tweak my code a tiny bit and see if that change broke my tests. I am used to Ruby's almost immediate test execution, and the long wait for running the tests in Clojure makes it less effective.

In Ruby Land, I am used to running a large number of tests (958 examples in our application) in about 3.8 seconds. In a brand new Clojure project, it takes about 4 seconds to run the only failing test. This is no surprise: Clojure code has to be be compiled to Java byte code, where the compilation takes time.

I bumped into [Ben Orenstein](https://twitter.com/r00k)'s great "[Tips for Clojure Beginners](http://robots.thoughtbot.com/tips-for-clojure-beginners)" blog post a few weeks ago. It's a must read if you're new to Clojure. Vim is my preferred editor, and he wrote about a vim plugin by [Tim Pope](https://twitter.com/tpope), called [fireplace.vim](https://github.com/tpope/vim-fireplace). I remember looking at it briefly, but for some reason, I did not give it a try at that time.

A few days later I hacked on some code in Clojure again, and it reached a point where I threw my hands in the air and declared: "enough is enough!" I caught myself checking out Twitter and other websites as I had to wait about 10 seconds to run the tests after a simple change. I went through [this blog post](http://loobyonrails.com/blog/2013/08/07/getting-started-with-tdd-and-clojure/), where the author talks about using fireplace.vim for test execution. I gave it a try, and there is no turning back!

I installed fireplace.vim with [pathogen](https://github.com/tpope/vim-pathogen). I opened another tab in my terminal, navigated to the root directory of my Clojure project. Fired up `lein repl` there and noted what the port number was.

![nrepl_port](/resources/2015/01/nrepl_port.png)

In this case, 53844 was the port number for the nREPL server. I connected to that from my vim session in the other terminal tab by typing the vim command `:Connect`.

![connect_to_nrepl_server](/resources/2015/01/connect_to_nrepl_server.png)

Fireplace gently investigated which nREPL server I wanted to connect to. I chose (the obvious) option one, it used localhost and I had to provide the port number from the other tab, which was 53844.

![full_connect_in_vim](/resources/2015/01/full_connect_in_vim.png)

I submitted this option, and I was connected to the nREPL in the other tab. Fireplace lets me run the tests in the currently selected pane by using the `:RunTests` command. I did that, and much to my surprise the tests executed almost instantaneously. I did it once more (or maybe 5 times) just for the heck of it! This is what I found in the quickfix list:

![quickfix_list](/resources/2015/01/quickfix_list.png)

I made the test pass, the output was terse. I guess there isn't much to say when all my expectations are passing. I included an animated gif here to show you what it feels like running the tests. Super sweet, don't you think!?

![running-the-tests](/resources/2015/01/running-the-tests.gif)

When I change a Clojure file in a different buffer (other than the buffer where my tests are), I need to `Require!` those files again. I get around this by writing all my functions right above the tests in the same buffer, and moving them to their final place when I feel confident about them.

There is an easier way to connect to a REPL by using fireplace.vim's `:Piggieback!` command. Please read the [docs](https://github.com/tpope/vim-fireplace/blob/master/doc/fireplace.txt) of this great vim plugin, that's how you can learn all the other features (like macroexpand) I have not described in this blog post.

My personal shortcut to run the tests is `,r`. Setting it up with vim was easy:
`:nmap ,r :RunTests<CR>`. With this change, I had the same joy in Clojure as I've had with Ruby and RSpec for years. Bye-bye checking out while I am test driving my code in Clojure!

**Update on 01/31/2015**

I've been using this keybinding with fireplace in vim recently: `:nmap ,r :Require! <bar> Eval (clojure.test/run-tests)<CR>`. It picks up any changes I make in the source and the test files as I require packages before every test run. I'd recommend giving this a try.


POSTED BY ATTILA DOMOKOS AT 2:26 PM


NO COMMENTS
