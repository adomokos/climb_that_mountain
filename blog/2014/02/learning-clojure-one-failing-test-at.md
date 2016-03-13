## Saturday, February 22, 2014

### [Learning Clojure One Failing Test at a Time](http://www.adomokos.com/2014/02/learning-clojure-one-failing-test-at.html)

I learn a new language just like I discover a new algorithm in a language I already know: one failing test at a time. The advantage of discovering a language this way is deeper learning. If I read something in a book, try it in the [REPL](http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop), I forget it by next week. However, writing a failing test first, describing what a function should do, I realized my learning is deeper.

I also like to keep around things I try as a reference. Once I type it in the REPL, unless I save my session somehow, the information goes away. I am unhappy when I make a mistake on the third line in the REPL, and I have to start the whole thing over. I also caught myself looking up things in tests I tried a few weeks ago.

I can try this in the REPL:

```clojure
user => (conj [1 2 3] 4 5)
  [1 2 3 4 5]
```

Or I could fire up a new project using Leiningen with `lein new learning`, add a new test `touch test/learning/vector_tests.clj` and do something similar in a test:

```clojure
(ns learning.vector-test
  (:require [clojure.test :refer :all]))

(deftest a-vector-test
  (testing "conjoining elements to a vector"
    (is (= [1 2 3 4 5] (conj [1 2 3] 4 5)))))
```

I run the test and everything works:

```bash
  lein test learning.vector-test

  Ran 1 tests containing 1 assertions.
  0 failures, 0 errors.
```

The next thing I try is removing the last element from a vector. The Clojure [pop](http://clojuredocs.org/clojure_core/clojure.core/pop) function does just that. Here is what I add to my test:

```clojure
(testing "removing the last element from a vector"
  (is (= [1 2 3] (pop [1 2 3 4])))))
```

You see where this is going. It's a bit more typing, but I'm building up my own reference guide as I learn new things. I also get a deeper knowledge faster by practicing this way.

Why Clojure’s own test framework does not have a colored output is beyond me. I am not ready to send a pull request for it just yet. But if you are as bothered as I am to look at a non-colored test reporter, you need to install [difftest](https://github.com/brentonashworth/lein-difftest) as a Leiningen plugin. You need to create or edit your `~/.lein/profiles.clj` file as it's described [here](https://github.com/brentonashworth/lein-difftest#leiningen-usage).

Once you successfully installed the plugin, you go from this:

![lein test](/resources/2014/02/lein_test.png)

To that:

![lein_difftest](/resources/2014/02/lein_difftest.png)

The REPL is great for trying short things quickly. As soon as my example reaches the second line I question myself if I really should continue it here or just put the code in a test. I don't enjoy using the REPL as an editor, but as a tool where I can try things, it works!


POSTED BY ATTILA DOMOKOS AT 2:09 PM


NO COMMENTS
