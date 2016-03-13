## Thursday, December 26, 2013

### [Test Driving the FizzBuzz Kata In Clojure](http://www.adomokos.com/2013/12/test-driving-fizzbuzz-kata-in-clojure.html)

I've been enjoying my journey down the path of functional languages. I started with Clojure, than looked at Erlang, and now I am back at Clojure again. This post does not try to explain [why](http://www.quora.com/Clojure/Why-would-someone-learn-Clojure) you should look at functional languages, I assume you passed that since you're reading this blog post.

The books will teach you the basic concepts. Those ideas vanish fast unless I practice them with a project or some kind of challenge. Programming quizzes or coding puzzles are a good way to exercise a new language. I usually start with the simple ones, and I work my way towards the more complex projects. The [FizzBuzz kata](http://codingdojo.org/cgi-bin/wiki.pl?KataFizzBuzz) is on the easier side, I was even [asked to code it](http://www.adomokos.com/2013/02/crossroads.html) on a white board not too long ago.

Here is how I [solved](https://gist.github.com/adomokos/8140070) the FizzBuzz kata in Clojure test driving it with its own [test-is](http://clojuredocs.org/clojure_core/clojure.test/is) tool. I describe every changes I made, feel free to follow along by typing in the way I did it.

I'll assume you have Clojure and [Leiningen](http://leiningen.org/) installed on your computer. Getting these tools is simple thanks to [Homebrew](http://brew.sh/) on OSX. Please do a Google search if you don't have them already installed and you're using an OS other than OSX. I am using Clojure 1.5.1 version at the time of this writing.

I generated a skeleton project with Leiningen:`lein new app fizzbuzz`. The directory structure had a `src` and a `test` directory. `lein test` runs the tests and `lein run` executes the program.

I put all my code in the `src/fizzbuzz/core.clj` and in the `test/fizzbuzz/core_test.clj` files. I write code in Vim, and I use a vertical split to look at the test and program files in the same window. I usually have the test on the left, and the code under test on the right hand side.

![fizzbuzz_kata_in_vim](/resources/2013/12/fizzbuzz_kata_in_vim.png)

Here is the first failing test I wrote:

```clojure
(ns fizzbuzz.core-test
  (:require [clojure.test :refer :all]
            [fizzbuzz.core :refer :all]))

(deftest fizz-buzz-printer-test
  (testing "with 1"
    (is (= "1\n" (fizz-buzz-printer 1)))))
```

I provided the skeleton for the `fizz-buzz-printer` function this way:

```clojure
(ns fizzbuzz.core
  (:gen-class))

(defn fizz-buzz-printer [limit])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))
```

When I ran the test with `lein test`, this is the error I had:

FAIL in (fizz-buzz-printer-test) (core_test.clj:7)
with 1
expected: (= "1\n" (fizz-buzz-printer 1))
  actual:
 (not (= 1
 nil))

<span style="color: #C00;">Ran 1 tests containing 1 assertions.</span>
<span style="color: #C00;">1 failures, 0 errors.</span>
Tests failed.

The simplest thing that could possibly work is this:

```clojure
(ns fizzbuzz.core
  (:gen-class))

(defn fizz-buzz-printer [limit]
  "1\n")

...
```

Please note that I omitted the `-main` function as it's not changed yet. I ran the tests, and it all passed:

<span style="color: #090;">Ran 1 tests containing 1 assertions.</span>
<span style="color: #090;">0 failures, 0 errors.</span>

Great! Now I am on to the next test:

```clojure
...

(deftest fizz-buzz-printer-test
  (testing "with 1"
    (is (= "1\n" (fizz-buzz-printer 1))))
  (testing "with 2"
    (is (= "1\n2\n" (fizz-buzz-printer 2)))))
```

The following code change made the test pass:

```clojure
...

(defn fizz-buzz-printer [limit]
  (if (= 2 limit)
    "1\n2\n"
    "1\n"))

...
```

A conditional won't serve me long to print out numbers. I refactored the function this way:

```clojure
...

(defn fizz-buzz-printer
  ([output current limit]
    (let [product (str output current "\n")]
      (if (< current limit)
        (fizz-buzz-printer product (+ 1 current) limit)
        product)))
  ([limit]
    (fizz-buzz-printer "" 1 limit)))

...
```

This might need a little explanation. I chose recursion instead of using a loop. The `fizz-buzz-printer` function is overloaded: it accepts 1 (the limit) or 3 arguments (the output, current value and limit). The formatted string is captured in the `product` value. If the current value in the cycle is less than the limit I call the same function but the current value is incremented by one, and if not, I know I reached the limit and just return the formatted string - the product.

With this code I should be able to print the numbers on the screen followed by a line break. I wrote a quick - temporary - test to verify that (see the last assert):

```clojure
...

(deftest fizz-buzz-printer-test
  (testing "with 1"
    (is (= "1\n" (fizz-buzz-printer 1))))
  (testing "with 2"
    (is (= "1\n2\n" (fizz-buzz-printer 2))))
  (testing "with 4"
    (is (= "1\n2\n3\n4\n" (fizz-buzz-printer 4)))))
```

I was ready for the fun part of this kata! For every number divisible by 3 I had to print out the word: "Fizz".
First I wrote the failing test:

```clojure
...

(deftest fizz-buzz-printer-test
  (testing "with 1"
    (is (= "1\n" (fizz-buzz-printer 1))))
  (testing "with 2"
    (is (= "1\n2\n" (fizz-buzz-printer 2))))
  (testing "from 1-3"
    (is (= "1\n2\nFizz\n" (fizz-buzz-printer 3)))))
```

The error was obvious:

FAIL in (fizz-buzz-printer-test) (core_test.clj:11)
from 1-3
expected: (= "1\n2\nFizz\n" (fizz-buzz-printer 3))
actual: (not (= "1\n2\nFizz\n" "1\n2\n3\n"))

<span style="color: #C00;">Ran 1 tests containing 3 assertions.</span>
<span style="color: #C00;">1 failures, 0 errors.</span>
Tests failed.

I was expecting the word `Fizz`, but I was getting the number 3\. That logic was missing. Here is how I added that:

```clojure
...

(defn fizz-buzz-printer
  ([output current limit]
    (let [product (if (= 3 current)
                    (str output "Fizz\n")
                    (str output current "\n"))]
      (if (< current limit)
        (fizz-buzz-printer product (+ 1 current) limit)
        product)))
  ([limit]
    (fizz-buzz-printer "" 1 limit)))

...
```

I felt generating the formatted string - the product value - put way too much logic in the let function. I extracted out that logic into its own function called `format-output`:

```clojure
...

(defn- format-output [output current]
  (if (= 3 current)
    (str output "Fizz\n")
    (str output current "\n")))

(defn fizz-buzz-printer
  ([output current limit]
    (let [product (format-output output current)]
      (if (< current limit)
        (fizz-buzz-printer product (+ 1 current) limit)
        product)))
  ([limit]
    (fizz-buzz-printer "" 1 limit)))

...
```

The function `format-output` is private, it's only visible within its own namespace thanks to declaring it with [defn-](http://clojuredocs.org/clojure_core/clojure.core/defn-).
The increased complexity in the let function triggered the "extract function" refactoring. I felt the `format-output` function should be responsible for deciding if it has to print the number or the words "Fizz", "Buzz" or "FizzBuzz".

The next test was simple, I wanted to make sure the logic worked from zero to four. I did not have to modify the code for it:

```clojure
...

(deftest fizz-buzz-printer-test
  (testing "with 1"
    (is (= "1\n" (fizz-buzz-printer 1))))
  (testing "with 2"
    (is (= "1\n2\n" (fizz-buzz-printer 2))))
  (testing "from 1-3"
    (is (= "1\n2\nFizz\n" (fizz-buzz-printer 3))))
  (testing "from 1-4"
    (is (= "1\n2\nFizz\n4\n" (fizz-buzz-printer 4)))))
```

I reached a point with my tests where I had to make my code a bit easier to test. I did not want to write tests that compares more than 3 numbers from the `fizz-buzz-printer`. I felt 3 is a large enough set to verify the boundaries. I added a new test for this:

```clojure
...

(deftest fizz-buzz-printer-test
  (testing "with 1"
    (is (= "1\n" (fizz-buzz-printer 1))))
  (testing "with 2"
    (is (= "1\n2\n" (fizz-buzz-printer 2))))
  (testing "from 1-3"
    (is (= "1\n2\nFizz\n" (fizz-buzz-printer 3))))
  (testing "from 1-4"
    (is (= "1\n2\nFizz\n4\n" (fizz-buzz-printer 4))))
  (testing "from 3-4"
    (is (= "Fizz\n4\n" (fizz-buzz-printer 3 4)))))
```

Since I did not have the function override for 2 arguments, the test failed:

<span style="color: #C00;">ERROR in (fizz-buzz-printer-test) (AFn.java:437)</span>
from 3-4
expected: (= "Fizz\n4\n" (fizz-buzz-printer 3 4))
  actual: clojure.lang.ArityException: Wrong number of args (2) passed to: core$fizz-buzz-printer

I added the function override which looked almost identical to the function with one argument:

```clojure
...

(defn fizz-buzz-printer
  ([output current limit]
    (let [product (format-output output current)]
      (if (< current limit)
        (fizz-buzz-printer product (+ 1 current) limit)
        product)))
  ([start limit]
   (fizz-buzz-printer "" start limit)) ; added function with 2 arguments
  ([limit]
    (fizz-buzz-printer "" 1 limit)))

...
```

All the tests passed. However, this code had duplication: the unary function (method with one argument) initialized the output just like the binary function. I decided to remove this duplication by changing the unary function to call the binary function that calls the ternary one.

```clojure
...

(defn fizz-buzz-printer
  ([output current limit]
    (let [product (format-output output current)]
      (if (< current limit)
        (fizz-buzz-printer product (+ 1 current) limit)
        product)))
  ([start limit]
   (fizz-buzz-printer "" start limit))
  ([limit]
    (fizz-buzz-printer 1 limit))) ; does not initializes the output

...
```

I was now ready to zoom into the ranges where the program had to produce the different words instead of numbers. The next test did just that:

```clojure
...

(deftest fizz-buzz-printer-test
  (testing "with 1"
    (is (= "1\n" (fizz-buzz-printer 1))))
  (testing "with 2"
    (is (= "1\n2\n" (fizz-buzz-printer 2))))
  (testing "from 1-3"
    (is (= "1\n2\nFizz\n" (fizz-buzz-printer 3))))
  (testing "from 1-4"
    (is (= "1\n2\nFizz\n4\n" (fizz-buzz-printer 4))))
  (testing "from 3-4"
    (is (= "Fizz\n4\n" (fizz-buzz-printer 3 4))))
  (testing "from 4-5"
    (is (= "4\nBuzz\n" (fizz-buzz-printer 4 5)))))
```

I did the simplest thing that could possibly work: I used a nested conditional:

```clojure
...

(defn- format-output [output current]
  (if (= 3 current)
    (str output "Fizz\n")
    (if (= 5 current) ; introduced this nested conditional
      (str output "Buzz\n")
      (str output current "\n"))))

...
```

Everything passed again, but this code was ugly. Here is how I used [cond](http://clojuredocs.org/clojure_core/clojure.core/cond) to refactor it:

```clojure
...

(defn- format-output [output current]
  (cond
    (= 3 current) (str output "Fizz\n")
    (= 5 current) (str output "Buzz\n")
    :else (str output current "\n")))

...
```

My next test covered the range from 4-6, where the word "Fizz" should be printed out for the number 6.

```clojure
...

(deftest fizz-buzz-printer-test
  (testing "with 1"
    (is (= "1\n" (fizz-buzz-printer 1))))
  (testing "with 2"
    (is (= "1\n2\n" (fizz-buzz-printer 2))))
  (testing "from 1-3"
    (is (= "1\n2\nFizz\n" (fizz-buzz-printer 3))))
  (testing "from 1-4"
    (is (= "1\n2\nFizz\n4\n" (fizz-buzz-printer 4))))
  (testing "from 3-4"
    (is (= "Fizz\n4\n" (fizz-buzz-printer 3 4))))
  (testing "from 4-5"
    (is (= "4\nBuzz\n" (fizz-buzz-printer 4 5))))
  (testing "from 4-6"
    (is (= "4\nBuzz\nFizz\n" (fizz-buzz-printer 4 6)))))
```

This test failed again as I was only checking for the number three and not for numbers divisible by three without a remainder. I changed the `format-output` function to use the modulus operator:

```clojure
...

(defn- format-output [output current]
  (cond
    (= 0 (mod current 3)) (str output "Fizz\n")
    (= 5 current) (str output "Buzz\n")
    :else (str output current "\n")))

...
```

All the tests now passed again.

I suspected the same error at number 10, therefore my next test was around it (between 9 and 11):

```clojure
...

(deftest fizz-buzz-printer-test
  (testing "with 1"
    (is (= "1\n" (fizz-buzz-printer 1))))
  (testing "with 2"
    (is (= "1\n2\n" (fizz-buzz-printer 2))))
  (testing "from 1-3"
    (is (= "1\n2\nFizz\n" (fizz-buzz-printer 3))))
  (testing "from 1-4"
    (is (= "1\n2\nFizz\n4\n" (fizz-buzz-printer 4))))
  (testing "from 3-4"
    (is (= "Fizz\n4\n" (fizz-buzz-printer 3 4))))
  (testing "from 4-5"
    (is (= "4\nBuzz\n" (fizz-buzz-printer 4 5))))
  (testing "from 4-6"
    (is (= "4\nBuzz\nFizz\n" (fizz-buzz-printer 4 6))))
  (testing "from 9-11"
    (is (= "Fizz\nBuzz\n11\n" (fizz-buzz-printer 9 11)))))
```

The test failed, I received 10 instead of "Buzz", I adjusted the second conditional to use the `modulus` function as well:

```clojure
...

(defn- format-output [output current]
  (cond
    (= 0 (mod current 3)) (str output "Fizz\n")
    (= 0 (mod current 5)) (str output "Buzz\n")
    :else (str output current "\n")))

...
```

This was great! Now I had to write code for one more scenario: for the numbers divisible by 5 and 3 I had to print the word "FizzBuzz" instead of "Fizz" or "Buzz". My last test did just that:

```clojure
...

(deftest fizz-buzz-printer-test
  (testing "with 1"
    (is (= "1\n" (fizz-buzz-printer 1))))
  (testing "with 2"
    (is (= "1\n2\n" (fizz-buzz-printer 2))))
  (testing "from 1-3"
    (is (= "1\n2\nFizz\n" (fizz-buzz-printer 3))))
  (testing "from 1-4"
    (is (= "1\n2\nFizz\n4\n" (fizz-buzz-printer 4))))
  (testing "from 3-4"
    (is (= "Fizz\n4\n" (fizz-buzz-printer 3 4))))
  (testing "from 4-5"
    (is (= "4\nBuzz\n" (fizz-buzz-printer 4 5))))
  (testing "from 4-6"
    (is (= "4\nBuzz\nFizz\n" (fizz-buzz-printer 4 6))))
  (testing "from 9-11"
    (is (= "Fizz\nBuzz\n11\n" (fizz-buzz-printer 9 11))))
  (testing "from 14-16"
    (is (= "14\nFizzBuzz\n16\n" (fizz-buzz-printer 14 16)))))
```

The error was what I expected: the program only printed out the word "Fizz" instead of "FizzBuzz". Adding the modulus check for 15 made this last test to pass:

```clojure
...

(defn- format-output [output current]
  (cond
    (= 0 (mod current 15)) (str output "FizzBuzz\n")
    (= 0 (mod current 3)) (str output "Fizz\n")
    (= 0 (mod current 5)) (str output "Buzz\n")
    :else (str output current "\n")))

...
```

All my tests passed!

Ran 1 tests containing 9 assertions.
0 failures, 0 errors.

I also wanted to see the program in action by running it with `leain run`. Here is how I changed the `main` function:

```clojure
...

(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (print (fizz-buzz-printer 100)))
```

And that's it! Please take a look at the final solution in [this Gist](https://gist.github.com/adomokos/8140070).
Hope you had as much fun reading this as I had preparing it.

Happy New Year everyone!


POSTED BY ATTILA DOMOKOS AT 8:57 PM


NO COMMENTS
