### Currying

I worked with a developer about a year ago, who had more experience with functional programming than I had. We worked on a Clojure project, and his deep Haskell background made him an expert on our team. This was especially revealing when we discussed partial function applications and currying. I was vaguely familiar with the concept, but I've never used them in any of the apps I've worked on.

Fast forward 1 year, after learning and playing with Haskell for a few months, I understand why: in Haskell, everything is curried. I repeat: everything. Even the function invocation is curried. In fact, you have to work hard if you want it differently. No wonder, this was so obvious for that developer.

Let's look at a simple example:

```haskell
-- You can type this in GHCi or in TryHaslell.com
let multiply x y = x * y
let double = multiply 2
let triple = multiply 3

double 3
triple 4
```

Let me explain what I did here. I created a multiply function that takes two arguments and multiplies them. In Haskell everything is curried, it's perfectly fine to invoke this function with only a single argument. What will I get back? Another function and not an exception. This was the breakthrough for me: partially applying a function yields another function. Then I defined two other functions, one that passes in 2 to double, the other 3 to triple whatever argument we pass to it.

What I was amazed by this was the easiness and the natural nature of Haskell's currying through partial application.

Let's see how this simple example would look in Clojure.

```clojure
(defn multiply [x y] (* x y))
(def double (partial multiply 2))
(def triple (partial multiply 3))

(double 3) ;; will yield 6
(triple 4) ;; will produce 12
```

This works, but yuck, I had to use a special language contsract `partial` to signal, that I'll be partially applying the `multiply` function. Based on the Haskell example, my intuition was to use `defn` for the double and triple functions, but that tripped me over, it did not work. I had to "StackOverflow" it to realize, that the `def` binding is needed instead of `defn` to produce the partially applied function. In Haskell, everything felt natural.

Although Ruby is a dynamically typed object-oriented language, it has many functional constructs that I enjoy using. I was curious, if Ruby supports currying. To my surprise, it does. Look at the same example with partial functions and currying in Ruby.

```ruby
multiply = -> (x, y) { x * y }
double = multiply.curry.(2)
triple = multiply.curry.(3)

double.(3) # will yield 6
triple.(4) # will produce 12
```
Well, this works, but it's far from Haskell's obvious nature, where I did not have to use any kind of special keywords to achieve the same result.

Here is how I would write this with "programming by wishful thinking":

```ruby
# This won't work
multiply = -> (x, y) { x * y }
double = multiply(2)
triple = multiply(3)
```

I am sure the Ruby language authors had a reason to use `curry` for partial applications, but it just did not feel natural. I have to learn and remember how to use it properly.

There are currying related npm packages in Node, but I have not found anything that's built into the language. Here is how the poor man's currying is done in JavaScript:

```javascript
var multiply = function(x) {
  return function(y) {
    return x * y;
  }
}

var double = function(2);
var triple = function(3);

double(3); // will yield 6
triple(4); // will produce 12
```
I like JavaScript's "functions are first class citizens" nature, I am sure once ES6 or 7 gets widely adopted, it will be a language I'll enjoy using in the future.

