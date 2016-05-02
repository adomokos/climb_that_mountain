### Currying

I worked with a developer who had more experience with functional programming than I had. We worked on a Clojure project, and his deep Haskell background made him an expert in our team. This was especially revealing when we discussed partial functions and currying. I knew about them, but I've never used them in any of the apps I've worked on.

Fast forward 1 year, after I have a few months of learning and playing with Haskell, and I understand why: in Haskell, everything is curried. I repeat, everything. Even the function invocation is curried. No wonder, this was a second nature for that developer.

Let's look at a simple example:

```haskell
-- You can type this in GHCi or in TryHaslell
let multiply x y = x * y
let double = multiply 2
let triple = multiply 3

double 3
triple 4
```

Let me explain what we did here. We describe a multiply function by multiplying the two arguments it receives. In Haskell everything is curried, it's perfectly fine to invoke this function with only a single argument. What will I get back? Another function and not the result or an exception. And this is the key. Partially applying a function yields another function. Then we define two other functions, one that passes in 2 to double, the other 3 to triple whatever argument we pass to it.

What I was amazed by this was the easiness and the natural nature of Haskell's currying through partial application.

Let's see how this simple example would look in Clojure.

```clojure
(defn multiply [x y] (* x y))
(def double (partial multiply 2))
(def triple (partial multiply 3))

(double 3) ;; will yield 6
(triple 4) ;; will produce 12
```

This works, but yuck, I had to use a special language contract `partial` to signal, that the I'll be partially applying the `multiply` function. Based on Haskell, my intuition was to use `defn` for the double and triple functions, but that tripped me over as it did not work. I had to look up to see that I had to use `def` binding instead of `defn` to produce the partially applied function. In Haskell, everything just felt natural.

Although Ruby is a dynamically typed object-oriented language, it has many functional constructs that I enjoy using. I was curious, if Ruby supports currying. To my surprise, it does. Look at the same example with partial functions and currying in Ruby.

```ruby
multiply = -> (x, y) { x * y }
double = multiply.curry.(2)
triple = multiply.curry.(3)

double.(3) # will yield 6
triple.(4) # will produce 12
```
Well, this works, but I still have to push the boudaries a bit, it's not like I could partially apply the `multiply` lambda. Regardless, I am glad Ruby provides, this feature, even when it's unnatural to use it.
This is how it would feel natural:

```ruby
# This won't work
multiply = -> (x, y) { x * y }
double = multiply(2)
triple = multiply(3)
```



