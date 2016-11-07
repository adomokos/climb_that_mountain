## Recursion Done Right

Learning Haskell has influenced the way I think about code. I wrote about [currying befefore](http://www.adomokos.com/2016/05/currying-in-haskell-clojure-ruby-and.html) in various languages, but Haskell tought me a bit about how to do recursion properly.

Although fast paced, I really like the examples in the book [Learn you a Little Haskell for Great Good](http://learnyouahaskell.com/). As [one chapter](http://learnyouahaskell.com/recursion#maximum-awesome) talks about recursion and higher order functions, I was amazed by the simplicity of the code that lets you do basic list operations.

Here is how one could find the maximum of a list:

```haskell
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of an empty list"
maximum' (x:xs) = max x (maximum' xs)
```
There is already a `maximum` function in Haskell's core library, this example just shows you what you need to do to implement it yourself.

I am not going into details about the type declaration, but there are a couple of points I'd like to talk about.
The pattern matching in the second line checks for the case, where the collection is an empty array. When that happens, an exception is thrown.
The last line does pattern matching as well, it grabs the head and the tail of the list and saves it into the x and xs variables. Then it uses the `max` functions to figure out which number is greater: x or the recured result of maximum' with the tail of the list.
This is a prime example of declarative code, it's simplicity is striking and the fact that I don't have to know how max works with the recursion makes it a joy to read.

Let's look at another example. Here is how you could implement `map` in Haskell yourself:

```haskell
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs
```

Similarly, the edge-case is handled first. And the last line has the logic, function f is applied to the head of the list, the result is concatenated with the recurred result of `map'` of f function and the tail of the list.

All right, let's see how we could get this logic expressed in Ruby.

Here is my first attempt:
```ruby
module Collections
  def self.maximum(collection)
    head = collection.first
    tail = collection[1..-1]

    return 0 unless tail
    max head, (maximum tail)
  end

  def self.max(a, b)
    a > b ? a : b
  end
  private_class_method :max
end

RSpec.describe 'Recursion done right' do
   context 'maximum' do
     it 'returns an empty array as max of empty list' do
       expect(Collections.maximum([])).to eq(0)
     end

    it 'returns the maximum of a list' do
      expect(Collections.maximum([1,3,2])).to eq(3)
    end
  end
end
```

I did not find a `max` method in Ruby, I added that as a private class method. This is still pretty easy to read, but a bit more verbose than what I'd like it to be. I wanted to find the `(x:xs)` head-tail equivalent in Ruby, I knew that will be key to make it more succint solution. This is it: `(head, *tail) = collection`. I also had to change the guard to quit from the recursion to look for empty array, as the splat operator will provide that.

Here is my revised solution:

```ruby
module Collections
  def self.maximum(collection)
    (head, *tail) = collection

    return 0 if tail.empty?
    max head, (maximum tail)
  end
  ...
end
```
This is better, but the destructuring can take place in the arguments:
```ruby

module Collections
  def self.maximum((head, *tail))
    return 0 if tail.empty?
    max head, (maximum tail)
  end
  ...
end
```
Now this is pretty darn close to the solution in Haskell. I applied the head-tail (or [car-cdr](https://en.wikipedia.org/wiki/CAR_and_CDR)) concept.

Now let's look at the map function.
