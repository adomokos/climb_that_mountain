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
This is pretty darn close to the solution in Haskell. I applied the head-tail (or [car-cdr](https://en.wikipedia.org/wiki/CAR_and_CDR)) concept.

Now let's look at the map function.

These specs describe the behavior:

```ruby
context 'map' do
  it 'mapping [] with (*3) gives []' do
    expect(Collections.map(->(x){ x*3 }, [])).to be_empty
  end
  it 'mapping [1,2,3] with (*3) gives [1,6,9]' do
    expect(Collections.map(->(x){ x*3 }, [1,2,3])).to eq([3,6,9])
  end
end
```
My implementation of `map` takes a lambda with one argument, which multiplies that one argument by three, and the second argument is the collction of items the map function will operate on.

This is my implementation for it:

```ruby
module Collections
  def self.map(f, (head, *tail))
    return [] unless head

    [f.(head)] + map(f, tail)
  end
  ...
end
```
The key to make it succint is the destructuring the collection argument into head and tail. The guard statement makes sure the recursion will quit once there is no item in the head. The bulk of the logic is the last line of the method: the lambda is applied to the head, it's converted into an array and that value is concatenated with the result of the recurred result of the lambda and the rest of the collection.

In our case, the following calculation takes place:
```
map (*3) [1,2,3]
[(3*1)] + map (*3) [2,3]
[(3*1)] + [(3*2)] + map (*3) [3]
[(3*1)] + [(3*2)] + [(3*3)]
[3] + [6] + [9]
[3,6,9]
```

Haskell takes pride in how easily it implements the [quick sort algorithm](https://en.wikipedia.org/quick_sort_algorithm). Let's see how it's done there:

```haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted
```
I don't blame you if this seems to be a bit more criptic than you wanted to be. It take a little practice to read what is really going on here. I'll explain it, as it will help our own Ruby implementation. The first line is the type declaration, ignore that for now. The second line is the guard, sorting an empty array will give you back an empty array. The meat of the logic begins on the third line. The collection argument is destructured into head and tail, just like I've been doing in the examples above. Based on the head value, we are filtering the elements into smaller-equal, and bigger parts. We do all this recursively until the list is exhausted. Right before the result is returned, the three items, the smaller sorted, the head value and the bigger sorted elements are combined into one collection.

Let's see how this is done in Ruby. Here are the specs I prepared to prove this logic:
```ruby
  context 'quick sort' do
    it 'returns an empty list for empty list' do
      expect(Collections.quicksort([])).to eq([])
    end
    it 'sorts a list of items' do
      expect(Collections.quicksort([2,5,3])).to eq([2,3,5])
    end
  end
```
