## Recursion Done Right

Learning Haskell has influenced the way I think about code. I wrote about [currying](http://www.adomokos.com/2016/05/currying-in-haskell-clojure-ruby-and.html) before in various languages, but Haskell taught me a bit about how to do recursion properly.

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
The last line does pattern matching as well, it grabs the head and the tail of the list and saves it into the x and xs variables. Then it uses the `max` functions to figure out which number is greater: x or the recurred result of maximum' with the tail of the list.
This is a prime example of declarative code, its simplicity is striking and the fact that I don't have to know how max works with the recursion makes it a joy to read.

Let's look at another example. Here is how you could implement `map` in Haskell yourself:

```haskell
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs
```

Similarly, the edge-case is handled first. The last line has the logic, function f is applied to the head of the list, the result is concatenated with the recurred result of `map'` of f function and the tail of the list.

All right, let's see how we could express this logic in Ruby.

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

I did not find a `max` method in Ruby, I added that as a private class method. This is still pretty easy to read, but a bit more verbose than what I'd like it to be. I wanted to find the `(x:xs)` head-tail ([car-cdr](https://en.wikipedia.org/wiki/CAR_and_CDR) for you LiSP folks) equivalent in Ruby, I knew that will be key to make it a more succinct solution. This is it: `(head, *tail) = collection`. I also had to change the guard to quit from the recursion to look for an empty array, as the splat operator will provide that.

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
This is pretty darn close to the solution in Haskell.
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
My implementation of `map` takes a lambda with one argument, which multiplies that one argument by three, and the second argument is the collection of items the map function will operate on.

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
The key to make it succinct is the destructuring the collection argument into head and tail. The guard statement makes sure the recursion will quit once there is no item in the head. The bulk of the logic is the last line of the method: the lambda is applied to the head, it's converted into an array and that value is concatenated with the result of the recurred result of the lambda and the rest of the collection.

In our case, the following calculation takes place:
```
map (*3) [1,2,3]
[(3*1)] + map (*3) [2,3]
[(3*1)] + [(3*2)] + map (*3) [3]
[(3*1)] + [(3*2)] + [(3*3)]
[3] + [6] + [9]
[3,6,9]
```

Haskell takes pride in how easily it implements the [quicksort algorithm](https://en.wikipedia.org/quick_sort_algorithm). Let's see how it's done there:

```haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted
```
I don't blame you if this seems to be a bit more cryptic than you wanted to be. It take a little practice to read what is really going on here. I'll explain it, as it will help our own Ruby implementation. The first line is the type declaration, ignore that for now. The second line is the guard, sorting an empty array will give you back an empty array. The meat of the logic begins on the third line. The collection argument is destructured into head and tail, just like I've been doing in the examples above. Based on the head value, we are filtering the elements into smaller-equal, and bigger parts. We do all this recursively until the list is exhausted. Right before the result is returned, the three items, the smaller sorted, the head value and the bigger sorted elements are combined into one collection.

Let's see how this is done in Ruby. Here are the specs I prepared to prove the logic:

```ruby
context 'quicksort' do
  it 'returns an empty list for empty list' do
    expect(Collections.quicksort([])).to eq([])
  end
  it 'sorts a list of items' do
    expect(Collections.quicksort([2,5,3])).to eq([2,3,5])
  end
end
```

Here is how I'd like the code to be:

```ruby
def self.quicksort((head, *tail))
  return [] unless head

  smaller_sorted = quicksort(Collections.filter(->(x) { x <= head }, tail))
  bigger_sorted = quicksort(Collections.filter(->(x) { x > head }, tail))
  smaller_sorted + [head] + bigger_sorted
end
```

This logic is very close to the Haskell example, but unfortunately, I don't have the filter function just yet. (Ruby standard library offers the `select` method on enumerables, but let's keep these examples free from all that.) `filter` takes a lambda as its predicate function, and a collection it needs to operate on.
This spec proves out our logic:

```ruby
context 'filter' do
  specify 'filter (>2) [] returns an empty list' do
    expect(Collections.filter(->(x){ x > 2 }, [])).to be_empty
  end
  specify 'filter (>2) [1,3,5] returns [3,5]' do
    expect(Collections.filter(->(x){ x > 2 }, [1,3,5])).to eq([3,5])
  end
end
```

And the implementation is similar what you've seen before:

```ruby
def self.filter(f, (head, *tail))
  return [] unless head

  if f.(head)
    [head] + filter(f, tail)
  else
    filter(f, tail)
  end
end
```

And now, when you run the entire spec, the quicksort implementation just magically works.

<img src="/resources/2016/11/specs_executed.png" width="440" height="187">

Studying Haskell taught me a few things about recursion. The head and tail concept is essential to make the code simple and neat. Without that it would have been a lot more noisier. Whenever I used recursion before, I always felt I needed an accumulator. I wanted something I could jump to and investigate when something went wrong. I would have written the filter function like this before:

```ruby
def self.filter(f, (head, *tail), accumulator=[])
  return accumulator unless head

  accumulator << head if f.(head)

  filter(f, tail, accumulator)
end
```
Although this works, adding the accumulator with a default argument to the list just makes this code a lot noisier, but I do like not having conditional branches in it, it's just easier to reason about this code.  
You can review the examples in [this gist](https://gist.github.com/adomokos/cc326bf27b0529d9386813c8af66e59c).

Based on what you read here, try implementing `replicate`, `take`, `reverse`, `repeat` and `zip` functions yourself. In case you need directions, check out this [gist](https://gist.github.com/adomokos/3182e9914ab29b5a36c444a16ae7ecd1) to see how I did it.
