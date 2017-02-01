## Monday, January 23, 2017

### Go

The first time I heard about Golang was a few years back, when the great guys at Brad's Deals, our nextdoor office neighbor organized and hosted the local Go meetup there. Then IO.js and Node.js war broke out and TJ Holowaychuck shifted from Node.js to Golang announcing the move in a [open letter](https://medium.com/@tjholowaychuk/farewell-node-js-4ba9e7f3e52b#.fada6ndrw) to the community.
I did not think much of the language, as its reputation was far from the beauty of a real functional language.

Fast forward a couple of years and I am giving Ruby a serious try on AWS Lambda. [Ruby works there](http://www.adomokos.com/2016/06/using-ruby-with-activerecord-in-aws.html), however, it needs enough memory and 3000 ms (3 seconds) to do anything. We have to invoke some of them millions of times in a month and when we [calculated](https://s3.amazonaws.com/lambda-tools/pricing-calculator.html) the cost for it, the bill gets fairly large quickly. 

Then one night I wrote a tiny Go program:

```go
package main

import "fmt"

func main() {
  fmt.Println("Hello, World!")
}
```

I cross compiled (since I am working on OSX) with the command `GOOS=linux GOARCH=amd64 go build github.com/adomokos/hello` to Linux, packaged it up with a Node.JS executor and ran it. I couldn't believe my eyes, it took only 34 ms to get the string `Hello, World!` back. 34 ms! And it was on a 128 MB memory instance. It was beautiful! Ruby would need four times the memory and it would still execute ~10 times slower than Go. That was the moment when I got hooked.


Go is a simple language. I am not saying it's easy to learn, it's subjective: it depends on your background, your experience. But it's far from the beauty of Haskell or Clojure. However, I know, that the team I am working with would have no trouble switching between Go and Ruby during the day.

What kind of a language today does not have `map` or `reduce` functions?! Especially when functions are first class citizens in the language. It turns out, I can write my own map function if I need to:

```go
package collections

import (
  "github.com/stretchr/testify/assert"
  "strconv"
  "testing"
)

func fmap(f func(int) string, numbers []int) []string {
  items := make([]string, len(numbers))

  for i, item := range numbers {
    items[i] = f(item)
  }

  return items
}

func TestFMap(t *testing.T) {
  numbers := []int{1, 2, 3}
  result := fmap(func(item int) string { return strconv.Itoa(item) }, numbers)
  assert.Equal(t, []string{"1", "2", "3"}, result)
}
```

Writing `map` with recursion would be more elegant, but it's not as performant as using another array with defined length that does not have to grow during the operation. Grow? Yes, take a look at the differences between slices and arrays.

#### History

Go was created by some very smart people at Google, I wanted to understand their decision to keep a language this pure.
Google has a large amount of code in C and C++, however, those languages are far from modern concepts, like parallel execution and web programming to name a few. Those languages were created in the 60s and 80s, well before the era of multi-core processors and the Internet. Compiling a massive codebase in C++ can easily take hour(s), and while they were waiting for compilation, the idea of a fast compiling, simple, modern language idea was born. Go does not aim to be shiny and nice, no, its designers kept it:

* to be simple and easy to learn
* to compile fast
* to run fast
* to make parallel processing easy

Google hires massive number of fresh CS graduates each year with some C++ and Java programming experience, these engineers can feel right at home with Go, where the syntax and concept is similar to those languages.

#### Tooling

Go comes with many built-in tool, like code formating and benchmarking to name the fiew. In fact I set up [Vim Go](https://github.com/fatih/vim-go) that leverages many of those tools for me. I can run, test code with only a couple of keystrokes.

(Add an animated gif demonstrating code formatting and testing on save in vim)

Let's see how performant the procedure I wrote it. I had to create a special benchmark test case:

```go

```

# Write about tooling
# WRite about formatting
# Benchmarking

# About the lack of Boundler
# Tiobe index
