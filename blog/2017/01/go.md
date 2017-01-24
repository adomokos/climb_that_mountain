## Monday, January 23, 2017

### Go

The first time I heard about Golang was a few years back, when the great guys at Brad's Deals, our nextdoor office neighbor organized and hosted the local Go meetup. Then IO.js and Node.js war broke out the TJ Holowaychuck shifted from Node.js to Golang announcing the move in a famous open letter to the community.
I did not think much of the language, as its reputation was far from the beauty of a real functional language.

Fast forward a couple of years and I am giving Ruby a serious try on AWS Lambda. It is far from ideal as we need to invoke some of them millions of times in a month and when we need to pay after used milliseconds, the bill gets fairly large quickly. No wonder, as AWS needs at least a 512 MB instance to run Ruby in a reasonable manner, and the startup time can be 3 seconds. 3000 ms just to see `Hello World!` is pretty expensive when you pay for usage in 100 ms increments.

Then one night I wrote a tiny Go program:

```go
package main

import "fmt"

func main() {
  fmt.Println("Hello, World!")
}
```

I cross compiled (since I am working on OSX) with the command `GOOS=linux GOARCH=amd64 go build github.com/adomokos/hello` to Linux, packaged it up with a Node.JS executor and ran it. I couldn't believe my eyes. It took only 34 ms to get the string `Hello, World!` back. 34 ms! And it was on a 128 MB memory instance. It was beautiful. Ruby would need 4 times the memory and it would still execute ~10 times slower than Go. That was the moment when I got hooked.

Go is a simple language. I am not saying it's easy to learn, it's subjective: it depends on your background, your experience. But it's far from the beauty of Haskell or Clojure. However, I know, that the team I am working with would have no trouble switching between Go and Ruby during the day.
What kind of a language today does not have a `map` or `reduce` functions? Especially when functions are first class citizens. It turns out, I can write my own map function if I need to:

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

#### History

Go was created by some very smart people at Google, I wanted to understand their decision to keep a language this pure.
Google has a large amount of code in C and C++, however, those languages are far from modern concepts, like parallel execution and web programming to name a few. Those languages were created in the 60s and 80s, well before the era of multi-core processors and the World Wide Web. Compiling a massive codebase in C++ can easily take hour(s), and while they were waiting for compilation, the idea of a fast compiling, simple, modern language idea was born. Go does not aim to be shiny and nice, no, its designers kept it:

* to be simple and easy to learn
* to compile fast
* to run fast
* making parallel processing easy

Google hires massive number of fresh CS graduates each year with some C++ and Java programming experience. These engineers can feel right at home with Go, where the syntax and concept is similar to those languages.

#### Tooling



# Write about tooling
# WRite about formatting
# Benchmarking

# About the lack of Boundler
# Tiobe index
