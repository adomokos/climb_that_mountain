## Friday, February 19, 2016

### [Teaching Clojure to a 7-Year-Old](http://www.adomokos.com/2016/02/teaching-clojure-to-7-year-old.html)

I see my 7-year-old son as someone, who is deeply interested in anything computer related. I recall how he went through photos on my iPod Touch when he was only two years old. Then video games kicked in, and now he can make an 8-hour flight back to Europe without taking a break from a game on an iPad.

This is all nice and cool, but why don't we do something more useful with this passion?! He has been curious about programming, he practiced basic function calls via the game [Code Combat](https://codecombat.com/). I figured, let's take this to the next level and try programming.

I wanted to use a language that is easy to understand, but can be very powerful. I considered Python or JavaScript, but I figured his first real programming language should be a functional one, and the simple nature of LiSP made Clojure the obvious choice.

![two_macs](/resources/2016/02/two_macs.jpg)

I wanted to find an editor that's easy to use. I was aware of [LightTable](http://lighttable.com/), but I've never tried it. We downloaded it on my wife's 11" MacBook Air and we jumped in.

What's really cool about LightTable is that you don't need to run a separate REPL, you could just write your Clojure expressions in a clj file, save it and by hitting <Cmd> + <Shift> + <Enter> the expressions are evaluated in line, right next to them. It's really the best tool for beginners.

We worked on a rectangular area calculator in our first session, since that's what he's been learning at school. This was our first expression:

```clojure
(defn area [x y] (* x y))

(area 2 3) # 6
```

We tried different numbers, he was pumped when the correct number was printed in LightTable after the evaluation.

Our area function is for rectangles, but what if we wanted to calculate the square's area? We only had to pass one number to our calculator.
Of course we could have done this:

```clojure
(area 4 4) # 16
```

But this was not very elegant. I proposed creating a new function for `square-area` and calling `area` from that this way:

```clojure
(defn square-area [x]
  (area x x))

(square-area [4]) # 16
```

My goal here was to show him how one function can leverage the functionality of another function. He liked this a lot.

In our next session - as he constantly nudged me to continue his journey in Clojure programming land - I wanted to teach him something we could build upon: we learned about vectors, which is similar to Arrays in other programming languages.

We started out with listing his best friends:

```clojure
(def friends [:andy :james :tommy :ethan :elliot])
```

I explained to him that these names are in order and they will always remain in order the way he defined it first. There are ways to explore the collection, for example pulling the first item from it. This is what we tried:

```clojure
(first friends) # :andy
```

I asked him if he could get the last item of the vector. He thought about a bit and this is what he came up with:

```clojure
(last friends) # :elliot
```

Excellent! Then I asked him how we could get the third item from the collection and he intuitively tried the function `third`, which does not exist. I showed him the `nth` function to do that. This is what he tried to get the third item:

```
(nth friends 3) # :ethan
```

But oh, it returned the fourth and not the third item. So we talked about the 0-based index, which he grasped, but did not make much sense to him.

I told him: "Imagine how great it would be to sort these names in alphabetical order. Do you know what verb would describe that operation?" He said "sort", so we gave it a try:

```clojure
(sort friends) # (:andy :elliot :ethan :james :tommy)
```

We both smiled at how easy it was. Then I asked him if we could put the names in descending order. We were looking for the right word, and he came up with `backwards`. Well, it's close, so I asked him to look up synonyms for that word in Google. We both settled on `reverse`. He tried this:

```clojure
(reverse friends) # (:elliot :ethan :tommy :james :andy)
```

Oh-oh. This only put the original list into reverse order without sorting. It was obvious that we needed to sort it first and then reverse it. I helped him to write this:

```clojure
(reverse (sort friends)) # (:tommny :james :ethan :elliot :andy)
```

We wrapped up our session by creating a vector with numbers.

```clojure
(def numbers [9 12 5 7 1])
```

Based on what he learned earlier, he put them numbers in descending order:

```clojure
(reverse (sort numbers)) # (12 9 7 5 1)
```

I showed him how we could use the `filter` and the `odd?` functions to filter out the odd numbers.

```clojure
(filter odd? numbers) # (9 5 7 1)
```

He was able to sort the odd numbers by using the `sort` function:

```clojure
(sort (filter odd? numbers)) # (1 5 7 9)
```

Picking the largest odd number was a tricky one, we had to look at the docs, as this did not work:

```clojure
(max (filter odd? numbers)) # (1 5 7 9)
```

But this did:

```clojure
(apply max (filter odd? numbers)) # 9
```

There was an easier way to get the largest odd number: reverse sorting the odd numbers and getting the first item. This is what we wrote:

```clojure
(first (reverse (sort (filter odd? numbers)))) # 9
```

We both smiled when we saw the result.

This is the full code we wrote together:

```clojure
(def friends [:andy :james :tommy :ethan :elliot])

(first friends)
(last friends)
(count friends)
(sort friends)
(reverse (sort friends))
(nth friends 2)
(= 1 1)
(= :andy (first friends))

(def numbers [9 12 5 7 1])
(= 9 (first numbers))
(sort numbers)
(reverse (sort numbers))
(filter odd? numbers)
(sort (filter odd? numbers))
(apply max (filter odd? numbers))
```

We are going to spend half of our time going through the same function composition the next time. Then we might look at other collection types like maps and sets. I don't want to push him, but if he asks for it, we'll go as far as we can.


POSTED BY ATTILA DOMOKOS AT 1:10 PM


#### 3 comments:

[Corey](https://www.blogger.com/profile/14036447741809943599) said...

_Interesting post, dude. I've been considering my son's first programming language (as I, too, have a 7 year old now), but I really, really hate LiSP. Any other recommendations for a language?_

February 22, 2016 at 11:22 AM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_Sorry to hear about your dislike of LiSP. :-( I wanted to find a language that is easy to understand, it's declarative, not object-oriented, dynamically typed. Python fits the bill pretty well, that was the runner-up language I considered. I'd shy away from Java, C#, or other statically typed language, as the strong-typed OO system just adds more complexity to a kid (or anybody else)._

February 22, 2016 at 8:01 PM

[Corey](https://www.blogger.com/profile/14036447741809943599) said...

_Yeah, there's no way I'd ever teach my kids Java or C# as their first language. Way too complex for them. Python, though, is an interesting idea, and it certainly has its simplicity. I might end up doing that then..._

February 23, 2016 at 9:29 AM
