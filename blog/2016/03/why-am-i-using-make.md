## Saturday, March 28, 2016

### [Why Makefile In Any Project?](http://www.adomokos.com/)

A new member joined our team a couple of weeks ago, and a we took him out for a cup of coffee after lunch on his first week. As we walked he asked me a question: "I've never seen this before and I wanted to ask you. Why are you using Make in your Ruby project?"

His question was ?valid? coming from someone in the Ruby world, where we have [rake](https://www.github.com/rake/rake) for achieving the same goal. I had to think about the history there as I explained my reasoning behind it.

As I started looking at other programming languages, rake wasn't available for me. A couple of years ago I got pretty deep with node.js, and there were repetitive tasks I had to do, like dropping and rebuilding a database, running the tests, etc. I created a `script` directory, and put separate scripts in it to do all that.

For example, to accomplish the two tasks I mentioned above, I created two scripts:
```
project_root
   |- scripts
        |- build_db.sh
        |- run_tests.sh
```

This worked OK, but when I contributed to the great testing framework, [mocha.js]() I realized that the author of that module, T.J., just took these convenience scripts to another level by using a [Makefile](https://www.github.com/mochajs/mochajs/Makefile).

I noticed he is using Make to run simple shell commands in a more elegant manner than I did with my scripts directory and shell scripts in it. I immediately started to adopt this practice.

As I started exploring other languages like Clojure, Erlang, Haskell, using Make was an obvious choice. It did not matter what language I used, dropping and building a database was the same task, regardless.

This practice came with me as I went back to Ruby as well. As I started working on larger, 3-4 year old Rails apps, running rake task was a time consuming exercise. Bundler had to load the whole world into memory before it could even evaluate what it had to do. This way simple tasks that had nothing to do with Rails had a 8-10 seconds startup time. I did not think twice about firing up a Makefile to do same thing in less than a second.

The Rails begots dismissed this, but I did not care, productivity over religion has a higher precedence for me.

I was amazed a few weeks ago, when I found a [blog post](http://blog-post-on-task-description) on how to add task descrpition to Make targets. I updated my Makefiles and now when I just run `make` in the terminal, this is what I see:

![make-output](get the file)

As I have not found a good make target generator, I created this [gist](https://www.github.com/adomokos/gists) to get me rolling. Documentation and a sample target template is what I need to get me rolling.
