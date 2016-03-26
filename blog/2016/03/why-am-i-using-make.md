## Saturday, March 28, 2016

### [Why Make?](http://www.adomokos.com/)

A new member joined our team a couple of weeks ago, and as we took him out for a cup of coffee on his first week, he asked me a question: "I've never seen this before and I wanted to ask you. Why are you using Make in your Ruby project?"

His question was legit coming from someone in the Ruby world, where we have [rake](https://github.com/ruby/rake) for achieving the same goal. I had to think about the history there as I explained my reasoning behind using it.

As I started looking at other programming languages, `rake` wasn't available for me. A couple of years ago I got pretty deep into node.js, and there were repetitive tasks I had to do, like dropping and rebuilding a database, running the tests, etc. I created a `script` directory, and put separate scripts in it to accomplish all that.

For example, the two tasks I mentioned above, I created two scripts:
```
project_root
   |- scripts
        |- build_db.sh
        |- run_tests.sh
```

This worked OK for a while, but when I contributed to the great testing framework [mocha.js](https://github.com/mochajs/mocha), I realized that the author of that module, [T.J.](http://tjholowaychuk.com/), just took these convenience scripts to another level by using [Make](https://github.com/mochajs/mocha/blob/master/Makefile).

I noticed he is using Make to run simple shell commands in a more elegant manner than I did with my scripts directory and shell scripts in it. I immediately started to adopt this practice.

As I started exploring other languages like Clojure, Erlang, Haskell, using Make was an obvious choice. It did not matter what language I used, dropping and building a database was the same task, regardless.

This practice came with me as I went back to Ruby as well. As I started working on larger, 3-4 year old Rails apps, running rake tasks was a time consuming exercise. Bundler had to load the whole world into memory before it could even evaluate what it had to do. This way, simple tasks that had nothing to do with Rails had a 8-10 seconds startup time. I did not think twice about firing up a Makefile to do same thing in less than a second.

Of course, some of the religious Rails deciples dismissed this, but I did not care, productivity over religion has a higher precedence for me.

I was amazed a few weeks ago, when I found a [blog post](http://marmelab.com/blog/2016/02/29/auto-documented-makefile.html) on how to add task descrpition to Make targets. I updated my Makefiles, and now, when I run `make` in the terminal, this is what I see:

![make-output](/resources/2016/03/make_output.jpg)

As I have not found a good make target generator, I created a [gist](https://gist.github.com/adomokos/2fd95840d59b19bbb3f4) to get me rolling. Documentation and a sample target is a good way to get started. I even added a shell function to grab it for me:

```shell
make-init() {
  curl https://gist.githubusercontent.com/adomokos/2fd95840d59b19bbb3f4/raw/7b548cd3fda0dab958ecb0e0955fbadc1af6ef6e/Makefile > Makefile
}
```
Now, I only need to type `make-init` in the terminal and I have a Makefile to work with.
