## Tuesday, December 11, 2012

### [npm and bundler - the Good Parts](http://www.adomokos.com/2012/12/npm-and-bundler-good-parts.html)

I need two things from a language to take it seriously: good testing framework and some kind of package management. I've dealt with testing in my previous blog posts, now I compare two package management tools that I like, [npm](https://npmjs.org/) and [bundler](http://gembundler.com/).

**:: NPM**

Early this year I went fairly deep into node.js development. I wanted to understand how it works and where I could use it in the future. Getting node on your local development environment is easy, but getting used to coding with async callbacks takes a bit of practice to master.

All I needed on OSX to get started with node was:

```shell
$: brew install node
```

When I wanted to use a package - let's say [mocha.js](http://visionmedia.github.com/mocha/) - I just used npm.

```shell
$: npm install mocha
```

It installed everything into the project root under the node_modules directory, <span style="font-weight: bold;">locally by default</span> like this:

```
$: lltree
   some_nodejs_project
   |--node_modules
   |----mocha
   |------bin
   |------images
   |- ...
```

Sure you can have it installed globally, but you need to use the "-g" flag to do that. I did not want to pollute my node installation, I was glad with its default behavior where it installed everything under node_modules.

You do pay a price for this behavior. When you execute "mocha" in the terminal, the executable is not found.

```shell
$: mocha
  zsh: command not found: mocha
```

You have to locate that file under the node_modules directory. It's in "node_modules/mocha/bin" dir, by the way.
To get around this I just use a Makefile with a couple tasks in it:

```
REPORTER = list

test: test-bdd

test-bdd:
 @./node_modules/mocha/bin/mocha \
  --reporter $(REPORTER) \
  --ui bdd \
  -- spec/*.js

test-doc:
 @./node_modules/mocha/bin/mocha \
  --reporter $(REPORTER) \
  --ui bdd
```

This way I can easily run my tests by executing "make" in the terminal.

**:: BUNDLER**

I switched from [rvm](https://rvm.io/) to [rbenv](https://github.com/sstephenson/rbenv) early this year. Rbenv with [bundler](http://gembundler.com/) makes a very powerful combo but when you install a gem it's going to install it <span style="font-weight: bold;">globally by default</span>.
Not good. I want to keep my gems local to the current project and I don't want to pollute my Ruby install with different versions of gems. What if I use Rails 3.2.9 in one project but I have to use 3.1.1 in another? Sure you could use [rbenv-gemsets](https://github.com/jamis/rbenv-gemset) to get around this, but I already started using node with npm and I wanted to have a similar experience.

The "--path" switch in bundler lets me specify which directory I want to install my gems into. When I start a new project I immediately create a Gemfile. It's very simple, all you need is this:

```ruby
source "http://rubygems.org"
gem "light_service", "->0.0.6"
gem "rspec"
```

Then I install all the gems through bundler with this command:

```shell
$: bundle install --path vendor/bundle --binstubs
```

Bundler puts all my gems under the vendor/bundle directory and creates a bin directory with executables for the gems that produce such a file. When I run rspec for my project this is what I do:

```shell
$: bin/rspec spec
```

You could either use "bin/rspec" or "bundle exec rspec", either works.

As you see, nor npm, neither bundler has the best solution. But they have facets that I like in both.

![npm-bundler-comparison](/resources/2012/12/npm-bundler-comparison.png)

Can we sync the good parts? Could both have local install by default with easy access to the executables?

#### Update

Shortly after I published this post, my good friend [Joe Fiorini](http://www.joefiorini.com/) pinged me on twitter. Here is our conversation:

![npm_bin_conversation](/resources/2012/12/npm_bin_conversation.jpeg)

I did not know that npm creates a ".bin" directory under "node_modules" with symlinks pointing to the individual executable files. This way it's very easy to run these files:

```shell
$: node_modules/.bin/mocha -h
```

Thanks Joe for pointing this out!


POSTED BY ATTILA DOMOKOS AT 9:50 AM

#### 1 comment:

[Paweł Gościcki](https://www.blogger.com/profile/13586527599147584496) said...

_You can easily configure Bundler to automatically install binstubs for every project:_

```shell
vim ~/.bundle/config
---
BUNDLE_PATH: .bundle
BUNDLE_BIN: .bundle/bin
```

_Reference: http://hmarr.com/2012/nov/08/rubies-and-bundles/_

_And the second part of the puzzle is automatically adding those binstubbed paths to the $PATH. I do it like this (in zsh):_

```shell
# initialy add the binstubs directory to PATH, but remove it on first `cd` to a directory without `Gemfile`
PATH=".bundle/bin:$PATH"

autoload -U add-zsh-hook
add-zsh-hook chpwd chpwd_add_binstubs_to_path

function chpwd_add_binstubs_to_path {
if [ -r $PWD/Gemfile ] && [ -d $PWD/.bundle/bin ]; then
  export PATH=.bundle/bin:${PATH//\.bundle\/bin:}
else
  export PATH=${PATH//\.bundle\/bin:}
fi
}
```

December 11, 2012 at 3:30 PM
