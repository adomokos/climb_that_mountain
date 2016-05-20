### Using Ruby in AWS Lambda

It was May 2015 at the AWS Summit in Chicago, where I first heard about AWS Lambdas. The company I worked with was running its servers on Linode, I had no chance of using it, but I still found the concept fascinating: "just deploy code and we will execute it for you" was the promsie.

Bulk of my work at my current gig is about transforming data: we get some kind of data, we need to transform and load it into our own data storage. Sure the worker boxes can do the job, but maintaining a series of these instances takes effort. AWS Lambdas would be the perfect solution for us, but Amazon does not support Ruby natively, which is most of our business logic is written in.

AWS Lambda, as of this writing, supports three main platforms: Java, Node.JS and Python. I played around running Clojure on AWS Lambda, which worked as the code is compiled into a jar file, but our current code - due to its monolithic nature - can't support any other language for a service just yet.

Amazon claimed you can run any language on AWS Lambda, Ruby included, but I have not found a comprehensive guide that would show me how. Once you can package up your app to run as an executable, you can run it. I found this [blog post](https://medium.com/@gigq/using-swift-in-aws-lambda-6e2a67a27e03#.gtg1u3lve) that describes how a Swift code base can be bundled, deployed and invoked on AWS Lambda. It was clear to me that this solution would work, I only had to package Ruby with its own interpreter to accomplish the same. I looked for tools that can do this, and found the great [Travelling Ruby](http://phusion.github.io/traveling-ruby/) app. You can package your code and run it as an executable on the user's computer, no local Ruby installation is necessary. I first wanted to try it locally, thinking if it works there (on OSX), it should work on AWS Lambda as well.

First things first: you need to have the same version of Ruby as the one Travelling Ruby offers. The latest there is Ruby 2.2.2, I'd recommend installing that through Rbenv or RVM, and use that throughout the project.

This post assumes you are familiar with AWS, you have access to the AWS web console and you have the AWS command line tool configured to interact with your services through the terminal.

1. Setting up the project

I named the project `aws-lambda-ruby` and created a directory structure like this:

```shell
- aws-lambda-ruby
    |- hello_ruby
       |- lib
          |- hello.rb
```

I put this code in the `hello.rb` file:
```ruby
puts 'Hello from Ruby!'
```

I made sure that my Ruby version in the project is 2.2.2 by setting it with Rbenv. I ran the app and made sure everything is wired up OK thus far:
```shell
$: cd hello_ruby && ruby lib/hello.rb
$: Hello from Ruby!
```
[Commit point](https://github.com/adomokos/aws-lambda-ruby/commit/c1d1970023ccf376c718aa1516b356df0a6c0d16)

2. Execute the Ruby code with Travelling Ruby

Create a directory under the project root directory with the name `resources`. Your directory structure should look like this:

```shell
- aws-lambda-ruby
    |- hello_ruby
    |- resources
```
Download the Ruby runtimes from [Travelling Ruby](http://phusion.github.io/traveling-ruby/)'s [S3 bucket](https://traveling-ruby.s3-us-west-2.amazonaws.com/list.html) into the `resources` directory. I only needed the [OSX version](http://d6r77u77i8pq3.cloudfront.net/releases/traveling-ruby-20150715-2.2.2-osx.tar.gz) for local development, and the [linux-x86_64](http://d6r77u77i8pq3.cloudfront.net/releases/traveling-ruby-20150715-2.2.2-linux-x86_64.tar.gz) version for AWS. My directory had these two files in it:

```shell
- aws-lambda-ruby
    |- hello_ruby
    |- resources
         |- traveling-ruby-20150715-2.2.2-linux-x86_64.tar.gz
         |- traveling-ruby-20150715-2.2.2-osx.tar.gz
```
[Commit point](https://github.com/adomokos/aws-lambda-ruby/commit/e4c12cb357f276f51ef771686a9fb4cad6df2162)

Create two new directories for assembling the project under OSX and Linux X86_64 like these:

```shell
- aws-lambda-ruby
    |- hello-2.0.0-linux-x86_64
    |- hello-1.0.0-osx
    |- hello_ruby
    |- resources
```
Add a [Makefile](http://www.adomokos.com/2016/03/why-make.html) to the project under the root directory, we want to automate all the different steps as early as possible. Create a Make target to package the code for OSX.


