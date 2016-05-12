### Run Ruby on AWS Lambda

I attended the AWS Summit in Chicago in 2015, where I first heard about AWS Lambdas. The company I worked with was on Linode, I had no chance of using it, but I still found the concept fascinating: don't run servers, just deploy code and we will execute it for you.

Bulk of my work at my current gig is really about transforming data: we get some kind of XML, we need to transform and load it into our own data storage. Sure the worker boxes can do the job, but maintaining a series of these instances takes effort. AWS Lambdas would be the perfect solution for us, but Amazon does not support Ruby natively, which is most of our business logic is written in.

AWS Lambda, as of this writing, supports 3 main platforms: Java, Node.JS and Python. I played around running Clojure on AWS Lambda, which worked as the code is compiled into a jar file, but the current code base can't really support any language for a service set up just yet.

Amazon claimed you can run any language on AWS Lambda, Ruby included, but I have not found a comprehensive guide that would show me how. Once you can package up your app to run as an executable, you can run it. I bumped into this blog post that was a pretty good tutorial for showing people how a Swift code base can be bundled, deployed and invoked on AWS Lambda. It was clear to me that I had to package Ruby with its own interpreter to accomplish the same thing. I looked for tools that can do this, and I found the great [Travelling Ruby](http://travelling.ruby) app. You can package your code and run it as an executable on the user's computer, no local Ruby installation is necessary. I first wanted to try it locally, as if it works there (on OSX), it should work on AWS Lambda as well.

I created a project skeleton like this:
