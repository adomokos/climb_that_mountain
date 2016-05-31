### Using Ruby with ActiveRecord in AWS Lambda

I showed in the previous blog post how you can run MRI Ruby on AWS Lambda. In this writing I'll guide you through adding gems to the project: first [faker](https://github.com/stympy/faker), and then the [mysql2](https://github.com/brianmario/mysql2) gem with [active_record](https://github.com/rails/rails/tree/master/activerecord), and finally we will have the Ruby code talk to an RDS instance.

I recorded all my changes in [this project](https://github.com/adomokos/aws-lambda-ruby), feel free to jump in where you want, I recorded commit points after each section.

(1) Add faker to the project

You can pick up the changes from the previous blog post [here](https://github.com/adomokos/aws-lambda-ruby/commit/7bf6c4e5d6f745d636dbdc6737db7f23a4371085). Let's jump in by editing our Ruby app, let's add a Gemfile to it in the `hello_ruby` directory:

```ruby
source 'https://rubygems.org'

gem 'faker'
```

Run `BUNDLE_IGNORE_CONFIG=1 bundle install --path vendor` in that directory. The `--path vendor` argument is important, as we have to package all the files in the `vendor` directory. Make sure the `BUNDLED WITH` part of your Gemfile.lock is not there as that can cause you some pain when you deploy your code to AWS Lambda.

Edit the `lib/hello.rb` file like this:

```ruby
#!/usr/bin/env ruby

require 'faker'

puts "Hello - '#{Faker::Name.name}' from Ruby!"
```

We required the faker gem and used it to generate a fake name. Run the app in the terminal with `bundle exec ruby lib/hello.rb` command.

```shell
Hello - 'Jamar Gutmann II' from Ruby!
```

You will get a different name between the single quotes, but that's the point, faker generates random name for us.

[Commit point](https://github.com/adomokos/aws-lambda-ruby/commit/a0ca3becd83e3a2c6c87f627114f20dfdcbffd5f)

(2) Use faker with Traveling Ruby

The `run` target in the Makefile will have to copy all vendorized gems, plus it needs to configure the app to run with the correct bundler settings. This step is heavily influenced by how Traveling Ruby packages gems for deployment, please review [their tutorial](https://github.com/phusion/traveling-ruby/blob/master/TUTORIAL-2.md) as reference.

Add a `bundler_config` template file to the `resources` directory with this content:

```shell
BUNDLE_PATH: .
BUNDLE_WITHOUT: development:test
BUNDLE_DISABLE_SHARED_GEMS: '1'
```

Changed the `resources/wrapper.sh` file to set the Gemfile's location:

```shell
#!/bin/bash
set -e

# Figure out where this script is located.
SELFDIR="`dirname \"$0\"`"
SELFDIR="`cd \"$SELFDIR\" && pwd`"

# Tell Bundler where the Gemfile and gems are.
export BUNDLE_GEMFILE="$SELFDIR/lib/vendor/Gemfile"
unset BUNDLE_IGNORE_CONFIG

# Run the actual app using the bundled Ruby interpreter, with Bundler activated.
exec "$SELFDIR/lib/ruby/bin/ruby" -rbundler/setup "$SELFDIR/lib/app/hello.rb"
```

Modify the Makefile's `run` target with the following changes:

```shell
...

run: ## Runs the code locally
	@echo 'Run the app locally'
	@echo '-------------------'
	@rm -fr $(OSXDIR)
	@mkdir -p $(OSXDIR)/lib/ruby
	@tar -xzf resources/traveling-ruby-20150715-2.2.2-osx.tar.gz -C $(OSXDIR)/lib/ruby
	@mkdir $(OSXDIR)/lib/app
	@cp hello_ruby/lib/hello.rb $(OSXDIR)/lib/app/hello.rb
	@cp -pR hello_ruby/vendor $(OSXDIR)/lib/
	@rm -f $(OSXDIR)/lib/vendor/*/*/cache/*
	@mkdir -p $(OSXDIR)/lib/vendor/.bundle
	@cp resources/bundler-config $(OSXDIR)/lib/vendor/.bundle/config
	@cp hello_ruby/Gemfile $(OSXDIR)/lib/vendor/
	@cp hello_ruby/Gemfile.lock $(OSXDIR)/lib/vendor/
	@cp resources/wrapper.sh $(OSXDIR)/hello
	@chmod +x $(OSXDIR)/hello
	@cd $(OSXDIR) && ./hello

...
```

Run the target with `make run` and you should see something similar to this in the terminal:

```shell
$: make run
Run the app locally
-------------------
Hello - 'Kelly Huel' from Ruby!
```

[Commit point](https://github.com/adomokos/aws-lambda-ruby/commit/3a3cfe89c5a65527be141256c5ab85700d1114ae)

(3) Deploy the app with faker to AWS Lambda

In order to run your app on AWS Lambda, you only need to change the `package` target in your Makefile, everything else, the delete, create, invoke targets should remain the same. Change the target like this:

```shell
...

package: ## Package the code for AWS Lambda
	@echo 'Package the app for deploy'
	@echo '--------------------------'
	@rm -fr $(LAMBDADIR)
	@rm -fr deploy
	@mkdir -p $(LAMBDADIR)/lib/ruby
	@tar -xzf resources/traveling-ruby-20150715-2.2.2-linux-x86_64.tar.gz -C $(LAMBDADIR)/lib/ruby
	@mkdir $(LAMBDADIR)/lib/app
	@cp hello_ruby/lib/hello.rb $(LAMBDADIR)/lib/app/hello.rb
	@cp -pR hello_ruby/vendor $(LAMBDADIR)/lib/
	@rm -f $(LAMBDADIR)/lib/vendor/*/*/cache/*
	@mkdir -p $(LAMBDADIR)/lib/vendor/.bundle
	@cp resources/bundler-config $(LAMBDADIR)/lib/vendor/.bundle/config
	@cp hello_ruby/Gemfile $(LAMBDADIR)/lib/vendor/
	@cp hello_ruby/Gemfile.lock $(LAMBDADIR)/lib/vendor/
	@cp resources/wrapper.sh $(LAMBDADIR)/hello
	@chmod +x $(LAMBDADIR)/hello
	@cp resources/index.js $(LAMBDADIR)/
	@cd $(LAMBDADIR) && zip -r hello_ruby.zip hello index.js lib/
	@mkdir deploy
	cd $(LAMBDADIR) && mv hello_ruby.zip ../deploy/
	@echo '... Done.'

...
```
The added rows are very similar to the ones we had to add to run the app locally with Travelling Ruby. Delete the lambda functions and recreate it by using the Makefile. When you invoke it, your should see something like this:

```shell
START RequestId: 3f6ae8f5-23c1-11e6-9acc-0f50ffa39e9b Version: $LATEST
2016-05-27T04:12:41.473Z
  3f6ae8f5-23c1-11e6-9acc-0f50ffa39e9b
    Hello - 'Mrs. Lelah Bradtke' from Ruby!

END RequestId: 3f6ae8f5-23c1-11e6-9acc-0f50ffa39e9b
REPORT RequestId: 3f6ae8f5-23c1-11e6-9acc-0f50ffa39e9b
       Duration: 3425.01 ms
       Billed Duration: 3500 ms
       Memory Size: 512 MB
       Max Memory Used: 65 MB
```
The `Hello - 'xyz' from Ruby!` string contains the Faker gem generated name. You can also invoke the Lambda function through the AWS Management Console, you should see something similar to this in the `Log output` section:

![faker-with-aws-lambda](/resources/2016/06/faker_with_aws_lambda.jpg)

[Commit point](https://github.com/adomokos/aws-lambda-ruby/commit/8fa581ded4007f3ae2b5a1ace2da79991f07b75e)

(4) Publish a newer version to AWS Lambda

Dropping and recreating the function works, but not the most effective solution. AWS allows you to update your function which we'll do with this new target in the Makefile:

```shell
...

publish: package ## Deploys the latest version to AWS
        aws lambda update-function-code \
                --function-name HelloFromRuby \
                --zip-file fileb://./deploy/hello_ruby.zip

...
```
This target will let you update the function code. It also calls the `package` target to make sure your latest changes will be deployed to AWS.

[Commit point](https://github.com/adomokos/aws-lambda-ruby/commit/1edeaca0ad85806a20947ff1df688e660f7f447e)

(5) Create a new RDS database with one table

Add this script to your Makefile, it will create a minimal RDS instance for you, you can drop that instance, connect to the DB and drop and create the database with some seed data in it.

```shell
DBPASSWD=Kew2401Sd
DBNAME=awslambdaruby

create-rds-instance: ## Creates an RDS MySQL DB instance
	aws rds create-db-instance \
		--db-instance-identifier MyInstance01 \
		--db-instance-class db.t1.micro \
		--engine mysql \
		--allocated-storage 10 \
		--master-username master \
		--master-user-password $(DBPASSWD)

delete-rds-instance: ## Deletes an RDS MySQL DB instance
	aws rds delete-db-instance \
		--db-instance-identifier MyInstance01 \
		--skip-final-snapshot

db-connect: ## Connects to the RDS instance
	mysql --user=master --password=$(DBPASSWD) --host myinstance01.cgic5q3lz0bb.us-east-1.rds.amazonaws.com

create-db: ## Creates a DB with a table and records
	@echo "Dropping  and creating database"
	@echo "-------------------------------"
	@mysql -u master --password='$(DBPASSWD)' --host myinstance01.cgic5q3lz0bb.us-east-1.rds.amazonaws.com -e "DROP DATABASE IF EXISTS $(DBNAME)" > /dev/null 2>&1
	@mysql -u master --password='$(DBPASSWD)' --host myinstance01.cgic5q3lz0bb.us-east-1.rds.amazonaws.com -e "CREATE DATABASE $(DBNAME)" > /dev/null 2>&1
	@mysql -u master --password='$(DBPASSWD)' --host myinstance01.cgic5q3lz0bb.us-east-1.rds.amazonaws.com $(DBNAME) < resources/schema.sql > /dev/null 2>&1
	@mysql -u master --password='$(DBPASSWD)' --host myinstance01.cgic5q3lz0bb.us-east-1.rds.amazonaws.com $(DBNAME) < resources/seed.sql > /dev/null 2>&1
	@echo "... Done"

...
```
Create the RDS instance first, then allow connecting to it from your own IP through adjusting the "Inbound" traffic through your own IP under your Security Group:

![adjust-security-group](/resources/2016/06/adjust_security_group.jpg)

There are plenty of documents out there to show you how you can do it, please search for it if you get stuck.

[Commit point](https://github.com/adomokos/aws-lambda-ruby/commit/435c76738689b24034446578d2707d5304544a89)