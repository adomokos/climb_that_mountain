### Containers

As I was exploring how to make Golang even faster on AWS Lambda, I found a project that promised sub-millisecond execution time compared to my (already pretty good) ~60 millisecond. It used Python execution that ran the Go code in process in contrast to my attempt, where I had to spawn a new process and execute the lambda code there. Very clever, no wonder that solution did not have the ~60 millisecond penalty for running that code.

```highlight
However, in order to build the sample code for this AWS Lambda I had to use Docker.
```

I've heard about Docker years ago, understood what it's used for at a very high level, however, I have never really given it a serious try. I figured it was time. Boy, I was in for some pleasant surprise!

The project <a href="">eawsy</a> used docker to containerize their build environment on my laptop. What does that mean? Imagine having a build server running on your computer in seconds, where the version of the Go compiler, the Python environment is set by the author of the project. I'd use a little build engine that takes in my code, runs its magic and a zip file comes out that I can run on Lambda. What?!

I remember writing all these different tutorials about <a href="">running MRI Ruby on AWS Lambda</a> or <a href="">interacting with a Postgres DB with Clojure</a> and I had to set up all the prereqs in plain text: "you have to have Postgres running, and Clojure, and MRI Ruby". I provided all the different Makefile scripts to follow the examples. However, with Docker, that's all the things of the past, I'd just provide a Dockerfile that sets up the right environment in the future.

I believe containers are big, and will be even bigger very soon.

```highlight
I see more and more applications where the code describes the software behavior and the container descriptor describes the environment. They will live side by side, clearly stating what virtual components the software needs to execute. Engineers can run the software with those containers locally, and the software can be deployed to the cloud with those images pre-built, with tight control around its execution context.
```

There are many Docker resources out there. I started with reading the <a href="">Docker in Action</a> book and went further with reading the <a href="">Practical Docker</a> book.

I created a Docker templates repository, where I collected ideas for different recepies. Do I need a Ruby worker with Redis and Postgres backend? I'll just run `docker compose uP` with <a href="">this docker_compose.yml file</a>, and I have an environment, where everything from the OS to the version of Redis and Postgres is predefined. If it works on my machine, it will work on yours, too.

There are many things I like about Docker as compared to Vagrant or other virtual machine solutions, the biggest thing for me is the low power Docker containers would need. Vagrant images would reserve 2 of your 4 cores and 8GB memory, when Docker will only take from the host as it needs. If it's 32MB, that's it, if it's 1GB, it will take that much.

Docker is just a better way to run resources for your software.
