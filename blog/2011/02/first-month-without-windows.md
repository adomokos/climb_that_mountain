## Wednesday, February 16, 2011

### [First Month Without Windows](http://www.adomokos.com/2011/02/first-month-without-windows.html)

It's been a little over than a month since I left the Microsoft Universe behind. I had used OS X and Linux before - mostly in the evenings and weekends - but I made the jump finally and I only touch Windows when I browse the web on my wife's laptop. And I just couldn't be happier...

**Life on the Mac**

I have a pretty powerful Mac. It has an Intel Core i5 CPU and I upgraded the RAM from 4GB to 8GB as soon as I received it. I don't have an SSD just yet, but I am still pretty happy with its performance. Whenever I feel like checking the current state of the machine I just run "[htop](http://htop.sourceforge.net/)" in the terminal.  

I started out using Firefox for my work email, but I write automated tests in it as well and I had to find another solution. "[Before I ran out of browsers](http://www.docondev.com/2011/01/squirrel.html)" I investigated my options. I could have purchased [Mailplane](http://mailplaneapp.com/) but I did not feel I needed a full blown app for it. I found [Fluid](http://fluidapp.com/), a site specific browser. Now I can open my work email just like any other application and I am still using a browser inside. I found a nice PNG file that I set up with it and the app looks just like a native app when I tab between applications. Here is how it appears under the Applications folder:  

![W3Mail](/resources/2011/02/W3Mail.jpg)

To organize my thoughts and notes I started using [Evernote](http://www.evernote.com/). It's a great tool for note taking but I think it does an even better job at <span style="font-weight: bold;">organizing</span> them. I even installed the [Evernote Chrome extension](https://chrome.google.com/extensions/detail/pioclpoplcdbaefihamjohnefbikjilc):

![evernote_chrome_extension](/resources/2011/02/evernote_chrome_extension.jpg)

I don't use the mouse - or track pad - to launch an application. First I used Spotlight but switched to [Alfred App](http://www.alfredapp.com/) recently. It's fast and I can use keyboard shortcuts. This image tells it all:

![alfred](/resources/2011/02/alfred.jpg)

**Living my life in the Terminal (iTerm)

After using the terminal for a couple of weeks I switched to [iTerm2](http://sites.google.com/site/iterm2home/). I don't use all its neat features just yet but I do like the split view mode. I have cucumber features running on one side and rails logs on the other.  

I am still learning (who doesn't) and tweaking all the different configuration options of my ~/.vimrc and ~/.zshrc files. I am particularly happy with this addition to my ~/.vimrc file that ignores my arrow keys when I am in command mode. No more arrows to move around!  

```shell
" Ignore arrow keys in vim
:map <Left> <Nop>
:map <Right> <Nop>
:map <Up> <Nop>
:map <Down> <Nop>
:map <PageUp> <Nop>
:map <PageDown> <Nop>
:map <Home> <Nop>
:map <End> <Nop>

:map! <Left> <Nop>
:map! <Right> <Nop>
:map! <Up> <Nop>
:map! <Down> <Nop>
:map! <PageUp> <Nop>
:map! <PageDown> <Nop>
:map! <Home> <Nop>
:map! <End> <Nop>
```

I even started listening to [Pandora](http://www.pandora.com/) in the terminal through [Pianobar](https://github.com/PromyLOPh/pianobar).

![pianobar](/resources/2011/02/pianobar.jpg)

I have used Git before, but my skills are not where it should be. I started reading the book [Pragmatic Version Control Using Git](http://pragprog.com/titles/tsgit/pragmatic-version-control-using-git) which I'd recommend to anybody who wants to go deep with Git. You should also check out the great [Git Immersion class](http://gitimmersion.com/) created by [EdgeCase](http://edgecase.com/).  

**My Typing Sucks**

Well, maybe it's not that bad, but my typing could and should improve. It's just not fast enough and I don't use all my fingers. I need to take my eyes off the monitor and look at the keyboard when I try to use special key commands that I have not used much before. And my typing is not accurate, I frequently have to go back and fix words that I mistyped.
Unacceptable. I wonder if companies should check in an interview how well a candidate can type.

I used the web site [typingweb.com](http://www.typingweb.com/) and an app called [aTypeTrainer4Mac](http://web.me.com/typetrainer4mac/aTypeTrainer4Mac/home.html) to practice. I am still not where I'd like to be, but I am working on it.

Thanks to [Joe Fiorini](http://joefiorini.com/) for showing me endless tips mentioned in this blog post.


POSTED BY ATTILA DOMOKOS AT 4:03 PM

#### 2 comments:

[nateklaiber](http://www.nateklaiber.com/) said...

_At first I was thinking 'He sits right next to a window, what is he talking about?' - then I read your post. Some great things in here, and I am mostly excited to learn with you as we get better at all of these things (as I type this not using all of my fingers)._

_We are lucky to have Joe on our team to help guide us. I know he's an inspiration to me as I work with VIM/Git - and I learn new things from him all the time._

_Here's to some fun times ahead!_


February 17, 2011 at 7:19 AM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_Nate, you're really a great guy to work with! The days when we pair up on features are not far._

_Thank you for your comment!_

February 17, 2011 at 8:03 PM
