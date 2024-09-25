DISCLAIMER: This is the course material and our group's implementation of a Scheme compiler for Professor Norman Ramsey's CS106: Virtual Machines and Language Translation. Credit for all course material and boilerplate code go to Professor Ramsey.

This is the student-facing code repository for CS 106: 
Simple Virtual Machines and Language Translation.

The repo is intended to be cloned and edited.  If your `git` skills
are limited, no problem!  You mainly need to know `git clone` to get
your own copy, `git commit` to save your own work, and `git pull` 
to get updates.  You can also use these resources:

  - If you're quite new to `git`, I recommend [The Git
    Parable](https://tom.preston-werner.com/2009/05/19/the-git-parable.html)
    by Tom Preston-Werner.  This introduction focuses on problems that
    `git` is meant to solve.

  - [Git Magic](http://www-cs-students.stanford.edu/~blynn/gitmagic/)
    by Ben Lynn is a fine tutorial.

  - If you use Emacs, [Magit](https://magit.vc/) by Jonas Bernoulli is
    the best interface---it blows everything else out of the water.

----------------------------------------------------------------------------------

In the repository, you'll find these directories:

  - `bin` holds compiled binaries and also scripts.  Add it to your `$PATH`.

  - `build` is a holding area for compiled files (`.o` and that ilk).
    If you are having trouble compiling, it is safe to remove
    everything from it and start over.

  - `src` contains sources that I provide and that you'll edit:

    - `src/vscheme` contains source code for a `vscheme` interpreter.
      Use it for testing and experimentation.
    - `src/svm` contains starter source code for your Simple Virtual Machine.
    - `src/uft` contains starter source code for your Universal Forward Translator.
