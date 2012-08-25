Projmake Mode For Emacs
=======================

What is It?
-----------

### TLDR

This mode builds your configured projects every time you save and
highlights any errors and warnings generated in your open buffers.

### Long version

This mode uses your projects build system to highlight errors in your
open source buffers each time you save. If you are familiar with
Flymake Mode, then this is a project oriented version of flymake mode
that updates when you save. If you are not familiar with Flymake Mode
then read on.

Your project already has a well designed build system where one
command builds the entire thing, correctly managing all the includes,
dependencies, etc. If it does not have it already then fix that
problem right now. This mode takes that build system and leverages it
to do syntax and error checking. Basically, it builds your project
every time you save and highlights errors and warnings in your open
buffers.


Functionality and Features
--------------------------

* Each time a file is saved a build is started silently in the
  background.

* When the build is finished error lines are highlighted in your open
  buffers with the details of the error messages.

* When starting a build of a particular project, either a possible
  previous live build of the same project is interrupted first or a
  new build is queued to start after the current one finishes. That is
  behaviour is configurable by you, the user.

* A project configuration file specifies the commands required to
  build a project.

* Multiple projects can be loaded into projmake mode and can be built
  as changes in the project occur.


Download
--------

You can get this mode in its current form from
[github](http://github.com/ericbmerritt/projmake-mode) or, even
easier, from [Marmalade](http://marmalade-repo.org).

Setup
------

The easiest way to set things up is to get this package from
Marmalade. However, if you are setting it up manually just add the
elisp directory to your build path.

    (add-to-list 'load-path (file-truename "path-to-the-el-files"))

to your ~/.emacs. You’ll probably also want to start the mode
automatically by adding

    (require 'projmake-mode)
    (projmake-mode)

to your Emacs init file for each file type you want projmake to be
enable for. Once the mode is activated, you should see the Projmake
indicator on the mode line.

Using the Mode
--------------

Typically projects are built (or compiled) using a tool like make, but
the details vary. The projmake mode needs a project configuration file
to know how to build your project. A project configuration file
basically contains an Emacs Lisp expression calling a function named
projmake that returns a project object. A simple example of a project
configuration file would be the (Build.bgb) file used with smlbot:

    (projmake
     :name  "SML-Bot"
     :shell "nice -n5 make all")

The projmake function takes a number of keyword arguments:

name
: specifies the name of the project. This can be any expression that
  evaluates to a string or to a nullary function that returns a
  string.

shell
: specifies a shell command to execute. This can be any expression
  that evaluates to a string, a list of strings, or to a nullary
  function returning a list of strings.

build?
: specifies a predicate to determine whether the project should be
  built after some files have been modified. The predicate is given a
  list of file names and should return a non-nil value when the project
  should be built and nil otherwise.

All of the keyword arguments, except :shell, are optional and can be left out.

Note the use of the nice command above. It means that background build
process is given a lower priority by the system process
scheduler. Assuming your machine has enough memory, using nice ensures
that your computer remains responsive. (You probably won’t even notice
when a build is started.)

### Manually Adding The Build.bgb

Once you have written a project file for projmake mode. Use the
projmake-add-project command to load the project file for projmake
mode.

### Automatically Adding the projmake file

Its also possible to load the mode automatically. By calling
projmake-find-add-project. This searches the current file system
hierarchy starting at the directory containing the file backing the
current buffer. It then searches up through the parent directories
looking for `projmake`. You may call this interactively or add it as
a hook for the mode you are interested in.


    (require 'projmake-mode)

    (defunct my-mode-hook ()
      (projmake-mode)
      (projmake-find-add-project)))

    (add-hook 'special-mode-hook 'my-mode-hook)

After the project file has been loaded and projmake mode activated,
each time you save a file in Emacs, the projmake mode tries to build
your project.
