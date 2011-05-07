README
======

This is a spin-off of the project [Angel](https://github.com/jamwt/Angel/), which is centralized around managing
multiple processes, this will only wrap a single process in a unix environment,

And I plan to build some really useful stuff into it.

Install
-------
Project requires ghc and cabal.

  #> cabal install

Make sure that ~/.cabal/bin is in your path.

Features
--------

* Manages processes by wrapping them with a special parent process which
  handles signal escalation (TERM -> wait -> KILL) on processes which have
  hung.
* Will restart the managed process when it exists, with a configurable delay.
* Zero configuration, default file location depends on the current working
  directory or the '--cwd' parameter, each file can be configured separately
  aswell.
* The last run command is stored in a special file which allows for
  re-invocation by default (no parameters).

For more information and examples, go to http://toolchain.eu/project/gabriel
