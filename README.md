What is This?
============

This is a spin-off of the project [Angel](https://github.com/jamwt/Angel/), which is centralized around managing
multiple processes, this will only wrap a single process in a unix environment,
And I plan to build loads of useful stuff into it : ).

Written with a defensive mindset using Haskell, usually a Win unless you
accidentally screw up.


Gabriel will try to protect a running process from the big bad world by
implementing normal daemonizing procedures described
[here](https://github.com/toyvo/hdaemonize), which will basically be the parent
process, wrapping a child process and keeping an watchful eye on the process'
state.

The following defensive measures are taken to protect the process:

 * Any time to process exits, wait and then restart the process
 * Run a heartbeat command, checking the integrity of the child process (you
   implement this) (coming soon)
 * HUP the parent process to implement changes in the command, this will cause
   TERM to be sent to the child process, and it will (hopefully) terminate
   nicely.

