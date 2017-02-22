0.3.1 (unreleased)
---- 
- Don't export `Delayed_backend` yet

0.3 (unreleased)
----
- Use topkg
- Use new mirage-time and mirage-clock modules
- Adapt to MirageOS version 3 errors scheme

0.2 10-09-2016
----
- New call `unregister_and_flush` lets a node wait for all its listener
  callbacks to return before unregistering from the backend (#4)
- Support more than 254 connected nodes

0.1 30-04-2015
----
- First release
