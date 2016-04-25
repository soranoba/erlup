erlup
========

## Command

- <a href="appup.md">appup</a>
- <a href="relup.md">relup</a>
- <a href="tarup.md">tarup</a>

## Log level

It use `rebar_log'.
So, even if you use the escript, it is possible to specify the log level in the same manner as when using the rebar3.

```bash
$ QUIET=1 erlup    # only display errors
$ DEBUG=1 erlup    # show debug output
```
