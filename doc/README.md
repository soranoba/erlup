erlup
========

## Command

- <a href="appup.md">appup</a>
- <a href="relup.md">relup</a>
- <a href="tarup.md">tarup</a>
- <a href="vsn.md">vsn</a>

## Log level

It use `rebar_log'.
So, even if you use the escript, it is possible to specify the log level in the same manner as when using the rebar3.

```bash
$ QUIET=1 erlup    # only display errors
$ DEBUG=1 erlup    # show debug output
```

## Version

When you use erlup which version is not published to hex, version maybe include git hash.

```bash
$ erlup -v                           # 0.2.0 (on hex), v0.2.0 (on github)
erlup v0.2.0
$ erlup -v
erlup v0.2.0+build.3.refebdd68a07e   # The version in development. git hash : ebdd68a07e
```
