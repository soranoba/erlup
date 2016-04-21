# [WIP] erlup
Upgrade tools for Erlang/OTP. Contains rebar3 plugin and escript.

## Overview
It is a tool to support the upgrade and downgrade of OTP application.

1. Automatic generation of the appup file. [See also](doc/erlup_appup.md).

## Usage

### Using rebar3

```erlang
%% rebar.config
{plugins, [erlup]}.
```

```bash
$ git checkout v0.0.1
$ rebar3 release        # make a previous version package

$ git checkout v0.0.2
$ rebar3 release        # make a current version package

$ rebar3 erlup appup    # generate the appup files (v0.0.1 -> v0.0.2)
$ rebar3 erlup relup    # generate the relup file  (v0.0.1 -> v0.0.2)
```

### Using escript

```bash
$ git clone https://github.com/soranoba/erlup
$ cd erlup
$ make
```

```bash
$ ls /usr/local/myapp/releases         # previous versions
0.0.1 0.0.2 RELEASES  start_erl.data
$ ls /tmp/myapp/releases               # a new version
0.0.3 RELEASES start_erl.data

$ erlup appup --dir /tmp/myapp,/usr/local/myapp -p 0.0.1 -c 0.0.2
$ erlup relup --dir /tmp/myapp
```

### More information

```
$ rebar3 help erlup
$ erlup -h
```

## Configure file

If you use the rebar3.
```
%% rebar.config
{erlup,
 [
  {appup, [{extra, {[], []}}]},
  {relup, []}
 ]}.
```

If you use the escript.
```
%% erlup.config
{appup, [{extra, {[], []}}]}.
{relup, []}.
```
The escript can read in the form of a rebar3.config.

### Attributes

|group| key  | value                                     | description                                                      |
|:----|:-----|:------------------------------------------|:-----------------------------------------------------------------|
|appup|extra |`{UpExtra :: term(), DoownExtra :: term()}'| Specify the extra at code_change.                                |
|     |deps  |`{module(), ModDeps :: [module()]}'        | Definition of dependency between modules.                        |
|     |applys|`{Func :: atom(), UpArgs, DownArgs}'       | If the function has been export, to run at upgrade and downgrade.|
