erlup
========
[![Build Status](https://travis-ci.com/soranoba/erlup.svg?branch=master)](https://travis-ci.com/soranoba/erlup)
[![hex.pm version](https://img.shields.io/hexpm/v/erlup.svg)](https://hex.pm/packages/erlup)

Upgrade tools for Erlang/OTP. Contains rebar3 plugin and escript.

## Overview
It is a tool to support the upgrade and downgrade of OTP application.

1. Automatic generation of the appup files. [See also](doc/appup.md).
2. Automatic generation of the relup file. [See also](doc/relup.md).
3. Create a rel file in the tar file for the upgrade / downgrade. [See also](doc/tarup.md)
4. Display the release vsn. [See also](doc/vsn.md).

Some of the library already exists, but they has many defects.
For example, supporting downgrade and create the more safety appup.
If you want to know in detail, please refer to the [documentation](doc) of each command.

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
$ make escriptize
```

```bash
$ ls /usr/local/myapp/releases         # previous versions
0.0.1 0.0.2 RELEASES  start_erl.data
$ ls /tmp/myapp/releases               # a new version
0.0.3 RELEASES start_erl.data

$ erlup appup -p 0.0.1 -c 0.0.2 -d /tmp/myapp -d /usr/local/myapp
$ erlup relup -c 0.0.2 -d /tmp/myapp -d /usr/local/myapp
```

### More information

[Documents](doc)

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

## Contribute

Pull request is welcome =D

## License

[MIT License](LICENSE)
