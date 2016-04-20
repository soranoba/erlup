# [WIP] erlup
Upgrade tools for Erlang/OTP. Contains rebar3 plugin and escript.

## Overview

## Usage

If you use the rebar3.
```
{plugins, [erlup]}.
```

If you use the escript.
```
$ git clone https://github.com/soranoba/erlup
$ cd erlup
$ make
$ ./erlup -h
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
