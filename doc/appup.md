appup
==========
Automatically generate the .appup files from the beam file.

## Overview

1. Support the multiple versions upgrade and downgrade.
2. You can define the extra and original code_change functions.

## Usage

```
$ rebar3 help erlup appup  # using rebar3.
$ erlup appup -h           # using escript.
```

## Configuration

```erlang
%% rebar.config
{erlup, [{appup, Configs}]}.

%% erlup.config
{appup, Configs}.
```

### Global configuration

When the `term()` to specify the following `atom`, it use the actual value.

- `'$from'`     : The version of the old release (it isn't the version of application)
- `'$to'`       : The version of the new release
- `'$from_vsn'` : The version of the old application (there is the `vsn` in the `.app` file)
- `'$to_vsn'`   : The version of the new application

### Applys

If changed module export the given function, the function is executed at the time of the upgrade / downgrade.

In the case of upgrade, it will be executed **after** the module has been loaded.  
In the case of downgrade, it will be executed **before** the module has been loaded.

So, it runs on the always new module.

```erlang
{appup, [{applys,
          [
           {Function :: atom(), UpArgs :: [term()], DownArgs :: [term()]}
          ]}
        ]}.
```

Length of `UpArgs` and `DownArgs` **MUST** be the same.  
`Function` **MUST NOT** use `code_change` and `system_code_change`.

### Deps

It specifies the update order of the module.

```erlang
{appup, [{deps,
          [
           {Mod :: module(), ModDeps :: [module()]}
          ]}
        ]}.
```

Load the `ModDeps` before loading the `Mod`.

### Extra

It specifies the Extra that is argument of [code_change](http://erlang.org/doc/man/gen_server.html#Module:code_change-3) and
 [system_code_change](http://erlang.org/doc/man/sys.html#Mod:system_code_change-4).

```erlang
{appup, [{extra, {UpExtra :: term(), DownExtra :: term()}}]}.
```
