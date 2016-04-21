

# Module erlup_appup #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Automatically generate the .appup files from the beam file.

Copyright (c) 2016 Hinagiku Soranoba All Rights Reserved.

__Behaviours:__ [`provider`](provider.md).

<a name="description"></a>

## Description ##

### Overview

1. Support the multiple versions upgrade and downgrade.
2. You can define the extra and original code_change functions.

### Usage

```
     bash
  $ rebar3 help erlup appup  # If you use the rebar3.
  $ erlup appup -h           # If you use the escript.
```

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#do-4">do/4</a></td><td>Automatically generate the .appup files from the beam file.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="do-4"></a>

### do/4 ###

<pre><code>
do(Dirs::[<a href="file.md#type-filename">file:filename()</a>], PreviousVsn::string(), CurrentVsn::string(), State0::<a href="erlup_state.md#type-t">erlup_state:t()</a>) -&gt; ok
</code></pre>
<br />

Automatically generate the .appup files from the beam file.

