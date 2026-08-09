---
title: Thebe-local Test
thebe:
  bootstrap: true
  requestKernel: false
  useBinder: false
  mathjaxUrl: false
  serverSettings:
    appendToken: true
    baseUrl: http://localhost:8998
    token: "test-jupyter"
    wsUrl: ws://localhost:8998
---

# Jupyterlab

For this experiment, you need a local jupyter.

Launch jupyter using

``` { style="font-size:smaller" }
jupyter lab --port 8998 \
--IdentityProvider.token=test-jupyter \
"--ServerApp.allow_origin=*"
```

or jupyter notebook using
``` { style="font-size:smaller" }
jupyter notebook --port 8998 \
--NotebookApp.token=test-jupyter \
"--NotebookApp.allow_origin=*"
```

Then continue.

# Code on slides

Embed code fragments on your slides:

~~~markdown
```{ .no-highlight executable="true" language="python" }
# This is a runnable python cell.
print("Hello World")
```
~~~

# Thebe result

```{ .no-highlight executable="true" language="python" }
# This is a runnable python cell.
print("Hello World")
```

This should start much faster with a local jupyter!

# Thebe result

```{ .no-highlight executable="true" language="python" }
# A local jupyter should run in the expected directory
!pwd
```
