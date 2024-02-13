---
title: Thebe Test
thebe:
  bootstrap: true
  requestKernel: true
  useBinder: true
  mathjaxUrl: false
---

# Code on slides

Embed code fragments on your slides:

~~~markdown
```{ .no-highlight executable="true" language="python" }
# This is a runnable python cell.
print("Hello World")
```
~~~

# Thebe Binder result

```{ .no-highlight executable="true" language="python" }
# This is a runnable python cell.
print("Hello World")
```

Press start and wait. If nothing happens, check the console.

Binder may have a rate limit exceeded error.

# Thebe Binder result

```{ .no-highlight executable="true" language="python" }
# A local jupyter should run in the expected directory
!pwd
```

This should yield some path on some container image.
