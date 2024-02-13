---
title: Thebe-lite Test
thebe:
  bootstrap: true
  requestKernel: false
  useBinder: false
  useJupyterLite: true
  mathjaxUrl: false

jupyter:
  litePluginSettings:
    '@jupyterlite/pyodide-kernel-extension:kernel':
      pipliteUrls: ["https://unpkg.com/@jupyterlite/pyodide-kernel@0.0.7/pypi/all.json"]
      pipliteWheelUrl: "https://unpkg.com/@jupyterlite/pyodide-kernel@0.0.7/pypi/piplite-0.0.7-py3-none-any.whl"
    disabledExtensions: [jupyterlab-kernel-spy, jupyterlab-tour]
    terminalsAvailable: false
  enableMemoryStorage: true
  settingsStorageDrivers: [memoryStorageDriver]
---

# Code on slides

Embed code fragments on your slides:

~~~markdown
```{ .no-highlight executable="true" language="python" }
# This is a runnable python cell.
print("Hello World")
```
~~~

# Thebe-lite result

```{ .no-highlight executable="true" language="python" }
# This is a runnable python cell.
print("Hello World")
```

Press start and wait. If nothing happens, check the console.

Jupyter-Lite takes a while to start.

# Thebe-lite result

```{ .no-highlight executable="true" language="python" }
# Not supported in jupyter-lite:
!pwd
```
