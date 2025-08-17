# Thebe Plugin

For more information on Thebe, see <https://github.com/executablebooks/thebe>.

This **plugin needs configuration in `decker.yaml`**, otherwise it will not start.

## Usage in slides

In your slides simply use, e.g.,
~~~markdown
```{ .no-highlight executable="true" language="python" }
# This is a runnable python cell.
print("Hello World")
```
~~~

Thebe looks for cells with `data-executable="true"`.
If no executable cells are found, the plugin will also not launch Thebe.

This is intentional as one deck may have executable cells, while the other does not, but the might use the same template.

The plugin will also *not* launch if there is no configuration in `decker.yaml`.

The recommended use is with a *local jupyter*, for students it may be nicert to use *binder* or *jupyter-lite*.

## Setup and Configuration

In your template, enable the `thebe` plugin by adding the CSS
```html
<link rel="stylesheet" href="$decker-support-dir$/plugins/thebe/thebe.css">
```
respectively the JavaScript
```js
import thebePlugin from './$decker-support-dir$/plugins/thebe/thebe.js';
```
and enabling the `thebePlugin`.

Add the following to `decker.yaml`:
```yaml
thebe:
  bootstrap: true
  requestKernel: true
  useBinder: true
  useJupyterLite: false
  serverSettings:
    appendToken: true
    baseUrl: http://localhost:8888
    token: "token-for-your-jupyter"
    wsUrl: ws://localhost:8888
  mathjaxUrl: false
  mountActivateWidget: false
  mountStatusWidget: false
  mountRunAllButton: true
  mountRunAllButton: false
  mountRestartButton: false
  mountRestartallButton: false
  codeMirrorConfig:
    lineNumbers: false
    scrollbarStyle: native
```

For configuration settings, see <https://thebe.readthedocs.io/en/stable/config_reference.html>.

- `useBinder`: set to false to use jupyterlite or a local jupyter\
  Note: binder containers have a 10 minute timeout, a local jupyter is preferrable
- `useJupyterLite`: set to true to use jupyterlite\
  this requires loading additional javascript (thebe-lite)
- `serverSettings`: for connecting to a local jupyter
- `mathjaxUrl`: set to false, as we already loaded a newer MathJaX than used by Jupyter
- `mount*`: choose the widgets you like
- `codeMirrorConfig`: configure the editor component\
  see <https://codemirror.net/5/doc/manual.html>

## CSS

To set the fonts, customize:

```css
.reveal {
  --jp-code-font-family: "Fira Code", monospace;
  --jp-content-font-family: "Fira Sans", sans-serif;
}
```

Jupyter adds a *huge* amount of CSS, unfortunately, including its own code highlighting that you may want to customize.

Other themes need additional CSS. We try to map the default theme to the Decker colors.

## Local Jupyter

The most controllable way is to use a local Jupyter lab instead of Binder.

Set `useBinder: false`

Start a local jupyter with the desired token:
```bash
jupyter lab --IdentityProvider.token=token-for-your-jupyter "--ServerApp.allow_origin=*"
```

This makes it easiest to pre-install packages, but it is harder to *share* the slides with students – they need to launch a similar jupyter server on their local machine to run your code.

## Binder

In this mode, a container will be launched on BinderHub. This can take a bit to start up, and the binder containers have inactivity timeouts of as little as 10 minutes.

Right now, **there is no automatic kernel status indicator**. So you cannot easily see if you are connected and/or retry.
<https://github.com/executablebooks/thebe/issues/697>

By default, Binder relies on mybinder.org to provide free containers, operated for example by [OVH](https://www.ovh.com/),
[GESIS](https://notebooks.gesis.org/) or [Curvenote](https://curvenote.com/). But sometimes these services are unavailable or fail, and there might be privacy issues involved with using hosted services.

But this will allow others to also run these cells on your own.

## thebe-lite

Set `useBinder: false` and `useJupyterLite: true`.

Thebe has a mode of operation using Pyodide in the client, without installation. This comes with some limitations, though:

- worse startup, as an entire Python is loaded into the browser
- some packages work, others do not: pytorch for example
- some function such as urlretrieve are limited by the browsers guardrails to prevent cross-site attacks

This requires additional configuration, in paticular if you want a custom installation.

The benefit is that if you share your slide, the users can also execute the cells within their own browser.

## Updating

To update the thebe libraries, first `npm install thebe thebe-lite`.

Right now, I have been using the 0.9.0-rc release candidates. Configuration names have changed across versions, so you may need to make some updates...

Put the assets into `decker/resource/decker/support/vendor/thebe`,
figuring out the right assets to copy is a bit of a mess.

- `*thebe-lite.min.js` from `thebe-lite/dist/lib/`, if you intend to use this (optional)
- `index.js` and `thebe.css` from `thebe/lib/`

The configuration of jupyter-lite is *separate* from Thebe.

Add this to your `decker.yaml` to configure JupyterLite:
```yaml
jupyter:
  litePluginSettings:
    '@jupyterlite/pyodide-kernel-extension:kernel':
      pipliteUrls: ["https://unpkg.com/@jupyterlite/pyodide-kernel@0.0.7/pypi/all.json"]
      pipliteWheelUrl: "https://unpkg.com/@jupyterlite/pyodide-kernel@0.0.7/pypi/piplite-0.0.7-py3-none-any.whl"
    disabledExtensions: [jupyterlab-kernel-spy, jupyterlab-tour]
    terminalsAvailable: false
  enableMemoryStorage: true
  settingsStorageDrivers: [memoryStorageDriver]
```

Note: memory storage means the state is lost when reloading the slides (likely the desired behavior).

Or use your own copy of pyodide if you do not trust unpkg.com.

## Known issues

1. ~~Input cursor placement is broken, depending on the browser window size.~~

This issue has been largely resolved, by applying a reverse scale to certain layers of the editor.
Once Thebe switches to CodeMirror 6, this should also no longer be an issue.

2. Output cells may overflow.

3. When starting the kernel fails (e.g., Binder not available), only an error is logged to the console, but there is no user feedback. Needs to be fixed in Thebe, <https://github.com/executablebooks/thebe/issues/735>

## TODO

To make this easier usable by non-hackers:

- [x] fix the editor component
- [x] dark mode 😎
- [x] add a kernel status widget via JavaScript to every cell
- [ ] menu configuration to choose local jupyter or binder!
- [ ] figure out how to set max-height to output + scrollable
- [ ] make an example with prerendered output
- [ ] make thebe-lite easier to configure for self-hosted use (GDPR)
- [ ] add invisible, but executed, Python cells for context setup

