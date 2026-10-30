# Resource pack `mono`

Applies when the project uses the `mono` resource pack
(`resource-pack: exe:mono`; check `resourcePack` in `decker info --json`). The
pack enables `experiments.fragment-templates` and `page-features.codapi` by
default, so fragment templates and live code work without further
configuration.

```yaml {.decker-example file="decker.yaml" project="mono"}
resource-pack: exe:mono
```

## Additional fragment templates

| Template                             | Invocation                              | Result                          |
|--------------------------------------|-----------------------------------------|---------------------------------|
| `wen`                                | `[§wen](Article_Name "Display Text")`   | English Wikipedia link          |
| `wde`                                | `[§wde](Artikel_Name "Anzeigetext")`    | German Wikipedia link           |
| `wp`                                 | `[§wp en](Article_Name "Display Text")` | Wikipedia link, language `arg1` (default `de`) |
| `wp-cc`                              | `[§wp-cc sa 4.0](File_Name.png)`        | Wikimedia file + CC BY licence note |
| `mdn`                                | `[§mdn](CSS/display "display")`         | MDN Web Docs link (German)      |
| `unit`, `subunit`, `assign`          | `[§unit](07 "Title")`                   | Index links to `unit/<url>-deck.html`, `unit/<url>/<url>-deck.html`, `assignments/<url>-page.html` |
| `unit-nl`, `subunit-nl`, `assign-nl` | `[§unit-nl](07 "Title")`                | Same entries without a link (not yet published) |
| `nop`                                | `[§nop](x)`                             | Renders nothing                 |

```markdown {.decker-example file="links-deck.md" project="mono"}
# Links

- [§wen](Haskell_(programming_language) "Haskell")
- [§wde](Rekursion "Rekursion")
- [§mdn](CSS/display "display")
```

## Live code (Codapi)

Declare the Codapi server once per deck or page. Runnable snippets use the
`live-code` template. **The trailing dash in `.live-code-` is intentional** —
never "fix" it to `.live-code`:

````markdown {.decker-example file="live-deck.md" project="mono"}
# Live code

[§live-code-server](https://example.com/codapi/v1)

```{#base .live-code- language="Haskell" sandbox="GHCi"}
x = 42
```

```{.live-code- language="Haskell" depends-on="base"}
main = print x
```
````

- `depends-on="<id>"` runs the code of another snippet first.
- `sandbox` names the Codapi sandbox (default: the language); `engine`
  defaults to `codapi`. Other attributes are passed through to
  `<codapi-snippet>`.

See the [users' guide](https://github.com/decker-edu/decker/blob/develop/users-guide/decker-users-guide.md), section *Live code*.
