# Fragment templates

Fragment templates are HTML snippets expanded from special links or code
blocks. They are **off by default**; enable them in `decker.yaml`. Check
`decker info --json` (`config.experiments`): a resource pack may enable them
already.

```yaml {.decker-example file="decker.yaml" project="templates"}
experiments:
  fragment-templates: true
```

Invocation, as a link or as a code block whose class ends in a dash:

````markdown {.decker-example file="templates-deck.md" project="templates"}
# Templates

[§wikipedia en](Haskell_(programming_language) "Haskell")

[wikipedia- en](Lambda_calculus "Lambda calculus")

```{.live-code- language="Haskell"}
main = print 42
```
````

The trailing dash in `.live-code-` and `wikipedia-` is the template marker —
do not "fix" it to `.live-code`.

If the experiment is off, invocations stay in the output as plain links and
code blocks. The build reports `fragment-templates-disabled`:

```markdown {.decker-example file="off-deck.md" expect="fragment-templates-disabled"}
# Templates

[§wikipedia en](Haskell_(programming_language) "Haskell")
```

## Lookup

The template `<name>.html` is searched in this order:

1. `<name>.html` relative to `decker.base-dir` (the directory of the document),
2. `templates/<name>.html` in the project,
3. `support/templates/<name>.html` in the resource pack, then in Decker.

An unknown name is a hard error (`fragment-template-not-found`).
`decker info --json` lists the templates of the project, the pack and Decker
(`fragmentTemplates`).

## Arguments

Templates are doctemplates (`$name$`, `$if(name)$…$endif$`). They receive:

| Variable                  | Value                                                  |
|---------------------------|--------------------------------------------------------|
| `url`, `title`            | Link target and title (link form)                      |
| `arg1`, `arg2`, …, `args` | Words after the name in the link text; `argn` = count  |
| `id`                      | The element id, or a random id                         |
| `class1`, …, `classes`    | Classes of the link or code block                      |
| `<key>`, `attribs`        | Key/value attributes (`data-` prefix removed)          |
| `code`, `codeEsc`         | Code block content, raw and HTML-escaped (code form)   |
| `rnd-id`, `caption`       | Random id and `caption` attribute (code form)          |
| `meta`                    | The document's meta data                               |

## Templates shipped with Decker

| Template                     | Use                                                       |
|------------------------------|-----------------------------------------------------------|
| `wikipedia`                  | `[§wikipedia en](Haskell "Haskell")` — Wikipedia link (`arg1` = language, default `de`) |
| `live-code`                  | Code block `.live-code-` — runnable Codapi snippet (needs `page-features.codapi` and a Codapi server) |
| `live-code-server`           | `[§live-code-server](https://host/codapi/v1)` — set the Codapi server (or meta `codapi.server`) |
| `live-code-prelude`          | Code block `{#id .live-code-prelude-}` — wrapper containing `##CODE##`, used with `template="#id"` |
| `grid`, `finegrid`, `canvas` | `[§grid](300px "Caption")` — empty drawing area; `url` is the height |

See the [users' guide](https://github.com/decker-edu/decker/blob/develop/users-guide/decker-users-guide.md), sections *Fragment templates* and *Live code*.
