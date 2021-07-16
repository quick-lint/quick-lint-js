# quick-lint-js Sublime Text plugin documentation

> The internal strategy used is to share information between all views that belong to the same buffer. Because that way, if there are multiple views (tabs) of the same buffer (file), they will all apply the same changes (have squiggly underlines and pop-ups available).

Before the commit a28ae0ef18185fc9bed853c9724e3fa82f16bafe, when we had multiple views/tabs for the same buffer/file, only the first view (primary view) would have squiggly underlines and pop-ups available. Now all views/tabs have.

| BEFORE THE COMMIT | AFTER THE COMMIT |
| ----------------- | ---------------- |
| ![out1][out01]    | ![out2][out02]   |

[out01]: images/out01.png
[out02]: images/out02.png

> Won't `on_load` get called anyway? Why do we need to explicitly lint in `__init__`?

Sublime Text calls the `__init__` method every time the tab (view) is loaded.

Sublime Text calls the `on_load` method every time the file (buffer) is loaded.

We need to explicitly lint in `__init__` because there are some situations where Sublime Text has already loaded the file, and, in these situations, Sublime Text calls `__init__` and on_load doesn't:

```
The `__init__` method was called.
Timestamp: 1626315449.1726804
File content: const 🎸 = "asdf";
```

We need to explicitly lint in `on_load` because there are some situations where Sublime Text needs to load the file, and, in these situations, Sublime Text calls `__init__` and `on_load` too, when `__init__` runs lint, lint gets an empty text, but when `on_load` runs lint, lint receives the entire file content:

```
The `__init__` method was called.
Timestamp: 1626315480.3791265
File content: 

The `on_load` method was called.
Timestamp: 1626315480.4387884
File content: const 🎸 = "asdf";
```
