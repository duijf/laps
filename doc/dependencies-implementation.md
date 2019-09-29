## Dependency implementation

This document discusses the implementation of the Laps dependency model.

Requirements on the implementation:

 1. Does not need to handle dependency cycles<sup>*</sup>.
 2. Make as much progress as possible.
 3. Units don't always terminate (or run very long).
 4. Support units that run in parallel.

<sup>*</sup> This needs precise definition at some point. We want to give a
helpful error message in case a user accidentally ends up in this case. We also
want to detect cycles before we start actual execution. Maybe we can allow
cyclical `wants_started`, but not `wants_finished`?

## Topological sort

My first idea was to sort the dependeny graph into a list and execute as much
as possible. (This is called a [topologicale sort][topo-sort]). This isn't
ideal because of requirements 2 and 3.

To see why, consider the following example:

```toml
[commands.a]
exec = ...

[commands.b]
exec = ...
wants-finished = ["a"]

[commands.c]
exec = ...
wants-finished = ["a"]
```

Or in graph form (arrow means after):

```
    A
  ↗  ↖
 B     C

(1): Dependency graph
```

And the following execution order (arrows flipped):

```
    A
  ↙  ↘
 B     C

(2): Execution graph
```

You can execute the tasks belonging to these nodes in two orders such that the
dependencies are preserved:

```
A B C   (1)
A C B   (2)
```

Now consider that `B` does not terminate. If we had picked ordering `(1)` and
executed it linearly, we'd have a problem: we'd never get to actually execute
`C`. Annoying.

Given that [we cannot predict nontermination][hp], we'll need a more powerful
plan output than a static ordering.

 [topo-sort]:https://en.wikipedia.org/wiki/Topological_sorting
 [hp]:https://en.wikipedia.org/wiki/Halting_Problem
