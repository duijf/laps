## Dependencies

Laps understands dependencies between commands, services, and watches
(hereafter called units). Units can have two kinds of dependencies:

 - `A` requires completion of `B`.
 - `A` requires `B` to be started.

Real world examples:

 - The `db` unit requires completion of `db-setup` before it can start.
 - The `app` unit requires the `db` unit to run next to it.

In Laps, you would model this with:

```toml
[command.db-setup]
description = "Setup database"
exec = ["/usr/bin/initdb", "foo"]

[serivce.db]
description = "Run database"
exec = ["postgres", "-D", "foo"]
after = ["db-setup"]

[serivce.app]
description = "Run app"
exec = ["cargo", "run"]
wants = ["db-setup"]
```

When you run `laps app`, Laps will take care to:

 - Run `db-setup` first and wait for it's completion.
 - Start `db` and subsequently `app`.

At the moment, Laps does not do anything special when `db` terminates with a
normal exit code. In most real-world scenario's, `app` would likely crash or
log a lot of errors.
