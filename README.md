# laps

Laps is a project automation tool. Status: pre-alpha.

## Ideas

Plan the execution of services and dependencies.

Signal handling. Wait for child termination before exiting.

Clean up temp files on termination.

A display thing for error messages. Include more meta-data for the current
error enum. Don't return boxed errors.

Make laps able to start more than 1 command.

Ensure that all subprocesses are killed when the main process is. There should
be a Linux cgroup API call/thing for this.

Start processes in a a pseudo-terminal.

Watches include a specific file whitelist as well.

Allow for `[environment.<sub>]` objects and make those flags for `laps`. E.g.
an `[environment.dev]` section corresponds to a `laps --dev` flag.

Implement file watching logic in Rust instead of relying on scripts.

Filtered output / ncurses app when running multiple services.

Make it possible to filter loglevel of apps dynamically as long as they perform
some structured logging. (Either: define protocol, or users specify regex)

Filter on stderr / stdout.

Virtual FS / chroot like features.

Some caching stuff?

## Implemented

Make scripts use a random filename when executing. (Avoids race conditions and
clashes.)

Interpolate environment variables in command strings. (Escape the dolar sign
with `$$`).
