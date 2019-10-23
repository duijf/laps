# laps

Laps is a project automation tool. Status: pre-alpha.

## Ideas

Better error reporting for spawned tasks.

Start processes in a a pseudo-terminal.

Watches include a specific file whitelist as well.

Allow for `[environment.<sub>]` objects and make those flags for `laps`. E.g.
an `[environment.dev]` section corresponds to a `laps --dev` flag.

Nix integration.

Implement file watching logic in Rust instead of relying on scripts.

Filtered output / ncurses app when running multiple services.

Make it possible to filter loglevel of apps dynamically as long as they perform
some structured logging. (Either: define protocol, or users specify regex)

Filter on stderr / stdout.

## Implemented

Reasonable error reporting for config validation.

Make scripts use a random filename when executing. (Avoids race conditions and
clashes.)

Interpolate environment variables in command strings. (Escape the dolar sign
with `$$`).

Make laps able to start more than 1 command.

Ensure that all subprocesses are killed when the main process is. We use UNIX
process groups for sending signals to multiple PIDs.

Signal handling. Wait for child termination before exiting.

Clean up temp files on termination.

Plan the execution of services and dependencies.

## Inspiration

Laps borrows a lot of ideas from the following tools:

 - SystemD
 - Foreman
 - Dotenv
