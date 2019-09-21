# laps

Laps is a project automation tool.

## Ideas

Make laps able to start more than 1 command.

Ensure that all subprocesses are killed when the main process is. There should
be a Linux cgroup API call/thing for this.

Allow for `[environment.<sub>]` objects and make those flags for `laps`. E.g.
an `[environment.dev]` section corresponds to a `laps --dev` flag.

Start processes in a a pseudo-terminal.

Make scripts use a random filename when executing. Avoids race conditions and
clashes. Also: use laps in the description; for watches it might be a bit
awkward.

Interpolate environment variables in command strings.

Implement file watching logic in Rust instead of relying on scripts.
