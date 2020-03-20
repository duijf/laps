# Ideas

## Watches

 - Expand the options for the watch configuration.

 - Implement file watching logic in code instead of relying on external tools.

 - Come up with a default for watches. Either: enable by default. Or: give
   every command with a watchconfig a `--watch` key so people don't need to
   define things twice.

## Nix integration

 - Actually implement support for pure shells and attributes.

 - Rely less on the different Nix shell commands. Add some deeper integration.

 - Minimize the amount of Nix evaluations in some way / cache Nix evaluation
   results in some manner.

## Start multiple processes

 - Allow the user to start multiple processes for a single command. (Without
   having to resort to scripting it themselves).

 - Multiple start orders: linear/in series and parallel. Also combinations of
   these.

 - Erlang style process supervision trees and (re)starting strategies.

 - Ensure that all subprocesses are killed when the main process is. We use
   UNIX process groups for sending signals to multiple PIDs.

 - Health checks / verification of a command being started other than having
   called `exec`. (e.g. `pg_isready`)

 - Depending on UI choices: Start processes in a a pseudo-terminal so they
   don't change their output from normal (because they assume output
   redirection to a file).

 - Handle and propagate signals like the user would expect so all processes are
   killed after Laps has stopped running. Maybe add support for non-POSIX stuff
   like setting `prctl` flags on Linux.

## UI

 - Optional terminal UI where users can control what they see. Things they would
   be able to do is control output per process.

 - Provide configuration about filtering process output.

 - Dynamic filtering of output of processes output stream.

 - Structured logging support? Maybe better left to a separate program.

## Escape hatches

 - Laps should provide as many convenient building blocks so users can easily
   create commands and workflows that they need.

 - When Laps does not provide a workflow for something, users should be
   able to define one in whatever way they find convenient. Drop down to an
   arbitrary scripting language (as long as it can be run using a shebang).
   Maybe even add support for a select few compiled languages for utility
   stuff. (e.g. Haskell, C, Rust and maybe also Go)

## Documentation

 - Explain (and think about) the goals that Laps is trying to achieve and how
   its features contribute to these goals.

    - Put knowledge and workflows into code. This type of knowledge would
      otherwise exist in some people's heads or in the best case end up in a
      comment, wiki or another low-visibility place.

    - Enable a "single command for most things you care about" workflow.

    - Programmable configuration. Do not assume we can implement all workflows
      that users care about. Provide building blocks so users can use Laps, but
      also extend it. Give the Dhall sales pitch about programmable config with
      imports, functions, types, and how that hopefully keeps the core of Laps
      simple.

    - Facilitate "escape hatches".

    - Great UX for users enabling workflows with Laps configuration. E.g. high
      quality error messages, type safety, allowing abstraction. We'll need to
      see if the Dhall error messages are good enough for this.

    - Great UX for users using Laps on the CLI.

 - Explain what Laps is, is not, what the alternatives are, and why you would
   (not) want to use Laps.

 - Add a minimal configuration file and explain how that ties in with the goals
   that Laps is trying to achieve. Show off some nice features.

 - Add screenshots / a screencast.

 - API documentation for what will become the standard library.

 - Add liberal usage examples and how-to's. Specific examples:

    - Watches
    - Multi-process stuff and supervision
    - Nix integration

 - Find a way to keep documentation up to date / test it.

 - Credit sources of inspiration: foreman, overmind, systemd, erlang, dotenv,
   lorri.

## Miscelaneous features and enhancements

 - Allow specifying environment variables for commands in some way. Maybe
   expose this via flags.

 - Command aliases: `laps foo -- bar baz --quix` passes `bar baz --quix` to the
   command or script defined in the `foo` command. Not sure how this would
   interact with starting multiple processes.

 - Validate a few required invariants asssertions about the config format once
   fully settled on the types. Report error messages nicely.

 - Provide a Dhall module / record so users can have a single import. Host it
   at some convenient place.

 - Convenience setting / function allowing users to discard process output
   unless crashes or errors occur.
