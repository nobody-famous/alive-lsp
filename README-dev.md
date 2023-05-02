# Alive-lsp Development

Notes on making changes to the Alive-lsp codebase.

## Basic Knowledge

Alive-lsp is primarily constructed using Lisp.
Since Alive provides a language extension to support Common Lisp
a basic understanding of that language and the Lisp REPL is assumed.
The specific Common Lisp implementation is Steel Bank Common Lisp.

The Alive-lsp codebase is hosted on
[github](https://github.com/nobody-famous/alive-lsp).
Basic understanding of `git` and `github` usage
including branches and forking is necessary.

Basic understanding of the Alive project itself is necessary.
Make sure to read the Alive
[development notes](https://github.com/nobody-famous/alive/blob/master/README-dev.md).

### VSCode Language Server Protocol

VSCode language extensions (such as Alive) are supported by a separate server
that provides the Language Server Protocol (LSP) for the particular language.

Developing LSP servers is quite involved.
Start learning with the
[Language Server Extension Guide](https://code.visualstudio.com/api/language-extensions/language-server-extension-guide).

## Startup

### Install Tools

Install the following tools:
* git
* Steel Bank Common Lisp (SBCL)

You should be able to execute the following programs:
* `git` (or whatever tool you prefer)
* `sbcl` Common Lisp REPL

The rest of this document assumes the command-line use of `git`.
You may prefer a different tool (including VSCode).

### Install Alive

Alive must be installed into VSCode to do testing of Alive-lsp.

Most Alive-lsp development will require working on both Alive and Alive-lsp
since the two communicate frequently.
Download the Alive codebase as described in the Alive
[development notes](https://github.com/nobody-famous/alive/blob/master/README-dev.md).

### Acquiring the Code

Alive-lsp development is done on a fork of the repository.
Pull Requests (PRs) are made from a branch in that fork
into the Alive repository.

* [Create a fork](https://docs.github.com/en/get-started/quickstart/fork-a-repo)
  of the Alive code into your own `github` account.
* [Clone your fork](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository)
  onto your development machine.

This is the same process documented for Alive development.

### Inital Steps

In the `.vscode/settings.json` file set `alive.lsp.install.path`
to point to the root directory where the Alive-lsp code is installed.
For example:

    "alive.lsp.install.path": "/home/<user>/work/lisp/alive-lsp",

This will cause the Alive extension installed into VSCode to find the
source code for the Alive-lsp project instead of downloading the latest release.
That way edits made to Alive-lsp code will be executed during testing.
_Avoid checking this change back into the repository_.

Test this by bringing up VSCode in the Alive-lsp directory.
Show the `Output` view at the bottom of the screen (`<ctrl-K><ctrl-H> or
the **Output: Focus on Output View** command) or just select the tab on the bottom panel.
On the right part of the title bar there is a dropdown to show output from different threads.
Choose the `Alive LSP` thread which should be available assuming that
the Alive extension is properly installed and the Alive-lsp server is running.

The Output view should show the following:
```
; compilation finished in <duration>
* [4/30/2023 17:02:03][INFO] Started on port <port-number>
```

Edit the file `src/server.lisp` to add an extra message on startup:
```
(defun start (&key (port *default-port*))
    (if *server*
        (logger:msg logger:*error* "Server already running")

        (progn (logger:init *standard-output* logger:*info*)
               (setf *server* (make-instance 'lsp-server))
               (start-server *server* port))))
```
The added line shows "Happy!".

Use the **Developer: Reload Window** command to restart the extension, restarting the Alive-lsp server.
The Output view should now look like this:
```
; compilation finished in <duration>
[4/30/2023 17:02:03][INFO] Happy!
[4/30/2023 17:02:03][INFO] Started on port <port-number>
```

Remove the extra message line so that it won't get uploaded with the code.

## Development and Debugging

### Development Branch

Always work on a development branch, not the main branch:
```
git checkout -b <branch-name>
```

### Development

Make whatever changes seem appropriate on your development branch.
Use VSCode in order to test the changes.

Unlike the Alive project, compilation of the LSP server code is done
when it is loaded into the Alive extension during use (because it's Lisp).
There is no need to run a "watch" process to re-compile changes.

### Testing and Debugging

There are three possible testing configurations:
* Just test changes to Alive-lsp.
* Test integration of Alive and Alive-lisp, making changes to both.
* Run Alive-lsp in a REPL to debug crashes and set breakpoints.

#### Testing Alive-lsp

Test Alive-lsp "by itself" by using VSCode with the Alive extension installed.
In this configuration it is not possible to make changes to Alive.
Only changes within Alive-lsp code can be made.

This is basically described above in **Initial Steps**. In the Alive-lsp directory:
* Set the `alive.lsp.install.path` to the root of the Alive-lsp code.
* Bring up VSCode.
* Make changes to the Alive-lsp code.
* Use the **Developer: Reload Window** command to restart the extension.
* Test the Alive functionality supported by the server.

#### Testing Alive *and* Alive-lsp

It is often necessary to work on both Alive and Alive-lsp at the same time.
This configuration allows changes to be made to both codebases.

In the Alive-lsp directory:
* Set the `alive.lsp.install.path` to the root of the Alive-lsp code.

In the Alive directory:
* Bring up VSCode.
* Make changes to the Alive code.
* Start the debugger (e.g. by pressing the `F5` key)
  to generate a VSCode Extension Development Host window.
* If necessary change VSCode to edit the Alive-lsp project in the Host window.
* Make changes to the Alive-lsp code.
* Use the **Developer: Reload Window** command to restart the extension.
* Test the Alive functionality supported by the server.

#### Testing via REPL

This method may help in cases where the REPL is crashing inexplicably.

In the Alive-lsp directory:
* Set the `alive.lsp.install.path` to the root of the Alive-lsp code.
* Set the `alive.lsp.install.host` to `"127.0.0.1"`.
* Set the `alive.lsp.install.port` to some number (e.g. `8006`).
* Start the server in the SBCL REPL:
```
        sbcl --eval "(asdf:load-system :alive-lsp)" \
             --eval "(alive/server:start :port 8006)"
```
* Use the **Developer: Reload Window** command to restart the extension.
* Exercise the server via the Alive extension.
  Some of this (e.g. acquiring defined packages)
  may happen automatically at startup.
* The REPL will crash and complain
  `The current thread is not at the foreground`.
* Execute `(sb-thread:release-foreground)`.
  The REPL prompt may not be visible, just enter the command.
* Use the Common Lisp
  [interactive debugger](https://lispcookbook.github.io/cl-cookbook/debugging.html#the-interactive-debugger).

With this configuration it is possible to add `(break)` calls in the code to cause a break in the REPL.
Note that breaks in some areas will prevent the Alive extension from completing startup.
For example, during startup Alive will request all known packages, ASDF systems, and threads.
Breaking within the function `handle-list-pkgs` will halt processing of the packages request and
the Alive extension startup will be delayed and (maybe) eventually fail.
The next session addresses recovering from this sort of loss without resorting to `vim`.

#### Debugging

Debugging Lisp code, even with Alive, is different from most other languages.
Breakpoints can not be marked in the editor window and stepped through in VSCode.
The Common Lisp
[interactive debugger](https://lispcookbook.github.io/cl-cookbook/debugging.html#the-interactive-debugger)
is not directly available in VSCode either.
For the most part the developer is reliant on log statements which print to the Output panel.

#### Locking Up the Alive Extension

If the LSP server crashes badly in the wrong place it can prevent the Alive extension from starting.
This will in turn block some VSCode behavior, notably editing and saving files to fix the issue.
In this case:
* Disable the Alive extension and execute the **Developer: Reload Window** command.
* Edit and save files or use `git` to return them to a previous working state.
* Re-enable the extensions and execute the **Developer: Reload Window** command again.

Another way to fix this is to
* Remove the `alive.lsp.install.path` entry from `.vscode/settings.json`
  using an external editor such as `vim`.
* Execute the **Developer: Reload Window** command in VSCode.
This will enable VSCode to work again but will not remove the actual error.

## Submitting Pull Requests

After your changes are working properly it is time to submit a PR from your development branch.
[Submit the PR from your fork](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request-from-a-fork)
to the [Alive-lsp repository](https://github.com/nobody-famous/alive-lsp) on github.
