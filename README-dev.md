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
The Alive-lsp project provides the LSP server for the Alive extension.

Developing LSP servers is quite involved.
Start learning with the
[Language Server Extension Guide](https://code.visualstudio.com/api/language-extensions/language-server-extension-guide).

## Startup

### Install Tools

Install the following tools:
* git
* Steel Bank Common Lisp (SBCL)
* asdf (comes with SBCL)

You should be able to execute the following programs:
* `git` (or whatever tool you prefer)
* `sbcl` Common Lisp REPL

The rest of this document assumes the command-line use of `git`.
You may prefer a different tool (including VSCode).

#### Check ASDF Version

Some older SBCL distributions may have an old version of ASDF.
You must have version 3.2 or 3.3.
To check this start the SBCL REPL and execute:
```
*features*
(:quicklisp :asdf3.3 :asdf3.2 :asdf3.1 :asdf3 :asdf2 :asdf :os-unix
 :non-base-chars-exist-p :asdf-unicode :arena-allocator :x86-64 :gencgc :64-bit
 :ansi-cl :common-lisp :elf :ieee-floating-point :linux :little-endian
 :package-local-nicknames :sb-ldb :sb-package-locks :sb-thread :sb-unicode
 :sbcl :unix)
```
Note the symbols starting with `:asdf`.
In the above case, in `SBCL 2.3.2` as shown when the REPL starts,
the highest version of ASDF is `:asdf3.3`.
This is sufficient for work on Alive-lsp.

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
  of the Alive=lsp code into your own `github` account.
* [Clone your fork](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository)
  onto your development machine.

This is the same process [documented](https://github.com/nobody-famous/alive/blob/master/README-dev.md)
for Alive development.

### Inital Steps

Launch VSCode and open a workspace with the downloaded Alive-lsp directory.
Edit the `.vscode/settings.json` file to set `alive.lsp.install.path`
to point to the root directory where the Alive-lsp code is installed.
For example:
```
    "alive.lsp.install.path": "/home/<user>/work/lisp/alive-lsp",
```
This setting will cause the Alive extension installed in VSCode to find the
source code for the Alive-lsp project instead of downloading the latest release.
That way edits made to Alive-lsp code will be executed during testing.
_Avoid checking this change back into the repository_.

Show the `Output` view at the bottom of the screen (`<ctrl-K><ctrl-H>` or
the **Output: Focus on Output View** command) or just select the `Output` tab on the bottom panel.
On the right part of the title bar there is a dropdown to choose output from different threads.
Choose the `Alive LSP` thread which should be available when
the Alive extension is properly installed and the Alive-lsp server is running.
This choice seems to persist but if the `Output` view becomes mysteriously blank check this first.

The `Output` view should show the following at the end:
```
* [<timestamp>][INFO] Started on port <port-number>
```
This is output from Alive-lsp when it starts up.

Edit the file `src/server.lisp` to add an extra log message when the server starts:
```
(defun start (&key (port *default-port*))
    (if *server*
        (logger:msg logger:*error* "Server already running")

        (progn (logger:init *standard-output* logger:*info*)
               (logger:msg logger:*info* "Happy!")
               (setf *server* (make-instance 'lsp-server))
               (start-server *server* port))))
```
The added line logs "Happy!" at startup.

Use the **Developer: Reload Window** command to restart the extension, restarting the Alive-lsp server.
The `Output` view should now look like this:
```
[<timestamp> 17:02:03][INFO] Happy!
* [<timestamp> 17:02:03][INFO] Started on port <port-number>
```

Remove the extra `"Happy!"` log message line so that it won't get uploaded with the code.

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
Only changes within Alive-lsp code can be made in this configuration.

This is basically described above in **Initial Steps**. In the Alive-lsp directory:
* Bring up VSCode.
* Set the `alive.lsp.install.path` to the root of the Alive-lsp code.
* Set the `Output` view to show output from the `Alive LSP` thread.
* Make changes to the Alive-lsp code (like logging `"Happy!"` at startup).
* Use the **Developer: Reload Window** command to restart the extension.
* Test the Alive functionality supported by the server.
* Log data will be in the `Output` view.

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
* If necessary tell VSCode to open the Alive-lsp project in the Extension Development Host window.
* Make changes to the Alive-lsp code.
* Use the **Developer: Reload Window** command to restart the extension.
* Exercise the Alive functionality supported by the server.
* Log data from Alive-lsp will be in the `Output` view.

#### Testing via REPL

This method may help in cases where the REPL is crashing inexplicably.
Consider this the configuration of last resort.

In the Alive-lsp directory:
* Start the server in an SBCL REPL using the same port number:
```
        sbcl --eval "(asdf:load-system :alive-lsp)" \
             --eval "(alive/server:start :port 8006)"
```

In the Alive directory:
* Set the `alive.lsp.remote.host` to `"127.0.0.1"`.
* Set the `alive.lsp.remote.port` to some number (e.g. `8006`).
* Use the **Developer: Reload Window** command to restart the extension.
* Exercise the server via the Alive extension to invoke the broken server code.
  Some of this (e.g. acquiring defined packages)
  may happen automatically at startup.

In the Alive-lsp directory:
* The REPL will crash and complain
  `The current thread is not at the foreground`.
* Execute `(sb-thread:release-foreground)`.
  The REPL prompt may not be visible, just enter the command.
* Use the Common Lisp
  [interactive debugger](https://lispcookbook.github.io/cl-cookbook/debugging.html#the-interactive-debugger).

The REPL can be run in a shell window from the Alive-lsp directory or in the
`Terminal` view at the bottom of the VSCode workbench.
Executing in a shell window may be marginally less confusing.

With this configuration it is possible to add `(break)` calls in the code to cause a break in the REPL.
Note that breaks in some areas will prevent the Alive extension from completing startup.
For example, during startup Alive will request all known packages, ASDF systems, and threads.
Breaking within the function `handle-list-pkgs` will halt processing of the packages request and
the Alive extension startup will be delayed and (maybe) eventually fail.
The **Locking Up the Alive Extension** section below addresses recovering from this sort of problem
without resorting to `vim`.

#### Debugging

Debugging Lisp code, even with Alive, is different from most other languages.
Breakpoints can not be marked in the editor window and stepped through in VSCode.
The Common Lisp
[interactive debugger](https://lispcookbook.github.io/cl-cookbook/debugging.html#the-interactive-debugger)
is not directly available in VSCode.
For the most part the developer is reliant on log statements which print to the `Output` view.

#### Locking Up the Alive Extension

If the LSP server crashes badly in the wrong place it can prevent the Alive extension from starting.
This will in turn block some VSCode behavior, notably editing and saving files to fix the issue.
In this case:
* Disable the Alive extension and execute the **Developer: Reload Window** command.
* Edit and save files or use `git` to return them to a previous working state.
* Re-enable the Alive extension and execute the **Developer: Reload Window** command again.

Another way to fix this is to
* Remove the `alive.lsp.install.path` entry from `.vscode/settings.json`
  using an external editor such as `vim`.
* Execute the **Developer: Reload Window** command in VSCode.
This will enable VSCode to work again but will not remove the actual error.
Removing `alive.lsp.install.path` just allows Alive to go back to using the latest releaase of
the LSP server downloaded from the release server.

## Submitting Pull Requests

After your changes are working properly it is time to submit a PR from your development branch.
[Submit the PR from your fork](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request-from-a-fork)
to the [Alive-lsp repository](https://github.com/nobody-famous/alive-lsp) on github.
