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
Start learning with the [Language Server Extension Guide](https://code.visualstudio.com/api/language-extensions/language-server-extension-guide).

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
Edit the file `src/server.lisp` to add an extra message on startup:
```
(defun start (&key (port *default-port*))
    (if *server*
        (logger:msg logger:*error* "Server already running")

        (progn (logger:init *standard-output* logger:*info*)
               (logger:msg logger:*info* "Happy!")
               (setf *server* (make-instance 'lsp-server))
               (start-server *server* port))))
```
The added line shows "Happy!".

Use the **Developer: Reload Window** command to restart the extension,
restarting the Alive-lsp server.
The Output panel at the bottom of the screen should include the additional message:
```
; compilation finished in <duration>
[4/30/2023 17:02:03][INFO] Happy!
[4/30/2023 17:02:03][INFO] Started on port <port-number>
```

Remove the extra message line so it won't get uploaded with the code.

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

There are two testing configurations that will work.
One of them just tests changes to Alive-lsp,
the other is useful when integrating changes to Alive and Alive-lsp.

Debugging Lisp code, even with Alive, is different from most other languages.
Breakpoints and stepping through code are not available for Common Lisp.
Nor will the Common Lisp stack trace mechanisms be accessible since
the LSP server is running in a thread and inaccessible to the user.

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
* Use the **Developer: Reload Window** command to restart the extension.
* Start the debugger (e.g. by pressing the `F5` key)
  to generate a VSCode Extension Development Host window.
* If necessary open the Alive-lsp project.
* Make changes to the Alive-lsp code.
* Use the **Developer: Reload Window** command to restart the extension.
* Test the Alive functionality supported by the server.

#### Debugging

Debugging Lisp code, even with Alive, is different from most other languages.
Breakpoints and stepping through code are not available for Common Lisp.
Nor will the Common Lisp stack trace mechanisms be accessible since
the LSP server is running in a thread and inaccessible to the developer.

The forces the developer to depend upon tracing with log statements.
Add them where necessary and use the Output panel to see what they say.

## Submitting Pull Requests

After your changes are working properly it is time to submit a PR from your development branch.
[Submit the PR from your fork](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request-from-a-fork)
to the [Alive-lsp repository](https://github.com/nobody-famous/alive-lsp) on github.
