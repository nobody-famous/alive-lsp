Alive Language Server
=====================

This is the language server for [Alive: The Average Lisp VSCode Environment](https://github.com/nobody-famous/alive).

## Running the server

For newcomers it could be difficult the start with Common LISP specially if the tech stack is not known yet. This instructions helps to have the server up and running as easy as possible. Steps are:

1. Install your LISP Implementation
2. Install a library/package manager
3. Clone this repository
4. Load the `alive-lsp` project and start the server

### Install your LISP Implementation

Here lets assume we install [sbcl](https://www.sbcl.org/)

```bash
# debian
apt-get install sbcl

# macos
brew install sbcl
```

### Install a library/package manager

Here lets assume we use [quicklisp](https://www.quicklisp.org/beta/)

```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
gpg --verify quicklisp.lisp.asc quicklisp.lisp
sbcl --load quicklisp.lisp
# ....
* (quicklisp-quickstart:install)
# it is also advisable to:
* (ql:add-to-init-file)
```

### Clone this repository

Previous step should have created a directory in your home directory: `~/quicklisp`, unless you decided to install it in another directory
```
(quicklisp-quickstart:install :path "~/.quicklisp")
```
just keep in mind the directory, lets called here `$QUICKLISP_HOME`. Lets clone this repository in there in order to make it easy to load it as project:

```bash
git clone git@github.com:nobody-famous/alive-lsp.git $QUICKLISP_HOME/local-projects/alive-lsp
```

### Load the `alive-lsp` project and start the server

```bash
sbcl
This is SBCL 2.2.11, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (ql:quickload "alive-lsp")
To load "alive-lsp":
  Load 1 ASDF system:
    alive-lsp
; Loading "alive-lsp"
.
("alive-lsp")
* (alive/server::start :port 8006)
#<SB-THREAD:THREAD "Alive LSP Server" RUNNING {70069B3BE3}>
```
Your language server works! You can stop it now (Ctrl + C) and proceed to properly configure you Alive Extension in vscode:

```json
{
    "alive.lsp.startCommand": [
        "sbcl",
        "--eval",
        "(require :asdf)",
        "--eval",
        "(asdf:load-system :alive-lsp)",
        "--eval",
        "(alive/server:start)"
    ]
}
```
note that `alive.lsp.startCommand` could also be the one used before in the REPL session, something like:

```json
{
    "alive.lsp.remote.port": "8006",
    "alive.lsp.startCommand": [
        "sbcl",
        "--noinform",
        "--eval",
        "ql:quickload \"alive-lsp\")",
        "--eval",
        "(alive/server::start :port 8006)"
    ]
}
```

just be sure to also configure the port if you specify one on the command for starting the server.


## Using the server outside of vscode

Take a look at thread [#31](https://github.com/nobody-famous/alive-lsp/issues/31)
