# Positions --- cyclic cursor positions manager for Emacs

Positions is an Emacs extension that allows to store the current cursor position in a ring and to jump to the stored positions in a cyclic way.

# Instalation

## Install the extension

Positions is hosted in GitHub at
[github.com/clari3d/positions.el.git](https://github.com/clari3d/positions.el.git).

### Linux

Clone the repository somewhere:
$ cd /tmp
$ git clone https://github.com/clari3d/positions.el.git

Copy the file /tmp/positions.el/positions.el in the direcotry where emacs stores the extensions:

$ cp /tmp/positions.el/positions.el ~/.emacs.d/site-lisp

or copy it in the shared emacs extensions directory:

$ cp /tmp/positions.el/positions.el /usr/share/emacs/site-lisp


### Macos

Clone the repository somewhere:
$ cd /tmp
$ git clone https://github.com/clari3d/positions.el.git

Copy the file /tmp/positions.el/positions.el in the directory where emacs stores
the extensions:

$ cp /tmp/positions.el/positions.el ~/.emacs.d/

or copy it in the shared emacs extensions directory:

$ cp /tmp/positions.el/positions.el /Applications/Emacs.app/Contents/Resources/site-lisp


### Windows

To be continued...


## Register the extension

Once the file is installed, register it in the .emacs: Add (require 'positions) in your .emacs file.

# Usage

Positions is a global minor mode. In order to activate the extension type:

M-x positions-mode

The word 'Position' should be displayed in the loaded minor modes.

The key binding is:
- C+j    : switch to the next position, if any
- C-x j n: switch to the next position, if any
- C-x j p: switch to the previous position, if any
- C-x j r: remove all the stored positions
- C-x j s: store the current cursor position
- C-x j l: list all the positions


# Author

Positions is edited by Clari3D [www.clari3d.com](https://www.clari3d.com) thats also edits the famous Clari3D viewer for Linux, Macos, Windows and Web.
