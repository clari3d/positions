# Position --- cyclic cursor position managed

Position is an Emacs extension that allows to store the current cursor position in a ring and to jump to the stored position in a cyclic way.

# Instalation

## Install the extension

Position is hosted in GitHub at https://github.com/clari3d/positions.el.git

### Linux

Clone the repository somewhere:
$ cd /tmp
$ git clone https://github.com/clari3d/positions.el.git

Copy the file /tmp/positions.el/position.el in the direcotry where emacs stores the extensions:

$ cp /tmp/positions.el/position.el ~/.emacs.d/site-lisp

or copy it in the shared emacs extensions

$ cp /tmp/positions.el/position.el /usr/share/emacs/site-lisp


### Macos

Clone the repository somewhere:
$ cd /tmp
$ git clone https://github.com/clari3d/positions.el.git

Copy the file /tmp/positions.el/position.el in the directory where emacs stores the extensions:

$ cp /tmp/positions.el/position.el ~/.emacs.d/

or copy it in the shared emacs extensions

$ cp /tmp/positions.el/position.el /Applications/Emacs.app/Contents/Resources/site-lisp


### Windows

To be continued...


## Register the extension

Once the file is installed, register it if the .emacs: Add (require 'position) in your .emacs file.

# Usage

Position is a global minor mode. In order to activate the extension type:

M-x position-mode

The word 'Position' should be displayed in the loaded minor modes.

The key binding is:
- C+j  : switch to the next position, if any
- C-xjn: switch to the next position, if any
- C-xjp: switch to the previous position, if any
- C-xjr: remove all the stored positions
- C-xjs: store the current cursor position
- C-xjl: list all the positions"
