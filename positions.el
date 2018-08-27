;; position.el --- cyclic cursor position managed
;; Copytight (c) 2017 to 2018 Andéor, SAS
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;; # Positions mode
;;
;; This minor mode allow to store positions in some buffer and to switch back
;; to the stored positions.
;;
;;
;; # Installation
;; Copy this file in the list extensions of the emacs installation. This can be
;; any of thes elocations:
;; - $HOME/.emacs.d/list
;; - /usr/share/emacs/site-lisp
;;
;; Add
;; (require 'positions)
;; to the .emacs file in your home directory.
;;
;;
;; # Activation
;;
;; Position is activated with:
;; M-x position-mode RET
;;
;;
;; # Key binding
;; - C+j  : switch to the next position, if any
;; - C-xjn: switch to the next position, if any
;; - C-xjp: switch to the previous position, if any
;; - C-xjr: remove all the stored positions
;; - C-xjs: store the current cursor position
;; - C-xjl: list all the positions"
;; - C-xjt: store all the position in a file
;; - C-xjo: resstore all the position from a file
;;
;;
;; # Todo
;;
;; - define a minor mode - ok
;; - allow to work in text mode - ok
;; - handle deleted buffers - ok
;;


;; v a r i a b l e s

;; version of the extension
(defconst positions/version "0.2")

;; boolean variable that reflects the position status
(defvar positions-mode nil)

;; description of the positions mode
(defgroup positions nil
  "Store and retrieve positions in the buffers."
  :tag   "Positions"
  :group 'convenience
  :group 'fill)

;; color to use in order to hilight the stored position in text mode                                        ;
(defcustom positions-color "#ddf"
  "Color used to draw the stored lines in terminal mode."
  :group 'positions
  :tag   "Positions color"
  :type  'color)

;; force text mode boolean
(defcustom positions-text-mode nil
  "Force text mode."
  :group 'positions
  :tag   "Positions text mode"
  :type  'boolean)

;; list of stored positions - a position is a pair of a buffer and a line number
(setq positions-locations '())

;; list of the position overlays
(setq positions-overlays  '())

;; index of the current position in the list of positions
(setq positions-current   0)


;; u t i l i t i e s

;; retrieve the index of an element in a list
;; @param object: the object to retrieve
;; @param list: the list where to search for the element
;; @return integer or nil: the index or nill if not found
(defun position-index (object list)
  "return the index of object in list"
  (if (not list)
      nil
    (if (equal object (car list))
        0
      (let ((position (position-index object (cdr list))))
        (if position
            (+ position 1)
          nil)))))

;; delete a stored position by its index
;; @param index: the position index in the list
;; @return void
(defun positions-delete (index)
  (let (;; get the location and the overlay
        (location (nth index positions-locations))
        (overlay  (nth index positions-overlays)))
    (when location
      ;; message
      (message "delete position %s:%d" (car location) (cdr location))
      ;; delete the overlay
      (delete-overlay overlay)
      ;; remove the overlay from the list of overlays
      (setq positions-overlays  (delete overlay positions-overlays))
      ;; remove the position from the list
      (setq positions-locations (delete location positions-locations)))))

;; add or remove the current psition where the text cursor is in or from the
;; list of positions: if the current position is not in the list, it is added and
;; if it is in the list, it is removed
;; @return void
(defun positions-set ()
  "add/remove the current cursor position in/from the position list"
  (letrec ((org      (point))
           (end      (progn (end-of-line) (point)))
           (pos      (progn (beginning-of-line) (point)))
           (location (cons (current-buffer) (line-number-at-pos))))
    ;; if the position is in the list
    (if (member location positions-locations)
        ;; it is removed
        (let (;; get the index of the position in the list
              (index (position-index location positions-locations)))
          (positions-delete index))
      ;; otherwise, it is added
      (progn
        (message "add position %s:%d" (car location) (cdr location))
        (let (;; create the overlay
              (overlay (make-overlay pos (if (display-images-p) pos end))))
          ;; in graphic mode
          (if (and (not positions-text-mode) (display-images-p))
              ;; add the indicator in the fringe
              (overlay-put overlay
                           'before-string
                           (propertize "A"
                                       'display '(left-fringe
                                                  filled-rectangle)))
            ;; otherwise change the text background
            (overlay-put overlay 'face `(:background ,positions-color)))
          ;; add the overlay in the list of overlay
          (setq positions-overlays  (cons overlay positions-overlays)))
        ;; add the position in the list of positions
        (setq positions-locations (cons location positions-locations))))))

;; jump to the current position (in positions-current) with name as action name
;; @param name: the name of the action,
;; @return void
(defun positions-jump (name)
  "jump to the current position"
  (let (;; get the number of elements in the positions list
        (len (length positions-locations)))
    ;; if there is some positions
    (when (> len 0)
      (let (;; get the index of the current position
            (position (nth positions-current positions-locations)))
        (message "jump to the %s position %s:%d"
                 name (car position) (cdr position))
        ;; activete the buffer in the position
        (switch-to-buffer (car position))
        ;; goto the line of the position
        (goto-line (cdr position))))))


;; write all the stored positions in a file
;; @param filename: the file name to use
;; @return void
(defun positions-write (filename)
  "save all the positions in a filename"
  (message "save all the positions-locations in %s" filename)
  ;; delete the file
  (delete-file filename)
  ;; make it empty
  (write-region "(\n" "" filename)
  ;; for all the positions stored in the list of positions
  (dolist (position positions-locations)
    ;; append the current position to the file
    (append-to-file (format "  (\"%s\" . %d)\n" (car position) (cdr position))
                    nil filename))
  (append-to-file ")\n" nil filename))

;; read and append all the stored positions from a file
;; @param filename: the file name to use
;; @return void
(defun positions-read (filename)
  "read all the positions from a filename"
  ;; message
  (message "read all the positions-locations from %s" filename)
  ;; remove all the existing positions
  (positions-reset)

  (letrec ((input   (find-file-noselect filename))
           (content (read input)))
    (message "%s" content)))

;; return the index of a buffer in the stored locations
;; buffer-name: the buffer to retrieve
;; @return integer or nil: the index or nil
(defun positions-index-of-buffer (buffer-name)
  (if buffer-name
      (catch 'ret
        (let ((index 0))
          (dolist (location positions-locations)
            (if (string= buffer-name (buffer-file-name (car location)))
                (throw 'ret index)
              (setq index (+ index 1))))
          nil))
    nil))


;; k e y   b i n d e d   f u n c t i o n s

;; jump to the next position
;; @return void
(defun positions-next ()
  "jump to the next position in the position list"
  (let (;; get the number of elements in the positions list
        (len (length positions-locations)))
    ;; if there is some elements
    (when (> len 0)
      ;; increment the current position recycling to zero
      (setq positions-current (+ positions-current 1))
      (when (>= positions-current len)
        (setq positions-current 0))
      ;; jump to the new current position
      (positions-jump "next"))))

;; jump to the previous position
;; @return void
(defun positions-previous ()
  "jump to the previous position in the position list"
  (let (;; get the number of psition in the positions list
        (len (length positions-locations)))
    ;; if there is some positions
    (when (> len 0)
      ;; change the current position, recycligin to zero
      (setq positions-current (- positions-current 1))
      (when (< positions-current 0)
        (setq positions-current (- len 1)))
      ;; jump to the new current position
      (positions-jump "previous"))))

;; remove all the stored positions
;; @return void
(defun positions-reset ()
  "delete all the positions"
  (message "reset all the positions-locations");
  ;; delete all the overlay
  (dolist (overlay positions-overlays)
    (delete-overlay overlay))
  ;; reset the list and indexes
  (setq positions-overlays  '())
  (setq positions-current 0)
  (setq positions-locations '()))

;; list all the stored positions
;; @return void
(defun positions-list ()
  "list all the positions"
  (message "list all the positions");
  (let (;; intialize the multi-lines message
        (msg ""))
    ;; for all the positions stored in the list of positions
    (dolist (position positions-locations)
      ;; append the current position to the multi-line message
      (setq msg (format "%sposition %s:%d\n"
                        msg
                        (car position) (cdr position))))
    ;; display the message
    (message (format "%sThere is %d positions"
                     msg (length positions-locations)))))

;; store all the stored positions in a file
;; @param filename: the file name to use
;; @return void
(defun positions-store ()
  "save all the positions in a asked filename "
  (let (;; get the filename
        (filename (read-file-name "file name where to write the positions: "
                                  (expand-file-name "~")
                                  (expand-file-name "~/positions.dat"))))
    ;; message
    (message "save all the positions-locations in %s" filename)
    ;; write the positions
    (positions-write filename)))

;; store all the stored positions in a file
;; @param filename: the file name to use
;; @return void
(defun positions-restore ()
  "load all the positions from a asked filename "
  (let (;; get the filename
        (filename (read-file-name "file name where to read the positions")))
    ;; if the file is readable
    (if (file-readable-p filename)
        ;; read the positions
        (positions-read filename)
      ;; on error, message
      (message "Error: file %s does not exist" filename))))


;; h o o k s

;; buffer kill hook
;; @return void
(defun positions-kill-buffer-hook ()
  (let (;; get the index of the buffer to delete
        (index (positions-index-of-buffer buffer-file-name)))
    (when index
      ;; delete the position
      (positions-delete index))))


;; m i n o r   m o d e

;; toggle the positions mode
;; @return void
(defun positions-mode-toggle ()
  (let (;; list of key bindings
        (bindings `(("\C-j", (lambda ()
                               (interactive)
                               (positions-next)))
                    ("\C-xjn", (lambda ()
                                 (interactive)
                                 (positions-next)))
                    ("\C-xjp", (lambda ()
                                 (interactive)
                                 (positions-previous)))
                    ("\C-xjr", (lambda ()
                                 (interactive)
                                 (positions-reset)))
                    ("\C-xjs", (lambda ()
                                 (interactive)
                                 (positions-set)))
                    ("\C-xjl", (lambda ()
                                 (interactive)
                                 (positions-list)))
                    ("\C-xjt", (lambda ()
                                 (interactive)
                                 (positions-store)))
                    ("\C-xjo", (lambda ()
                                 (interactive)
                                 (positions-restore))))))
    ;; if the mode is currently on
    (if positions-mode
        (progn
          ;; add the hook on buffer kill
          (add-hook 'kill-buffer-hook 'positions-kill-buffer-hook)
          ;; bind all the keys of the extension
          (dolist (binding bindings)
            (global-set-key (car binding) (cadr binding))))
      ;; if the mode is off
      (progn
        ;; remove the hook on buffer kill
        ;; remove all the key bindings
        (dolist (binding bindings)
          (global-set-key (car binding) nil))
        ;; remove all the positions
        (positions-reset)))))

;; autoload position mode
;;;###autoload
(define-minor-mode positions-mode
  "Toggle Position mode.
Interactively with no argument, this command toggles the mode.
Once set, the following keys are defined:
C+j  : switch to the next position, if any
C-xjn: switch to the next position, if any
C-xjp: switch to the previous position, if any
C-xjr: remove all the stored positions
C-xjs: store the current cursor position
C-xjl: list all the positions
C-xjt: store all the position in a file
C-xjo: resstore all the position from a file"
  :init-value positions-mode
  :lighter    " Positions"
  :global     t
  :group      'position
  (positions-mode-toggle))

;; provide the extension
(provide 'positions)
