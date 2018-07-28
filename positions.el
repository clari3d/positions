;; position.el --- cyclic cursor position managed
;; Copytight (c) 2017 Andéor, SAS
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
;;
;;
;; # Todo
;;
;; - define a minor mode - ok
;; - allow to work in text mode - ok
;; - handle deleted buffers
;;
 
;; version of the extension
(defconst positions/version "0.1")
 
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
 
;; add or remove the current psition where the text cursor is in or from the
;; list of positions: if the current position is not in the list, it is added and
;; if it is in the list, it is removed
;; @return void
(defun positions-set ()
"add/remove the current cursor position in/from the position list"
(letrec ((org      (point))
          (end      (progn (end-of-line) (point)))
          (pos      (progn (beginning-of-line) (point)))
          (position (cons (current-buffer) (line-number-at-pos))))
   ;; if the position is in the list
   (if (member position positions-locations)
       ;; it is removed
        (progn
          (message "delete position %s:%d" (car position) (cdr position))
          (letrec (;; get the index of the position in the list
                   (index   (position-index position positions-locations))
                   ;; get the corresponding overlay
                   (overlay (nth index positions-overlays)))
            ;; delete the overlay
            (delete-overlay overlay)
            ;; remove the overlay from the list of overlays
            (setq positions-overlays  (delete overlay  positions-overlays)))
          ;; remove the position from the list
          (setq positions-locations (delete position positions-locations)))
     ;; otherwise, it is added
     (progn
       (message "add position %s:%d" (car position) (cdr position))
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
       (setq positions-locations (cons position positions-locations))))))
 
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
 
(defvar positions-mode nil)
 
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
                                 (positions-list))))))
    ;; if the mode is currently on
    (if positions-mode
        ;; bind all the keys of the extension
        (dolist (binding bindings)
          (global-set-key (car binding) (cadr binding)))
      ;; if the mode is off
      (progn
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
C-xjl: list all the positions"
  :init-value positions-mode
  :lighter    " Positions"
  :global     t
  :group      'position
  (positions-mode-toggle))
 
;; provide the extension
(provide 'positions)
