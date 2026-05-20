;;; kirigami.el --- A unified method to fold and unfold text -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; Version: 1.0.5
;; URL: https://github.com/jamescherti/kirigami.el
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The kirigami package offers a unified interface for opening and
;; closing folds across a diverse set of major and minor modes in Emacs,
;; including `treesit-fold-mode', `outline-mode', `outline-minor-mode',
;; `outline-indent-minor-mode', `org-mode', `markdown-mode', `gfm-mode',
;; `embark-collect-mode', `vdiff-mode', `vdiff-3way-mode',
;; `hs-minor-mode' (hideshow), `hide-ifdef-mode', `vimish-fold-mode',
;; `TeX-fold-mode' (AUCTeX), `fold-this-mode', `origami-mode', `yafolding-mode',
;; `folding-mode', and `ts-fold-mode'.
;;
;; With Kirigami, folding key bindings only need to be configured once. After
;; that, the same keys work consistently across all supported major and minor
;; modes, providing a unified and predictable folding experience. The available
;; commands include:
;;
;; - `kirigami-open-fold': Open the fold at point.
;; - `kirigami-open-fold-rec': Open the fold at point recursively.
;; - `kirigami-close-fold': Close the fold at point.
;; - `kirigami-open-folds': Open all folds in the buffer.
;; - `kirigami-close-folds': Close all folds in the buffer.
;; - `kirigami-toggle-fold': Toggle the fold at point.
;;
;; (In addition to unified interface, the kirigami package enhances folding
;; behavior in outline-mode, outline-minor-mode, markdown-mode, and
;; org-mode. It ensures that deep folds open reliably and allows folds to be
;; closed even when the cursor is positioned inside the content.)
;;
;; Installation from MELPA
;; -----------------------
;; (use-package kirigami)

;;; Code:

;;; Variables

(defgroup kirigami nil
  "A unified method to fold and unfold text."
  :group 'kirigami
  :prefix "kirigami-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/kirigami.el"))

(defcustom kirigami-enhance-outline t
  "Enable enhancements for `outline' and `outline-minor-mode'.
This improves folding behavior in `outline-mode', `outline-minor-mode',
`org-mode', `markdown-mode', `gfm-mode', `outline-indent-minor-mode', and any
other mode built upon `outline-minor-mode':
- It ensures that deep folds open reliably
- It ensures that sibling folds at the same level are visible when a sub-fold is
  expanded
- It maintains `window-start' heading stability by automatically adjusting the
  scroll position to keep folded headings visible, preventing the context from
  disappearing when closing a fold that is partially scrolled off-screen.
- It allows folds to be closed even when the cursor is positioned inside the
  content.
- Additionally, it resolves upstream Emacs issues, such as
  https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-08/msg01128.html

It is recommended to keep this variable set to t unless there is a
specific reason to disable these enhancements."
  :type 'boolean
  :group 'kirigami)

(defcustom kirigami-preserve-visual-position nil
  "When non-nil, maintain the vertical position of the cursor during folding.
This prevents the window from jumping or re-centering when headings are expanded
or collapsed, keeping the relative distance between the cursor and the top of
the window constant."
  :type 'boolean
  :group 'kirigami)

(defvar kirigami-fold-list
  `(((outline-mode
      outline-minor-mode
      outline-indent-minor-mode
      org-mode
      markdown-mode
      gfm-mode)
     :open-all   kirigami--outline-open-all
     :close-all  kirigami--outline-close-all
     :toggle     kirigami--outline-toggle-children
     :open       kirigami--outline-open
     :open-rec   kirigami--outline-show-subtree
     :close      kirigami--outline-close)
    ((vdiff-mode)
     :open-all   vdiff-open-all-folds
     :close-all  vdiff-close-all-folds
     :toggle     ,(lambda () (call-interactively 'vdiff-toggle-fold))
     :open       ,(lambda () (call-interactively 'vdiff-open-fold))
     :open-rec   ,(lambda () (call-interactively 'vdiff-open-fold))
     :close      ,(lambda () (call-interactively 'vdiff-close-fold)))
    ((vdiff-3way-mode)
     :open-all   vdiff-open-all-folds
     :close-all  vdiff-close-all-folds
     :toggle     ,(lambda () (call-interactively 'vdiff-toggle-fold))
     :open       ,(lambda () (call-interactively 'vdiff-open-fold))
     :open-rec   ,(lambda () (call-interactively 'vdiff-open-fold))
     :close      ,(lambda () (call-interactively 'vdiff-close-fold)))
    ((hs-minor-mode)
     :open-all   hs-show-all
     :close-all  ,(lambda ()
                    ;; Restore the column because `hs-hide-all' may move
                    ;; point backward
                    ;; TODO: Emacs patch?
                    ;;
                    (condition-case err
                        (kirigami--call-preserve-column 'hs-hide-all)
                      (error
                       (unless (string-match-p "^Already at end of element"
                                               (error-message-string err))
                         ;; If it is a different error, re-throw it
                         (signal (car err) (cdr err))))))
     :toggle     ,(lambda ()
                    ;; Restore the column because `hs-toggle-hiding' may move
                    ;; point backward
                    ;; TODO: Emacs patch?
                    (kirigami--call-preserve-column 'hs-toggle-hiding))
     :open      ,(lambda ()
                   ;; Restore the column because `hs-show-block' may move point
                   ;; backward
                   ;; TODO: Emacs patch?
                   (kirigami--call-preserve-column 'hs-show-block))
     :open-rec  nil
     :close     ,(lambda ()
                   ;; Restore the column because `hs-hide-block' may move point
                   ;; backward
                   ;; TODO: Emacs patch?
                   (kirigami--call-preserve-column 'hs-hide-block)))
    ((hide-ifdef-mode)
     :open-all   show-ifdefs
     :close-all  hide-ifdefs
     :toggle     nil
     :open       show-ifdef-block
     :open-rec   nil
     :close      hide-ifdef-block)
    ((ts-fold-mode)
     :open-all    ts-fold-open-all
     :close-all   ts-fold-close-all
     :toggle      ts-fold-toggle
     :open        ts-fold-open
     :open-rec    ts-fold-open-recursively
     :close       ts-fold-close)
    ((treesit-fold-mode)
     :open-all   ,(lambda ()
                    (when (fboundp 'treesit-fold-open-all)
                      (treesit-fold-open-all)))
     :close-all  ,(lambda ()
                    (when (fboundp 'treesit-fold-close-all)
                      (treesit-fold-close-all)))
     :toggle     ,(lambda ()
                    (when (fboundp 'treesit-fold-toggle)
                      (save-excursion
                        (kirigami--normalize-point)
                        (treesit-fold-toggle))))
     :open       ,(lambda ()
                    (when (fboundp 'treesit-fold-open)
                      (save-excursion
                        (kirigami--normalize-point)
                        (treesit-fold-open))))
     :open-rec   ,(lambda ()
                    (when (fboundp 'treesit-fold-open-recursively)
                      (save-excursion
                        (kirigami--normalize-point)
                        (treesit-fold-open-recursively))))
     :close      ,(lambda ()
                    (when (fboundp 'treesit-fold-close)
                      (save-excursion
                        (kirigami--normalize-point)
                        (treesit-fold-close)))))
    ((folding-mode)
     :open-all   folding-open-buffer
     :close-all  ,(lambda()
                    (save-excursion
                      (when (fboundp 'folding-whole-buffer)
                        (folding-whole-buffer))))
     :toggle     folding-toggle-show-hide
     :open       folding-show-current-entry
     :open-rec   folding-show-current-subtree
     :close      folding-hide-current-entry)
    ((fold-this-mode)
     :toggle     fold-this-unfold-at-point
     :open-all   fold-this-unfold-at-point
     :open       fold-this-unfold-at-point
     :open-rec   fold-this-unfold-at-point
     :close-all  ,(lambda() (when (and (use-region-p)
                                       (fboundp 'fold-this))
                              (call-interactively 'fold-this)))
     :close      ,(lambda() (when (and (use-region-p)
                                       (fboundp 'fold-this))
                              (call-interactively 'fold-this))))
    ((origami-mode)
     :open-all   ,(lambda () (when (fboundp 'origami-open-all-nodes)
                               (origami-open-all-nodes (current-buffer))))
     :close-all  ,(lambda () (when (fboundp 'origami-close-all-nodes)
                               (origami-close-all-nodes (current-buffer))))
     :toggle     ,(lambda () (when (fboundp 'origami-toggle-node)
                               (origami-toggle-node (current-buffer) (point))))
     :open       ,(lambda () (when (fboundp 'origami-open-node)
                               (origami-open-node (current-buffer) (point))))
     :open-rec   ,(lambda () (when (fboundp 'origami-open-node-recursively)
                               (origami-open-node-recursively (current-buffer)
                                                              (point))))
     :close      ,(lambda () (when (fboundp 'origami-close-node)
                               (origami-close-node (current-buffer) (point)))))
    ((vimish-fold-mode)
     :open-all   vimish-fold-unfold-all
     :close-all  vimish-fold-refold-all
     :toggle     vimish-fold-toggle
     :open       vimish-fold-unfold
     :open-rec   vimish-fold-unfold
     :close      vimish-fold-refold)
    ((TeX-fold-mode)
     :open-all  TeX-fold-clearout-buffer
     :close-all TeX-fold-buffer
     :toggle    TeX-fold-dwim  ; Or a lambda calling TeX-fold-item
     :open      TeX-fold-clearout-item
     :open-rec  TeX-fold-clearout-item
     :close     ,(lambda ()
                   (when (and (fboundp 'TeX-active-mark)
                              (fboundp 'TeX-fold-item)
                              (fboundp 'TeX-fold-region)
                              (fboundp 'TeX-fold-comment))
                     (cond
                      ((TeX-active-mark) (TeX-fold-region (mark)
                                                          (point)))
                      ((TeX-fold-item 'macro))
                      ((TeX-fold-item 'math))
                      ((TeX-fold-item 'env))
                      ((TeX-fold-comment))))))
    ((yafolding-mode)
     :open-all   yafolding-show-all
     :close-all ,(lambda () (save-excursion
                              (goto-char (point-min))
                              (call-interactively 'yafolding-hide-all)))
     :toggle     yafolding-toggle-element
     :open       yafolding-show-element
     :open-rec   yafolding-show-element
     :close      yafolding-hide-element))
  "Actions to be performed for various folding operations.

The value should be a list of fold handlers, where a fold handler has
the format: ((MODES) PROPERTIES)

MODES acts as a predicate, containing the symbols of all major or minor modes
for which the handler should match. For example the following would match for
either `outline-minor-mode' or `org-mode', even though the former is a minor
mode and the latter is a major.
  \\='((outline-minor-mode org-mode) ...)

PROPERTIES specifies possible folding actions and the functions to be
applied in the event of a match on one (or more) of the MODES; the
supported properties are:
  - `:open-all': Open all folds.
  - `:close-all': Close all folds.
  - `:toggle': Toggle the display of the fold at point.
  - `:open': Open the fold at point.
  - `:open-rec': Open the fold at point recursively.
  - `:close': Close the fold at point.

Each value must be a function.  A value of nil will cause the action
to be ignored for that respective handler.  For example:

  `((org-mode)
     :close-all  nil
     :open       ,(lambda ()
                    (show-entry)
                    (show-children))
     :close      `hide-subtree')

would ignore `:close-all' actions and invoke the provided functions on
`:open' or `:close'.")

;; (defcustom kirigami-verbose nil
;;   "Enable displaying verbose messages."
;;   :type 'boolean
;;   :group 'kirigami)

(defvar kirigami-pre-action-predicates nil
  "Hook dispatched before the execution of buffer folding procedures.

Each function member is invoked with a single argument, ACTION, denoting the
specific transformation. The ACTION argument is constrained to the following
keywords: :open-all, :close-all, :toggle, :open, :open-rec, or :close.

IMPORTANT: This hook acts as a gatekeeper. Each function MUST return a non-nil
value to authorize the operation. If any member returns nil, the execution
sequence is immediately terminated and the action is denied.")

(defvar kirigami-post-action-functions nil
  "Hook dispatched after the execution of buffer folding procedures.

Each function member is invoked with a single argument, ACTION, denoting the
specific transformation that was completed. The ACTION argument is constrained
to the following keywords: :open-all, :close-all, :toggle, :open, :open-rec, or
:close.

The return values of functions in this hook are ignored.")

(defvar kirigami-gc-threshold (* 128 1024 1024)
  "GC threshold for temporary increase.")

(defvar kirigami-gc-percentage 0.3
  "GC percentage for temporary increase.")

;;; Internal functions

(defvar kirigami-inhibit-redisplay t
  "Non-nil means inhibit UI redisplay during bulk fold operations.
When expanding or collapsing multiple folds simultaneously, intermediate
redisplay cycles consume unnecessary CPU overhead. Setting this variable
to non-nil defers screen updates until the complete execution finishes.")

(defvar kirigami-inhibit-message t
  "Non-nil means suppress Echo Area messages during bulk fold operations.
Some underlying folding functions call the `message' function to log structural
updates. When this variable is non-nil, those calls are inhibited to prevent
echo area redraws and unwanted writes to the '*Messages*' buffer, reducing I/O
overhead during large-scale changes.")

(defmacro kirigami--optimize (&rest body)
  "Evaluate BODY with temporarily inhibit redisplay and increased GC limits."
  (declare (indent 0) (debug t))
  `(let ((gc-cons-threshold (max gc-cons-threshold kirigami-gc-threshold))
         (gc-cons-percentage (max gc-cons-percentage kirigami-gc-percentage))
         (inhibit-redisplay kirigami-inhibit-redisplay)
         (inhibit-message kirigami-inhibit-message))
     ,@body))

(defun kirigami--normalize-point ()
  "Adjust point to a valid position on the current line.
Move point back by one character if it is at the end of a non-empty line.
Move point forward to the first non-whitespace character if it is located
within the leading whitespace."
  (when (and (eolp) (not (bolp)))
    (backward-char 1))

  ;; Adjust point only if it resides inside the leading whitespace.
  ;; In `treesit-fold-mode', for example, this avoids overriding specific
  ;; sub-node targeting later on the line.
  ;; For example, if point is on the '[' in 'val = [1, 2, 3]', an unconditional
  ;; jump back to indentation shifts targeting to 'val' and folds the entire
  ;; assignment instead of the array.
  (when (< (current-column) (current-indentation))
    (back-to-indentation)))

(defun kirigami--call-preserve-column (fn)
  "Call FN and restore point to the original column when possible."
  (let ((column (current-column)))
    (when (fboundp fn)
      (funcall fn)
      (let ((line-size (- (line-end-position)
                          (line-beginning-position))))
        (move-to-column (if (< column line-size)
                            column
                          line-size))))))

(defun kirigami--mode-p (modes)
  "Check if any symbol in MODES matches the current buffer's modes."
  (let ((tail modes)
        (found nil))
    (while (and tail (not found))
      (let ((mode (car tail)))
        (if (or (eq major-mode mode)
                (and (boundp mode) (symbol-value mode)))
            (setq found t)
          (setq tail (cdr tail)))))
    found))

(defun kirigami-fold--action-get-func (list action &optional ignore-errors)
  "Return the function to execute ACTION in a major/minor mode in LIST."
  (if (null list)
      (unless ignore-errors
        (user-error
         "Enable one of the following modes for folding to work: %s"
         (mapconcat #'symbol-name (mapcar #'caar kirigami-fold-list) ", ")))
    (let* ((modes (caar list)))
      (if (kirigami--mode-p modes)
          (let* ((actions (cdar list))
                 (fn (plist-get actions action)))
            (when fn
              fn))
        (kirigami-fold--action-get-func (cdr list) action ignore-errors)))))

(defun kirigami-fold-action (list action &optional ignore-errors)
  "Perform fold ACTION for each matching major or minor mode in LIST.

The procedure executes `kirigami-pre-action-predicates` as a gatekeeper.
If authorized, it executes the transformation. The post-action hook
triggers only if the operation returns a non-nil value and completes
without unhandled errors.

Returns the result of the folding function, or nil if the operation
was blocked or failed."
  (save-match-data
    (when (run-hook-with-args-until-failure 'kirigami-pre-action-predicates action)
      (let ((fn (kirigami-fold--action-get-func list action ignore-errors)))
        (when fn
          (let ((result (with-demoted-errors "Error: %S" (funcall fn))))
            (run-hook-with-args 'kirigami-post-action-functions action)
            result))))))

(defun kirigami--outline-invisible-p (&optional pos)
  "Non-nil if the character after POS has outline invisible property.
If POS is nil, use point instead."
  (let ((position (or pos (point))))
    (cond
     ((and (derived-mode-p 'org-mode)
           (fboundp 'org-fold-folded-p))
      (org-fold-folded-p position))

     ((and (derived-mode-p 'org-mode)
           (fboundp 'org-invisible-p))
      (org-invisible-p position t))

     ((fboundp 'outline-invisible-p)
      (outline-invisible-p position))

     (t
      (invisible-p position)))))

(defun kirigami--outline-ensure-window-start-heading-visible ()
  "Adjust `window-start' in all windows of the current buffer to fix issues.

This function iterates through every window displaying the current buffer. It
checks if the text at the top of the window (`window-start') is currently hidden
inside a folded outline subtree. If so, it resets the window start to the
position of the parent heading to make it visible.

This fixes an issue in `outline-mode', `outline-minor-mode', `org-mode',
`markdown-mode', `outline-indent-minor-mode'... where folding a subtree that is
partially scrolled off-screen causes the heading to disappear."
  (interactive)
  (when (and kirigami-enhance-outline
             (fboundp 'outline-on-heading-p)
             (fboundp 'outline-invisible-p)
             (fboundp 'outline-back-to-heading)
             (fboundp 'outline-up-heading)
             (or (derived-mode-p 'outline-mode)
                 (bound-and-true-p outline-minor-mode)))
    ;; We are using save-match-data because inside outline-back-to-heading,
    ;; Emacs performs a regular expression search (re-search-backward) to find
    ;; the heading line. In Emacs Lisp, match data (the results of the last
    ;; regex search) is global state.
    (save-match-data
      (dolist (current-window (get-buffer-window-list (current-buffer) nil t))
        (when (window-live-p current-window)
          (let ((heading-point
                 (save-excursion
                   (progn
                     ;; Explicitly pass current-window to
                     ;; window-start
                     (goto-char (window-start current-window))
                     ;; Use native invisible-p to handle org-mode
                     ;; and outline reliably
                     (when (kirigami--outline-invisible-p (point))
                       (let ((result nil))
                         (condition-case nil
                             (progn
                               (outline-back-to-heading)
                               ;; If the heading itself is also
                               ;; hidden, climb the hierarchy
                               ;; until we find a visible parent
                               ;; heading.
                               (while
                                   (and (> (point) (point-min))
                                        (kirigami--outline-invisible-p (point)))
                                 (outline-up-heading 1 t))
                               (setq result (point)))
                           (error
                            nil))
                         result))))))
            ;; Ensure folded headings remain visible after hiding subtrees.
            ;; Fixes a bug in outline and Evil where headings could scroll
            ;; out of view when their subtrees were folded.
            ;; TODO Send a patch to Emacs and/or Evil
            (when (and heading-point
                       ;; Explicitly pass current-window to window-start
                       (< heading-point (window-start current-window)))
              (set-window-start current-window heading-point t))))))))

;;; Functions: `outline' enhancements (`kirigami-enhance-outline')

(defun kirigami--outline-heading-folded-p ()
  "Return non-nil if the body following the current heading is folded."
  (if (fboundp 'outline-back-to-heading)
      (save-excursion
        (save-match-data
          (catch 'done
            (progn
              (condition-case nil
                  (outline-back-to-heading)
                (error
                 (throw 'done t)))

              ;; Is it invisible?
              (kirigami--outline-invisible-p (line-end-position))))))
    (error "Required outline functions are undefined")))

(defun kirigami--outline-legacy-show-entry ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible.
This is the Emacs version of `outline-show-entry'."
  (interactive)
  (if (and (fboundp 'outline-back-to-heading)
           (fboundp 'outline-flag-region)
           (fboundp 'outline-next-preface))
      (save-excursion
        (outline-back-to-heading t)
        (outline-flag-region (1- (point))
                             (progn
                               (outline-next-preface)
                               (if (= 1 (- (point-max) (point)))
                                   (point-max)
                                 (point)))
                             nil))
    (error "Required outline functions are undefined")))

(defun kirigami--outline-legacy-hide-subtree (&optional event)
  "Hide everything after this heading at deeper levels.
If non-nil, EVENT should be a mouse event.
This is the Emacs version of `outline-hide-subtree'."
  (interactive (list last-nonmenu-event))
  (if (fboundp 'outline-flag-subtree)
      (save-excursion
        (when (mouse-event-p event)
          (mouse-set-point event))
        (outline-flag-subtree t))
    (error "Required outline functions are undefined")))

(defun kirigami--outline-toggle-children ()
  "Show or hide the current `outline' subtree depending on its current state."
  (cond
   ((and (bound-and-true-p outline-indent-minor-mode)
         (fboundp 'outline-indent-toggle-fold))
    (call-interactively 'outline-indent-toggle-fold))

   ((fboundp 'outline-toggle-children)
    (unwind-protect
        (outline-toggle-children)
      (kirigami--outline-ensure-window-start-heading-visible)))))

(defun kirigami--outline-show-entry-and-parents ()
  "Reveal the current entry and its parent hierarchy.
This command ensures that the current entry, all of its ancestor
headings, and their immediate sibling headings are visible.

The function iteratively unfolds the children and body of the target
entry until it is fully revealed. If invoked when the point is inside
a completely hidden subtree, it manages the visibility state to avoid
leaving the buffer in an inconsistent layout. This guarantees a safe
and predictable visual expansion."
  (interactive)
  (when (and (fboundp 'outline-back-to-heading)
             (fboundp 'outline-show-children)
             (fboundp 'outline-show-entry))
    ;; Wrap in `save-match-data' because outline functions use regular
    ;; expressions. Without this, calling `outline-show-entry-and-parents'
    ;; programmatically would clobber the caller's match data, leading to
    ;; subtle, hard-to-trace bugs.
    (save-match-data
      ;; Repeatedly expand the outline structure at point from the outside
      ;; in until the target text is fully visible.
      ;;
      ;; Think of this block as manually opening nested folds:
      ;; - It checks whether the heading at point is folded.
      ;; - If it is folded, it moves backward to that parent heading.
      ;; - It opens the heading to reveal its text and subheadings.
      ;; - It repeats this process layer by layer down to the target.
      (let (heading-point
            prior-heading-point)
        (while (condition-case nil
                   (save-excursion
                     ;; Navigate backward to the nearest visible heading
                     (outline-back-to-heading)
                     (setq heading-point (point))
                     ;; Break the loop if we stop making progress,
                     ;; preventing infinite recursion
                     (if (eq heading-point prior-heading-point)
                         ;; Break out of the loop
                         nil
                       (setq prior-heading-point heading-point)
                       ;; Check if the heading is folded by inspecting the
                       ;; end of the line
                       (when (invisible-p (if (fboundp 'pos-eol)
                                              (pos-eol)
                                            (line-end-position)))
                         ;; Ignore errors to guarantee the target entry is
                         ;; still revealed via `outline-show-entry' even
                         ;; if a buggy third-party `outline-level'
                         ;; function fails during child expansion.
                         (ignore-errors (outline-show-children))

                         ;; Show the body directly following this heading
                         (outline-show-entry)

                         ;; Return t to continue drilling down to the next
                         ;; layer of the outline hierarchy
                         t)))
                 (outline-before-first-heading
                  nil)))))))

(defun kirigami--outline-on-heading-p ()
  "Return t if point is on a visible outline heading."
  (save-excursion
    (vertical-motion 0)
    (cond
     ((and (derived-mode-p 'org-mode)
           (fboundp 'org-at-heading-p))
      (org-at-heading-p))

     ((and (derived-mode-p 'org-mode)
           (fboundp 'org-on-heading-p))
      (org-on-heading-p))

     ((fboundp 'outline-on-heading-p)
      (outline-on-heading-p)))))

(defun kirigami--outline-show-entry (&rest _)
  "Ensure the current heading and body are fully visible.
Repeatedly reveal children and body until the entry is no longer folded.

- Goes back to the heading.
- Runs `outline-show-children' (ensures immediate children are made visible).
- Runs the legacy `outline-show-entry' function to reveal the body.

After the loop, calls `kirigami--outline-legacy-show-entry' once more to ensure
the entry is fully visible."
  (interactive)
  (if (and (fboundp 'outline-on-heading-p)
           (fboundp 'outline-invisible-p)
           (fboundp 'outline-back-to-heading)
           (fboundp 'outline-show-children)
           (fboundp 'outline-up-heading)
           (fboundp 'outline-level)
           (fboundp 'outline-show-entry))
      (save-match-data
        (let* (;; Evaluate if the point is on an outline heading and whether
               ;; that heading is currently invisible.
               ;;
               ;; If the cursor is on an invisible subheading, kirigami assumes
               ;; the user did not intentionally target it for expansion.
               ;; Consequently, kirigami will close that specific subheading
               ;; after the reveal process.
               ;;
               ;; A heading fold is left open by kirigami only if the user
               ;; triggered the action while the cursor was on a visible
               ;; heading.
               (on-heading (kirigami--outline-on-heading-p))
               (on-invisible-subheading (and on-heading
                                             (outline-on-heading-p t)
                                             (kirigami--outline-invisible-p))))
          (unwind-protect
              (progn
                (kirigami--outline-show-entry-and-parents)

                ;; If the header was previously hidden, hide the subtree to
                ;; collapse it. Otherwise, leave the fold open. This allows the
                ;; user to decide whether to expand the content under the
                ;; cursor.
                (when on-invisible-subheading
                  (kirigami--outline-legacy-hide-subtree)))
            ;; Workaround for an outline-mode issue: when jumping via imenu or
            ;; search, sibling headings above the current one and at the same
            ;; level often remain hidden. This ensures all sub-items at the
            ;; current level are revealed, preventing the 'isolated item'
            ;; effect.
            (save-excursion
              (catch 'done
                (condition-case nil
                    (outline-back-to-heading t)
                  (error (throw 'done t)))

                (let ((current-level (funcall outline-level)))
                  ;; Only attempt to climb if we are deeper than level 1
                  (while (and (numberp current-level) (> current-level 1))
                    (let ((prev-point (point)))
                      (condition-case nil
                          ;; invisible-ok is t
                          (outline-up-heading 1 t)
                        (error (throw 'done t)))

                      ;; If point didn't move or level didn't decrease, we've
                      ;; hit a wall or a sibling jump
                      (let ((new-level (funcall outline-level)))
                        (when (or (= prev-point (point))
                                  (>= new-level current-level))
                          (throw 'done t))
                        (setq current-level new-level))

                      (condition-case nil
                          (progn
                            (outline-show-children)
                            (outline-show-entry))
                        (error (throw 'done t)))))))))))
    (error "Required outline functions are undefined")))

(defun kirigami--empty-subtree-p ()
  "Return non-nil if the current outline heading has no content or subtrees."
  (when (and (fboundp 'outline-on-heading-p)
             (fboundp 'outline-end-of-heading)
             (fboundp 'outline-end-of-subtree)
             (fboundp 'outline-back-to-heading))
    (save-match-data
      (save-excursion
        (outline-back-to-heading)
        (let* ((start (progn
                        (outline-end-of-heading)
                        (when (eolp)
                          (forward-char -1))
                        (point)))
               (end (progn
                      (outline-end-of-subtree)
                      (when (eolp)
                        (forward-char -1))
                      (point))))
          (= start end))))))

(defun kirigami--outline-hide-subtree ()
  "Close the current heading subtree.

If the current heading is folded or contains no content, locate the previous
higher-level heading and close its subtree instead.

Keep the cursor at its original position (even if that position becomes hidden
inside the fold)."
  (if (and (fboundp 'outline-back-to-heading)
           (fboundp 'outline-end-of-subtree)
           (fboundp 'outline-up-heading)
           (fboundp 'outline-on-heading-p))
      (progn
        (save-excursion
          (catch 'quit-function
            ;; Move to the current heading; error if before the first heading
            (condition-case nil
                (outline-back-to-heading)
              (error
               (throw 'quit-function t)))

            (let ((heading-point (point)))
              ;; If the current heading is folded, or if it contains no content,
              ;; move to the previous higher-level heading.
              (catch 'done
                (when (or (kirigami--outline-heading-folded-p)  ; Folded?
                          ;; Or fold without any content
                          (kirigami--empty-subtree-p))
                  ;; Try to move up to previous higher-level heading
                  (condition-case nil
                      (outline-up-heading 1 t)
                    (error
                     (throw 'done t)))

                  (setq heading-point (point))))

              (when (outline-on-heading-p)
                (kirigami--outline-legacy-hide-subtree))

              ;; Ensure folded headings remain visible after hiding subtrees.
              ;; Fixes a bug in outline and Evil where headings could scroll out
              ;; of view when their subtrees were folded.
              ;; TODO Send a patch to Emacs and/or Evil
              (let ((window (selected-window)))
                (when (and (window-live-p window)
                           (eq (current-buffer) (window-buffer window))
                           heading-point
                           (< heading-point (window-start)))
                  (set-window-start (selected-window) heading-point t)))))))
    (error "Required outline functions are undefined")))

(defmacro kirigami--save-window-start (&rest body)
  "Preserve and restore `window-start' relative to the lines above the cursor.

This macro saves the first visible line in the selected window. After BODY
executes, the window is restored so that the same lines remain visible above the
cursor, maintaining the relative vertical position of the cursor within the
window.

To also restore the mark, this macro can be combined with
`save-mark-and-excursion'. For preservation of horizontal and vertical scroll,
consider using the `kirigami--save-window-scroll' macro.

This macro is appropriate when it is necessary to maintain the visual layout of
the buffer, especially if BODY may scroll the window or otherwise move the
cursor."
  (declare (indent 0) (debug t))
  (let ((window (make-symbol "window"))
        (window-buffer (make-symbol "window-buffer"))
        (lines-before-cursor (make-symbol "lines-before-cursor"))
        (start-pos (make-symbol "start-pos")))
    `(let* ((,window (selected-window))
            (,window-buffer (window-buffer ,window))
            (,lines-before-cursor
             (when (and (window-live-p ,window)
                        (eq (current-buffer) ,window-buffer))
               (count-screen-lines
                (save-excursion
                  (goto-char (window-start ,window))
                  (vertical-motion 0)
                  (point))
                (save-excursion
                  (vertical-motion 0)
                  (point))
                nil
                ,window))))
       (unwind-protect
           (progn ,@body)
         (when (and ,lines-before-cursor
                    (window-live-p ,window)
                    (buffer-live-p ,window-buffer)
                    (eq ,window-buffer (window-buffer ,window)))
           (with-selected-window ,window
             (let ((,start-pos (save-excursion
                                 (vertical-motion 0)
                                 (vertical-motion (- ,lines-before-cursor)
                                                  ,window)
                                 (vertical-motion 0)
                                 (point))))
               (set-window-start ,window
                                 ,start-pos
                                 ;; No force
                                 t))))))))

(defmacro kirigami--save-window-scroll (&rest body)
  "Execute BODY while preserving the horizontal and vertical scroll."
  (declare (indent 0) (debug t))
  (let ((window (make-symbol "window"))
        (window-buffer (make-symbol "window-buffer"))
        (hscroll (make-symbol "hscroll"))
        (vscroll (make-symbol "vscroll"))
        (should-restore (make-symbol "should-restore")))
    `(let* ((,window (selected-window))
            (,window-buffer (window-buffer ,window))
            ;; Check conditions and capture scroll BEFORE body runs
            (,should-restore (and (window-live-p ,window)
                                  (eq (current-buffer)
                                      (window-buffer ,window))))
            (,hscroll (when ,should-restore
                        (window-hscroll ,window)))
            (,vscroll (when ,should-restore
                        (window-vscroll ,window t))))
       (unwind-protect
           ;; Execute body exactly ONCE
           (progn ,@body)
         ;; Restore only if conditions were originally met
         (when (and ,should-restore
                    (window-live-p ,window)
                    (buffer-live-p ,window-buffer)
                    (eq ,window-buffer (window-buffer ,window)))
           (set-window-vscroll ,window ,vscroll t)
           (set-window-hscroll ,window ,hscroll))))))

(defun kirigami--outline-close ()
  "Close the `outline' fold at point."
  (cond
   ((and (bound-and-true-p outline-indent-minor-mode)
         (fboundp 'outline-indent-close-fold))
    (call-interactively 'outline-indent-close-fold))

   ((and kirigami-enhance-outline
         (fboundp 'kirigami--outline-hide-subtree))
    (if (and (fboundp 'outline-back-to-heading)
             (save-excursion
               (ignore-errors
                 (outline-back-to-heading)
                 (< (point) (window-start)))))
        (kirigami--save-window-start
          (kirigami--save-window-scroll
            (kirigami--outline-hide-subtree)))
      (kirigami--outline-hide-subtree)))

   ((fboundp 'outline-hide-subtree)
    (unwind-protect
        (outline-hide-subtree)
      (kirigami--outline-ensure-window-start-heading-visible)))

   ((fboundp 'hide-subtree)
    (unwind-protect
        (hide-subtree)
      (kirigami--outline-ensure-window-start-heading-visible)))))

(defun kirigami--outline-show-subtree ()
  "Open `outline' fold at point recursively."
  (cond
   ((and (bound-and-true-p outline-indent-minor-mode)
         (fboundp 'outline-indent-open-fold-rec))
    (call-interactively 'outline-indent-open-fold-rec))

   ((fboundp 'outline-show-subtree)
    (unwind-protect
        (outline-show-subtree)
      (kirigami--outline-ensure-window-start-heading-visible)))

   ((fboundp 'show-subtree)
    (unwind-protect
        (show-subtree)
      (kirigami--outline-ensure-window-start-heading-visible)))))

(defun kirigami--outline-open ()
  "Open the `outline' fold at point."
  (cond
   ((and (bound-and-true-p outline-indent-minor-mode)
         (fboundp 'outline-indent-open-fold))
    (call-interactively 'outline-indent-open-fold))

   ((and kirigami-enhance-outline
         (fboundp 'kirigami--outline-show-entry))
    (unwind-protect
        (kirigami--outline-show-entry)
      (kirigami--outline-ensure-window-start-heading-visible)))

   ((and (fboundp 'outline-show-entry)
         (fboundp 'outline-show-children))
    (unwind-protect
        (ignore-errors
          (outline-show-entry)
          (outline-show-children))
      (kirigami--outline-ensure-window-start-heading-visible)))

   ((and (fboundp 'show-entry)
         (fboundp 'show-children))
    (unwind-protect
        (ignore-errors
          (show-entry)
          (show-children))
      (kirigami--outline-ensure-window-start-heading-visible)))))

(defun kirigami--outline-open-all ()
  "Show all `outline' folds and ensure the first heading remains visible."
  (cond
   ((and (bound-and-true-p outline-indent-minor-mode)
         (fboundp 'outline-indent-open-folds))
    (call-interactively 'outline-indent-open-folds))

   ((fboundp 'outline-show-all)
    (unwind-protect
        (outline-show-all)
      (kirigami--outline-ensure-window-start-heading-visible)))

   ((fboundp 'show-all)
    (unwind-protect
        (show-all)
      (kirigami--outline-ensure-window-start-heading-visible)))))

(defun kirigami--outline-close-all ()
  "Close all `outline' folds and ensure the first heading remains visible."
  (cond
   ((and (bound-and-true-p outline-indent-minor-mode)
         (fboundp 'outline-indent-close-folds))
    (call-interactively 'outline-indent-close-folds))

   ((or (fboundp 'hide-sublevels)
        (fboundp 'outline-hide-sublevels))
    (if kirigami-enhance-outline
        (when (and (fboundp 'outline-on-heading-p)
                   (fboundp 'outline-next-visible-heading)
                   (fboundp 'outline-next-heading))
          ;; When `kirigami-enhance-outline' is non-nil
          (unwind-protect
              ;; TODO send a patch to Emacs
              ;; In modes like markdown-mode, it is common for a document to
              ;; start with a deeply nested heading (e.g., ###) without any
              ;; parent # or ## headings present. The standard
              ;; outline-hide-sublevels command often fails to collapse the
              ;; buffer correctly in these cases if it defaults to a level that
              ;; does not exist or treats level 0 incorrectly.
              (save-excursion
                (goto-char (point-min))

                ;; Handle preamble (if the file doesn't start with a heading)
                (unless (outline-on-heading-p)
                  (outline-next-heading))

                ;; Collapse every top-level heading found
                (let ((done nil))
                  (while (and (not done)
                              (not (eobp))
                              (outline-on-heading-p))
                    (let ((current-point (point)))
                      ;; Hide the subtree
                      (condition-case nil
                          (kirigami--outline-hide-subtree)
                        (error
                         nil))

                      ;; Try to move to the next visible heading.
                      (progn
                        (outline-next-visible-heading 1)
                        ;; SAFETY CHECK: If point did not move forward, we
                        ;; must stop. This catches cases where the function
                        ;; returns successfully but fails to advance (e.g., at
                        ;; the last heading in some modes).
                        (when (<= (point) current-point)
                          (setq done t)))))))
            (kirigami--outline-ensure-window-start-heading-visible)))

      ;; When `kirigami-enhance-outline' is nil
      (cond
       ((fboundp 'outline-hide-sublevels)
        (outline-hide-sublevels 1))
       ((fboundp 'hide-sublevels)
        (hide-sublevels 1)))))))

;;; Functions: open/close folds

(defun kirigami-close-folds-except-current ()
  "Close all folds except the current one."
  (kirigami--optimize
    (let ((point (point)))
      (kirigami-close-folds)
      (goto-char point)
      (kirigami-open-fold))))

;;;###autoload
(defun kirigami-open-fold ()
  "Open fold at point.
See also `kirigami-close-fold'."
  (interactive)
  (kirigami--optimize
    (kirigami-fold-action kirigami-fold-list :open)

    ;; TODO Restore visual position in certain cases
    ;; (if kirigami-preserve-visual-position
    ;;     (kirigami--save-window-start
    ;;       (kirigami--save-window-scroll
    ;;         (kirigami-fold-action kirigami-fold-list :open)))
    ;;   (kirigami-fold-action kirigami-fold-list :open))
    ))

;;;###autoload
(defun kirigami-open-fold-rec ()
  "Open fold at point recursively.
See also `kirigami-open-fold' and `kirigami-close-fold'."
  (interactive)
  (kirigami--optimize
    (if kirigami-preserve-visual-position
        (kirigami--save-window-start
          (kirigami--save-window-scroll
            (kirigami-fold-action kirigami-fold-list :open-rec)))
      (kirigami-fold-action kirigami-fold-list :open-rec))))

;;;###autoload
(defun kirigami-open-folds ()
  "Open all folds.
See also `kirigami-close-folds'."
  (interactive)
  (kirigami--optimize
    (if kirigami-preserve-visual-position
        (kirigami--save-window-start
          (kirigami--save-window-scroll
            (kirigami-fold-action kirigami-fold-list :open-all)))
      (kirigami-fold-action kirigami-fold-list :open-all))))

;;;###autoload
(defun kirigami-close-fold ()
  "Close fold at point.
See also `kirigami-open-fold'."
  (interactive)
  (kirigami--optimize
    (kirigami-fold-action kirigami-fold-list :close)))

;;;###autoload
(defun kirigami-toggle-fold ()
  "Open or close a fold under point.
See also `kirigami-open-fold' and `kirigami-close-fold'."
  (interactive)
  (kirigami--optimize
    (if kirigami-preserve-visual-position
        (kirigami--save-window-start
          (kirigami--save-window-scroll
            (kirigami-fold-action kirigami-fold-list :toggle)))
      (kirigami-fold-action kirigami-fold-list :toggle))))

;;;###autoload
(defun kirigami-close-folds ()
  "Close all folds."
  (interactive)
  (kirigami--optimize
    (if kirigami-preserve-visual-position
        (kirigami--save-window-start
          (kirigami--save-window-scroll
            (kirigami-fold-action kirigami-fold-list :close-all)))
      (kirigami-fold-action kirigami-fold-list :close-all))))

(provide 'kirigami)

;;; kirigami.el ends here
