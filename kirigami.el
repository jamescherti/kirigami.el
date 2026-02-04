;;; kirigami.el --- A unified method to fold and unfold text -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.0
;; URL: https://github.com/jamescherti/kirigami.el
;; Keywords: convenience
;; Package-Requires: ((emacs "26.3"))
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

;; The kirigami package offers a unified interface for text folding across a
;; diverse set of major and minor modes in Emacs, including `outline-mode',
;; `outline-minor-mode', `outline-indent-mode', `org-mode', `markdown-mode',
;; `vdiff-mode', `vdiff-3way-mode', `hs-minor-mode', `hide-ifdef-mode',
;; `origami-mode', `yafolding-mode', `folding-mode', and `treesit-fold-mode'.
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
;; (use-package kirigami
;;   :ensure t)

;;; Code:

;; Kirigami offers a unified interface for text folding across a diverse set of
;; major and minor modes in Emacs, including `outline-mode',
;; `outline-minor-mode', `outline-indent-mode', `org-mode', `markdown-mode',
;; `vdiff-mode', `vdiff-3way-mode', `hs-minor-mode', `hide-ifdef-mode',
;; `origami-mode', `yafolding-mode', `folding-mode', and `treesit-fold-mode'.
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
;; This eliminates the need to memorize or configure separate key bindings for
;; each mode, providing a truly unified and efficient workflow. Users can fold,
;; unfold, and navigate sections immediately, regardless of the file type or
;; mode, saving time and reducing errors.
;;
;; (In addition to unified interface, the kirigami package enhances folding
;; behavior in `outline-mode', `outline-minor-mode', and `org-mode'. It ensures
;; that deep folds open reliably and allows folds to be closed even when the
;; cursor is positioned inside the content.)

;;; Variables

(defgroup kirigami nil
  "A unified method to fold and unfold text."
  :group 'kirigami
  :prefix "kirigami-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/kirigami.el"))

(defcustom kirigami-enhance-outline t
  "Enable enhancements for `outline' and `outline-minor-mode' mode.

This enhances folding behavior in `outline-mode', `outline-minor-mode', and
`org-mode':
- It ensures that deep folds open reliably
- It allows folds to be closed even when the cursor is positioned inside the
  content.
- Additionally, it resolves upstream Emacs issues, such as
  https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-08/msg01128.html

It is recommended to keep this variable set to t unless there is a
specific reason to disable these enhancements."
  :type 'boolean
  :group 'kirigami)

(defcustom kirigami-preserve-visual-position t
  "When non-nil, maintain the cursor's vertical position during folding.
This prevents the window from jumping or re-centering when headings are expanded
or collapsed, keeping the relative distance between the cursor and the top of
the window constant."
  :type 'boolean
  :group 'kirigami)

(defvar kirigami-fold-list
  `(((outline-indent-minor-mode)
     :open-all   ,(lambda ()
                    (when (fboundp 'outline-indent-open-folds)
                      (call-interactively 'outline-indent-open-folds)))
     :close-all  ,(lambda ()
                    (when (fboundp 'outline-indent-close-folds)
                      (call-interactively 'outline-indent-close-folds)))
     :toggle     ,(lambda ()
                    (when (fboundp 'outline-indent-toggle-fold)
                      (call-interactively 'outline-indent-toggle-fold)))
     :open       ,(lambda ()
                    (when (fboundp 'outline-indent-open-fold)
                      (call-interactively 'outline-indent-open-fold)))
     :open-rec   ,(lambda ()
                    (when (fboundp 'outline-indent-open-fold-rec)
                      (call-interactively 'outline-indent-open-fold-rec)))
     :close      ,(lambda ()
                    (when (fboundp 'outline-indent-close-fold)
                      (call-interactively 'outline-indent-close-fold))))
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
    ((treesit-fold-mode)
     :open-all   treesit-fold-open-all
     :close-all  treesit-fold-close-all
     :toggle     treesit-fold-toggle
     :open       treesit-fold-open
     :open-rec   treesit-fold-open-recursively
     :close      treesit-fold-close)
    ((hs-minor-mode)
     :open-all   hs-show-all
     :close-all  ,(lambda ()
                    ;; Restore the column because `hs-hide-all' may move
                    ;; point backward
                    ;; TODO: Emacs patch?
                    (kirigami--call-preserve-column 'hs-hide-all))
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
    ((yafolding-mode)
     :open-all   yafolding-show-all
     :close-all ,(lambda () (save-excursion
                              (goto-char (point-min))
                              (call-interactively 'yafolding-hide-all)))
     :toggle     yafolding-toggle-element
     :open       yafolding-show-element
     :open-rec   yafolding-show-element
     :close      yafolding-hide-element)
    ((outline-mode
      outline-minor-mode
      org-mode
      markdown-mode
      gfm-mode)
     :open-all   show-all
     :close-all  kirigami--outline-close-all
     :toggle     outline-toggle-children
     :open       ,(lambda ()
                    (cond
                     ((and (bound-and-true-p outline-indent-minor-mode)
                           (fboundp 'outline-indent-open-fold))
                      (outline-indent-open-fold))

                     ((and kirigami-enhance-outline
                           (fboundp 'kirigami--outline-show-entry))
                      (kirigami--outline-show-entry))

                     (t
                      (when (and (fboundp 'show-entry)
                                 (fboundp 'show-children))
                        (ignore-errors
                          (show-entry)
                          (show-children))))))
     :open-rec show-subtree
     :close ,(lambda ()
               (cond
                ((and kirigami-enhance-outline
                      (fboundp 'kirigami--outline-hide-subtree))
                 (kirigami--outline-hide-subtree))

                (t
                 (when (fboundp 'hide-subtree)
                   (hide-subtree))))))
    ;; TODO support vimish-fold-mode
    ;; ((vimish-fold-mode)
    ;;  :open-all   ,(lambda () (call-interactively 'vimish-fold-unfold-all))
    ;;  :close-all  ,(lambda () (call-interactively 'vimish-fold-refold-all))
    ;;  :toggle     ,(lambda () (call-interactively 'vimish-fold-toggle))
    ;;  :open       ,(lambda () (call-interactively 'vimish-fold-unfold))
    ;;  :open-rec   ,(lambda () (call-interactively 'vimish-fold-unfold))
    ;;  :close      ,(lambda () (call-interactively 'vimish-fold-refold)))
    )
  "Actions to be performed for various folding operations.

The value should be a list of fold handlers, were a fold handler has
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

;;; Internal functions

;; (defun kirigami--message (&rest args)
;;   "Display a message with the same ARGS arguments as `message'."
;;   (apply #'message (concat "[kirigami] " (car args)) (cdr args)))

;; (defmacro kirigami--verbose-message (&rest args)
;;   "Display a verbose message with the same ARGS arguments as `message'."
;;   (declare (indent 0) (debug t))
;;   `(progn
;;      (when kirigami-verbose
;;        (kirigami--message
;;         (concat ,(car args)) ,@(cdr args)))))

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
  (unless (eq modes '())
    (let ((mode (car modes)))
      (or (eq major-mode mode)
          (and (boundp mode) (symbol-value mode))
          (kirigami--mode-p (cdr modes))))))

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
  "Perform fold ACTION for each matching major or minor mode in LIST."
  (let ((fn (kirigami-fold--action-get-func list action ignore-errors)))
    (when fn
      (with-demoted-errors "Error: %S" (funcall fn)))))

;;; Functions: `outline' enhancements (`kirigami-enhance-outline')

(defun kirigami--outline-heading-folded-p ()
  "Return non-nil if the body following the current heading is folded."
  (if (fboundp 'outline-back-to-heading)
      (save-excursion
        (outline-back-to-heading)
        (end-of-line)
        ;; Is it invisible?
        (cond ((and (bound-and-true-p outline-minor-mode)
                    (fboundp 'outline-invisible-p))
               (funcall 'outline-invisible-p (point)))

              ((and (derived-mode-p 'org-mode)
                    (fboundp 'org-invisible-p))
               (funcall 'org-invisible-p (point)))

              (t (invisible-p (point)))))
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
  (if (and (fboundp 'outline-flag-subtree))
      (save-excursion
        (when (mouse-event-p event)
          (mouse-set-point event))
        (outline-flag-subtree t))
    (error "Required outline functions are undefined")))

(defun kirigami--outline-close-all ()
  "Close all folds and ensure the first heading remains visible."
  (when (fboundp 'hide-sublevels)
    (hide-sublevels 1)

    (when (and (fboundp 'outline-on-heading-p)
               (fboundp 'outline-back-to-heading))
      (let ((heading-point (save-excursion
                             (condition-case nil
                                 (progn
                                   (goto-char (window-start))
                                   (beginning-of-visual-line)
                                   (if (outline-on-heading-p)
                                       (point)
                                     (outline-back-to-heading t)))
                               (error
                                nil)))))
        ;; Ensure folded headings remain visible after hiding subtrees. Fixes a
        ;; bug in outline and Evil where headings could scroll out of view when
        ;; their subtrees were folded. TODO Send a patch to Emacs and/or Evil
        (when (and heading-point
                   (< heading-point (window-start)))
          (set-window-start (selected-window) heading-point t))))))

(defun kirigami--outline-show-entry (&rest _)
  "Ensure the current heading and body are fully visible.
Repeatedly reveal children and body until the entry is no longer folded.

- Goes back to the heading.
- Runs `outline-show-children' (ensures immediate children are made visible).
- Runs the legacy `outline-show-entry' function to reveal the body.

After the loop, calls `kirigami--outline-legacy-show-entry' once more to ensure
the entry is fully visible."
  (interactive)
  (when (not (and (fboundp 'outline-on-heading-p)
                  (fboundp 'outline-invisible-p)
                  (fboundp 'outline-back-to-heading)
                  (fboundp 'outline-show-children)
                  (fboundp 'outline-up-heading)
                  (fboundp 'outline-show-children)))
    (error "Required outline functions are undefined"))

  (when (and (fboundp 'outline-on-heading-p)
             (fboundp 'outline-invisible-p)
             (fboundp 'outline-back-to-heading)
             (fboundp 'outline-show-children)
             (fboundp 'outline-up-heading)
             (fboundp 'outline-show-children))
    ;; Workaround for an outline-mode limitation: when jumping via imenu or
    ;; search, sibling headings above the current one and at the same level
    ;; often remain hidden. This ensures all sub-items at the current level are
    ;; revealed, preventing the 'isolated item' effect.
    (save-excursion
      ;; Climbing as long as a parent heading exists
      (catch 'done
        (while (not (bobp))
          (condition-case nil
              (progn (outline-up-heading 1 t))
            (error
             (throw 'done t)))

          (outline-show-children))))

    ;; Repeatedly reveal children and body until the entry is no longer folded
    (condition-case nil
        (let ((on-invisible-heading (when (outline-on-heading-p t)
                                      (outline-invisible-p)))
              (on-visible-heading (save-excursion
                                    (beginning-of-line)
                                    (outline-on-heading-p))))
          ;; TODO select when (use-region-p)

          ;; Repeatedly reveal children and body until the entry is no longer
          ;; folded
          (progn
            (while (kirigami--outline-heading-folded-p)
              (save-excursion
                (outline-back-to-heading)
                (outline-show-children)
                (kirigami--outline-legacy-show-entry))))

          ;; If the header was previously hidden, hide the subtree to collapse
          ;; it. Otherwise, leave the fold open. This allows the user to
          ;; decide whether to expand the content under the cursor.
          (when (and on-invisible-heading (not on-visible-heading))
            (kirigami--outline-legacy-hide-subtree)))
      ;; `outline-back-to-heading' issue
      (outline-before-first-heading
       nil))))

(defun kirigami--outline-hide-subtree ()
  "Close the current heading's subtree in a robust manner.

If the current heading is folded or contains no content, move to the previous
heading with a higher level and close its subtree.

Otherwise, close the current subtree. Ensures that folded headings remain
visible in the window after hiding."
  (if (and (fboundp 'outline-back-to-heading)
           (fboundp 'outline-end-of-subtree)
           (fboundp 'outline-up-heading)
           (fboundp 'outline-on-heading-p))
      (save-excursion
        ;; Move to the current heading; error if before the first heading
        (outline-back-to-heading)

        (let ((heading-point (point)))
          ;; If the current heading is folded, or if it contains no content,
          ;; move to the previous higher-level heading.
          (when (or (kirigami--outline-heading-folded-p)  ; Folded?
                    ;; Or fold without any content
                    (let ((start (save-excursion (end-of-line)
                                                 (point)))
                          (end (save-excursion (outline-end-of-subtree)
                                               (point))))
                      (= start end)))
            ;; Try to move up to previous higher-level heading
            (outline-up-heading 1 t)
            (setq heading-point (point)))

          (when (outline-on-heading-p)
            (kirigami--outline-legacy-hide-subtree))

          ;; Ensure folded headings remain visible after hiding subtrees. Fixes
          ;; a bug in outline and Evil where headings could scroll out of view
          ;; when their subtrees were folded.
          ;; TODO Send a patch to Emacs and/or Evil
          (when (and heading-point
                     (< heading-point (window-start)))
            (set-window-start (selected-window) heading-point t))))
    (error "Required outline functions are undefined")))

(defmacro kirigami--save-window-start (&rest body)
  "Preserve and restore `window-start' relative to the lines above the cursor.

This macro saves the first visible line in the selected window. After BODY
executes, the window is restored so that the same lines remain visible above the
cursor, maintaining the relative vertical position of the cursor within the
window.

To also restore the mark, this macro can be combined with
`save-mark-and-excursion'. For preservation of horizontal scroll only (hscroll),
consider using the `kirigami--save-window-hscroll' macro.

Example:
  (kirigami--save-window-hscroll
        (kirigami--save-window-start
          (save-mark-and-excursion
            ;;; Add code here
            t))

This macro is appropriate when it is necessary to maintain the visual layout of
the buffer, particularly if BODY may scroll the window or otherwise move the
cursor."
  (declare (indent 0) (debug t))
  (let ((window (make-symbol "window"))
        (buffer (make-symbol "buffer"))
        (restore-window-settings-p (make-symbol "restore-window-settings-p"))
        (lines-before-cursor (make-symbol "lines-before-cursor")))
    `(let* ((,window (selected-window))
            (,buffer (window-buffer ,window))
            (,restore-window-settings-p (eq ,buffer
                                            (current-buffer)))
            (,lines-before-cursor nil))
       (when ,restore-window-settings-p
         (with-current-buffer ,buffer
           (setq ,lines-before-cursor (count-screen-lines
                                       (save-excursion
                                         (goto-char (window-start))
                                         (beginning-of-visual-line)
                                         (point))
                                       (save-excursion
                                         (beginning-of-visual-line)
                                         (point))
                                       nil
                                       ,window))))
       (unwind-protect
           (progn ,@body)
         (when ,restore-window-settings-p
           (set-window-start ,window
                             ;; Dotimes and (line-move-visual -1) is more accurate
                             ;; than (line-move-visual N).
                             (save-excursion
                               (dotimes (_ ,lines-before-cursor)
                                 (condition-case nil
                                     (let ((line-move-visual t)
                                           (line-move-ignore-invisible t))
                                       (line-move -1))
                                   (error nil)))

                               (beginning-of-visual-line)
                               (point))))))))

(defmacro kirigami--save-window-hscroll (&rest body)
  "Execute BODY while preserving the horizontal scroll of the selected window.

This macro saves the current `window-hscroll` of the selected window.
After BODY executes, the horizontal scroll is restored exactly, leaving
the vertical position and window start unchanged.

Use this macro when you only need to maintain horizontal alignment,
without restoring the lines above the cursor."
  (declare (indent 0) (debug t))
  (let ((window (make-symbol "window"))
        (hscroll (make-symbol "window-hscroll")))
    `(let ((,window (selected-window))
           (,hscroll (window-hscroll)))
       (unwind-protect
           (progn ,@body)
         (set-window-hscroll ,window ,hscroll)))))

;;; Functions: open/close folds

;;;###autoload
(defun kirigami-open-fold ()
  "Open fold at point.
See also `kirigami-close-fold'."
  (interactive)
  (kirigami-fold-action kirigami-fold-list :open))

;;;###autoload
(defun kirigami-open-fold-rec ()
  "Open fold at point recursively.
See also `kirigami-open-fold' and `kirigami-close-fold'."
  (interactive)
  (kirigami-fold-action kirigami-fold-list :open-rec))

;;;###autoload
(defun kirigami-open-folds ()
  "Open all folds.
See also `kirigami-close-folds'."
  (interactive)
  (if kirigami-preserve-visual-position
      (kirigami--save-window-hscroll
        (kirigami--save-window-start
          (save-excursion
            (kirigami-fold-action kirigami-fold-list :open-all))))
    (kirigami-fold-action kirigami-fold-list :open-all)))

;;;###autoload
(defun kirigami-close-fold ()
  "Close fold at point.
See also `kirigami-open-fold'."
  (interactive)
  (kirigami-fold-action kirigami-fold-list :close))

;;;###autoload
(defun kirigami-toggle-fold ()
  "Open or close a fold under point.
See also `kirigami-open-fold' and `kirigami-close-fold'."
  (interactive)
  (kirigami-fold-action kirigami-fold-list :toggle))

;;;###autoload
(defun kirigami-close-folds ()
  "Close all folds."
  (interactive)
  (if kirigami-preserve-visual-position
      (kirigami--save-window-hscroll
        (kirigami--save-window-start
          (save-excursion
            (kirigami-fold-action kirigami-fold-list :close-all))))
    (kirigami-fold-action kirigami-fold-list :close-all)))

;;;###autoload
(defun kirigami-close-folds-except-current ()
  "Close all folds except the current one."
  (let ((point (point)))
    (kirigami-close-folds)
    (goto-char point)
    (kirigami-open-fold)))

(provide 'kirigami)
;;; kirigami.el ends here
