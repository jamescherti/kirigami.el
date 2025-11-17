;;; kirigami.el --- A unified method to fold and unfold text -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
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
;; A unified method to fold and unfold text.

;;; Code:

;; Kirigami offers a unified interface for text folding across a diverse set of
;; major and minor modes in Emacs, including outline-mode, outline-minor-mode,
;; outline-indent-mode, org-mode, markdown-mode, vdiff-mode, vdiff-3way-mode,
;; hs-minor-mode, hide-ifdef-mode, and origami-mode.
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

;;; Variables

(defgroup kirigami nil
  "A unified method to fold and unfold text."
  :group 'kirigami
  :prefix "kirigami-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/kirigami.el"))

(defvar kirigami-fold-list
  `(((vdiff-mode)
     :invisible-p  ,(lambda () (invisible-p (point)))
     :open-all   vdiff-open-all-folds
     :close-all  vdiff-close-all-folds
     :toggle     ,(lambda () (call-interactively 'vdiff-toggle-fold))
     :open       ,(lambda () (call-interactively 'vdiff-open-fold))
     :open-rec   ,(lambda () (call-interactively 'vdiff-open-fold))
     :close      ,(lambda () (call-interactively 'vdiff-close-fold)))
    ((vdiff-3way-mode)
     :invisible-p  ,(lambda () (invisible-p (point)))
     :open-all   vdiff-open-all-folds
     :close-all  vdiff-close-all-folds
     :toggle     ,(lambda () (call-interactively 'vdiff-toggle-fold))
     :open       ,(lambda () (call-interactively 'vdiff-open-fold))
     :open-rec   ,(lambda () (call-interactively 'vdiff-open-fold))
     :close      ,(lambda () (call-interactively 'vdiff-close-fold)))
    ((hs-minor-mode)
     :invisible-p  ,(lambda () (eq (get-char-property (point) 'invisible) 'hs))
     :open-all   hs-show-all
     :close-all  hs-hide-all
     :toggle     hs-toggle-hiding
     :open       hs-show-block
     :open-rec   nil
     :close      hs-hide-block)
    ((hide-ifdef-mode)
     :invisible-p  ,(lambda () (invisible-p (point)))
     :open-all   show-ifdefs
     :close-all  hide-ifdefs
     :toggle     nil
     :open       show-ifdef-block
     :open-rec   nil
     :close      hide-ifdef-block)
    ((outline-mode
      outline-minor-mode
      org-mode
      markdown-mode)
     :invisible-p  ,(lambda ()
                      (cond ((and (bound-and-true-p outline-minor-mode)
                                  (fboundp 'outline-invisible-p))
                             (funcall 'outline-invisible-p (point)))

                            ((and (derived-mode-p 'org-mode)
                                  (fboundp 'org-invisible-p))
                             (funcall 'org-invisible-p (point)))

                            (t (invisible-p (point)))))
     :open-all   show-all
     :close-all  ,(lambda ()
                    (with-no-warnings (hide-sublevels 1)))
     :toggle     outline-toggle-children
     :open       ,(lambda ()
                    (cond
                     ((and (bound-and-true-p outline-indent-minor-mode)
                           (fboundp 'outline-indent-open-fold))
                      (outline-indent-open-fold))

                     ((and (bound-and-true-p kirigami-outline-global-mode)
                           (fboundp 'kirigami-outline--show-entry))
                      (kirigami-outline--show-entry))

                     (t
                      (with-no-warnings
                        (ignore-errors
                          (show-entry)
                          (show-children))))))
     :open-rec   show-subtree
     :close      ;; hide-subtree
     ,(lambda ()
        (cond
         ((and (bound-and-true-p kirigami-outline-global-mode)
               (fboundp 'kirigami-outline--hide-subtree))
          (kirigami-outline--hide-subtree))

         (t
          (with-no-warnings
            (hide-subtree)))))
     ;; TODO try this again
     ;; :close kirigami--outline-close-previous-lower-heading-if-current-closed)
     ((origami-mode)
      :invisible-p  ,(lambda () (invisible-p (point)))
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
                                (origami-close-node (current-buffer) (point)))))))
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
  - `:invisible-p': Non-nil if the character after (point) is invisible.
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

;;; Internal variables

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

;;; Internal functions

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
              (with-demoted-errors "Error: %S" (funcall fn))))
        (kirigami-fold--action-get-func (cdr list) action ignore-errors)))))

(defun kirigami-fold-action (list action &optional ignore-errors)
  "Perform fold ACTION for each matching major or minor mode in LIST."
  (let ((fn (kirigami-fold--action-get-func list action ignore-errors)))
    (when fn
      (with-demoted-errors "Error: %S" (funcall fn)))))

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
  (kirigami-fold-action kirigami-fold-list :open-all))

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
  (kirigami-fold-action kirigami-fold-list :close-all))

;;;###autoload
(defun kirigami-close-folds-except-current ()
  "Close all folds except the current one."
  (let ((point (point)))
    (kirigami-close-folds)
    (goto-char point)
    (kirigami-open-fold)))

;;;###autoload
(defun kirigami-invisible-p ()
  "Return t if text is invisible."
  (kirigami-fold-action kirigami-fold-list :invisible-p))

(provide 'kirigami)
;;; kirigami.el ends here
