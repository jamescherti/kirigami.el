;;; kirigami-jump.el --- Use Kirigami when jumping to a folded text -*- lexical-binding: t -*-

;;; Commentary:

;; The kirigami-jump package provides an additional layer of integration so that
;; any jump in Emacs automatically makes folded text visible. The benefit is
;; that navigation never lands inside an invisible or partially hidden region.
;;
;; Key advantages:
;;
;; * Jump targets from xref, imenu, consult, bookmarks, save-place, flymake,
;;   org-agenda, evil jumps, and grep are always shown in a fully unfolded
;;   context.
;;
;; * Parent headings are revealed so that the hierarchical position of the jump
;;   target is visible immediately.
;;
;; * Delayed unfolding avoids performance regressions during minibuffer-driven
;;   searches such as consult-ripgrep.
;;
;; * The logic is centralized in one place, rather than having each subsystem
;;   implement its own unfolding behavior.
;;
;; * The behavior is symmetrical across all supported modes because the
;;   unfolding is delegated to Kirigami's unified fold functions.

;;; Code:

;;; Require

(require 'kirigami)
(require 'kirigami-outline)
(require 'outline)

(defun kirigami-jump--outline-open-parent-headers (&rest _)
  "Reveal the entry at point while ignoring errors."
  (save-excursion
    (catch 'done
      (while t
        (unless (eq (ignore-errors (outline-up-heading 1 t)
                                   :success)
                    :success)
          (throw 'done t))

        (kirigami-outline--legacy-show-entry)))))

(defun kirigami-jump--open-fold (&rest _)
  "Ensure the current heading and body are fully visible."
  (interactive)
  (ignore-errors (kirigami-open-fold)))

(defun kirigami-jump--around-outline-show-entry (fn &rest args)
  "FN is the advised function. ARGS are the function arguments."
  (let ((result nil))
    (unwind-protect
        (progn (setq result (apply fn args))
               (outline-show-entry))
      result)))

(defun kirigami-jump--outline-after-jump-not-minibuffer ()
  "Use `run-at-time' or `run-with-idle-timer' to defer expensive work."
  ;; Checking minibufferp fixes consult fd and ripgrep slow down
  (unless (minibufferp)
    (run-with-idle-timer 0 nil #'kirigami-jump--outline-open-parent-headers)))

(defun kirigami-jump--delayed-open-parent-headers ()
  "Use `run-at-time' or `run-with-idle-timer' to defer expensive work."
  (run-with-idle-timer 0 nil #'kirigami-jump--outline-open-parent-headers))

(defun kirigami-jump--set-hooks (enable)
  "Enable or disable hooks and advices for automatic unfolding.
If ENABLE is non-nil, install hooks. Otherwise remove them."
  (let ((fn (if enable #'add-hook #'remove-hook)))
    (with-eval-after-load 'xref
      (funcall fn 'xref-after-jump-hook #'kirigami-jump--open-fold -80))

    ;; When pressing C-o for instance
    (with-eval-after-load 'evil
      (funcall fn 'evil-jumps-post-jump-hook #'kirigami-jump--open-fold -80))

    (with-eval-after-load 'saveplace
      (funcall fn 'save-place-after-find-file-hook #'kirigami-jump--open-fold -80))

    (with-eval-after-load 'bookmark
      (funcall fn 'bookmark-after-jump-hook #'kirigami-jump--open-fold -80))

    ;; grep-mode --> file
    (with-eval-after-load 'simple
      (funcall fn 'next-error-hook #'kirigami-jump--open-fold -80))

    ;; -------------------------------------------
    ;; OPEN PARENT HEADERS
    ;; Generally useful when parts of it are open
    ;; -------------------------------------------

    ;; For instance: `consult-line'
    ;; Seems to use my-outline-isearch-open-invisible-function
    (with-eval-after-load 'imenu
      (funcall fn 'imenu-after-jump-hook #'kirigami-jump--delayed-open-parent-headers -80))

    (with-eval-after-load 'consult
      (funcall fn 'consult-after-jump-hook #'kirigami-jump--outline-after-jump-not-minibuffer -80))

    ;; org agendo to TODO entry
    (with-eval-after-load 'org-agenda
      (funcall fn 'org-agenda-after-show-hook #'kirigami-jump--delayed-open-parent-headers -80)))
  (if enable
      (progn
        ;; ENABLE
        (with-eval-after-load 'flymake
          (advice-add 'flymake-goto-next-error :around #'kirigami-jump--around-outline-show-entry)
          (advice-add 'flymake-goto-prev-error :around #'kirigami-jump--around-outline-show-entry))

        ;; Unfold after going to a mark
        (with-eval-after-load 'evil
          ;; When using :number to jump to a line
          ;; TODO bug when pressing gg
          (advice-add 'evil-goto-line :around #'kirigami-jump--around-outline-show-entry)

          (advice-add 'evil-goto-mark :around #'kirigami-jump--around-outline-show-entry)))
    ;; DISABLE
    (with-eval-after-load 'flymake
      (advice-remove 'flymake-goto-next-error #'kirigami-jump--open-fold)
      (advice-remove 'flymake-goto-prev-error #'kirigami-jump--open-fold))

    (with-eval-after-load 'evil
      (advice-remove 'evil-goto-mark #'kirigami-jump--open-fold))))

(define-minor-mode kirigami-jump-mode
  "Enhancements for `jump-mode', `jump-minor-mode', and related modes.
Optional enhancements for `jump-mode', `jump-minor-mode', and related
modes such as `jump-indent-mode' and `org-mode'."
  :global t
  :lighter " KirigamiJ"
  :group 'kirigami-jum-
  (if kirigami-jump-mode
      (kirigami-jump--set-hooks t)
    (kirigami-jump--set-hooks nil)))

(provide 'kirigami-jump)
;;; kirigami-jump.el ends here
