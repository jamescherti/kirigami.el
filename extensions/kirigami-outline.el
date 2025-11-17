;;; kirigami-outline.el --- Enhance outline -*- lexical-binding: t -*-

;;; Commentary:

;; Optional enhancements for `outline-mode', `outline-minor-mode', and related
;; modes such as `outline-indent-mode' and `org-mode'.

;;; Code:

;;; Require

(require 'outline)

;;; Variables

(defgroup kirigami-outline nil
  "Enhancements for `outline-mode', `outline-minor-mode', and related modes.
Optional enhancements for `outline-mode', `outline-minor-mode', and related
modes such as `outline-indent-mode' and `org-mode'."
  :group 'kirigami-outline
  :prefix "kirigami-outline-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/kirigami.el"))

;;; Functions

(defun kirigami--outline-heading-folded-p ()
  "Return non-nil if the body following the current heading is folded."
  (if (and (fboundp 'outline-back-to-heading)
           (fboundp 'outline-invisible-p))
      (save-excursion
        (outline-back-to-heading)
        (end-of-line)
        (outline-invisible-p (point)))
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
           (fboundp 'outline-show-children))
      (condition-case nil
          (let ((on-invisible-heading (save-excursion
                                        (beginning-of-line)
                                        (when (outline-on-heading-p t)
                                          (outline-invisible-p)))))
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
            (when on-invisible-heading
              (kirigami--outline-legacy-hide-subtree)))
        ;; `outline-back-to-heading' issue
        (outline-before-first-heading
         nil))
    (error "Required outline functions are undefined")))

(defun kirigami--outline-hide-subtree ()
  "Close the previous lower-level heading if current heading is folded or empty.
If the current heading is folded or empty, move to the previous heading
with a lower level and close its subtree. Otherwise, close the current subtree."
  (if (and (fboundp 'outline-back-to-heading)
           (fboundp 'outline-end-of-subtree)
           (fboundp 'outline-up-heading)
           (fboundp 'outline-on-heading-p))
      (save-excursion
        (outline-back-to-heading)
        (if (or (kirigami--outline-heading-folded-p)
                ;; Fold without any content
                (let ((start (save-excursion (end-of-line) (point)))
                      (end (save-excursion (outline-end-of-subtree) (point))))
                  (= start end)))
            (progn
              (outline-up-heading 1 t)
              (when (outline-on-heading-p)
                (kirigami--outline-legacy-hide-subtree)))
          (kirigami--outline-legacy-hide-subtree)))
    (error "Required outline functions are undefined")))

;;;###autoload
(define-minor-mode kirigami-outline-mode
  "Enhancements for `outline-mode', `outline-minor-mode', and related modes.
Optional enhancements for `outline-mode', `outline-minor-mode', and related
modes such as `outline-indent-mode' and `org-mode'."
  :global t
  :lighter " KirigamiOL"
  :group 'kirigami-outline
  (if kirigami-outline-mode
      ;; TODO advise show entry
      t
    t))

(provide 'kirigami-outline)
;;; kirigami-outline.el ends here
