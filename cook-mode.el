;;; cook-mode.el --- Emacs mode for cooklang -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Rafael Accácio Nogueira
;;
;; Author: Rafael Accácio Nogueira <https://github.com/Accacio>
;; Maintainer: Rafael Accácio Nogueira <raccacio@poli.ufrj.br>
;; Created: octobre 27, 2021
;; Modified: octobre 27, 2021
;; Version: 0.0.1
;; Keywords: cooking
;; Homepage: https://github.com/Accacio/cook-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cook" . cook-mode))

(autoload 'cook-yaml-narrow "cook-yaml-narrow" nil t)

(defgroup cook '()
  "Emacs mode for cooklang."
  :group 'languages
  :prefix "cook-")

(defcustom cook-mode-show-images t
  "Whether to show images."
  :group 'cook
  :type 'boolean)

(defcustom cook-image-filename-re (concat  "\\[-\s*\\(?3:'\\|\"\\|\\)\\(?1:.*\\."
                                           (regexp-opt '("png" "PNG" "JPG" "jpeg"
                                                         "jpg" "JPEG" "eps" "EPS"))
                                           "\\)\\(?:\\3\\)\s*-\\]")
  "Regexp to match image filenames in comments."
  :group 'cook
  :type 'regexp)

(defcustom cook-image-width 200
  "Width for preview images."
  :group 'cook
  :type 'natnum)

;;; Regular Expressions ========================================================

(defconst cook-cookware-re (rx (group-n 1 "#")
                               (or (seq (group-n 2 (zero-or-more (not ?{)))

                                        ?{
                                        (group-n 3 (zero-or-more (not ?})))
                                        ?})
                                   (group-n 2 (zero-or-more (any alpha))))))

;; Ingredient extraction
(defconst cook-ingredient-font-lock-re
  (rx (group ?@)
      (group (* (not (any ?{ whitespace))))
      (? (group (* (not ?{)))
         ?{
         (group (* (not ?})))
         ?}
         (? ?\(
            (group (* (not ?\))))
            ?\))))
  "Ingredient regex for fast font-locking.

Group 1: Matches @ marker.
Group 2 and 3: Matches the ingredient.
Group 4: Matches the quantity if available.
Group 5: Matches the preparations if available.")


(defconst cook-ingredient-parsing-re (rx (seq "@"
                                              (or (seq (group-n 1 (zero-or-more (not ?{))) ?{
                                                       (group-n 2 (zero-or-more (not (any ?% ?}))))
                                                       (? ?%
                                                          (group-n 3 (zero-or-more (not (any ?})))))
                                                       ?})
                                                  (group-n 1 (zero-or-more (not whitespace))))))
  "Ingredient regex for fast parsing.")

(defconst cook-timer-re (rx (group ?~)
                            (group (* (not ?{)))
                            ?{
                            (group (* (not ?})))
                            ?}))

;;; Syntax Highlighting ========================================================

(defvar cook-mode-font-lock `(
                              ;; source | author
                              ("\\(>>\\) \\(source\\|author\\)\\(:\\)\\(.*$\\)" 1 'font-lock-comment-face)
                              ("\\(>>\\) \\(source\\|author\\)\\(:\\)\\(.*$\\)" 3 'font-lock-comment-face)
                              ("\\(>>\\) \\(source\\|author\\)\\(:\\)\\(.*$\\)" 2 'cook-source-author-keyword-face)
                              ("\\(>>\\) \\(source\\|author\\)\\(:\\)\\(.*$\\)" 4 'cook-source-author-face)

                              ;; time required
                              ("\\(>>\\) \\(time required\\)\\(:\\)\\(.*$\\)" 1 'font-lock-comment-face)
                              ("\\(>>\\) \\(time required\\)\\(:\\)\\(.*$\\)" 3 'font-lock-comment-face)
                              ("\\(>>\\) \\(time required\\)\\(:\\)\\(.*$\\)" 2 'cook-time-keyword-face)
                              ("\\(>>\\) \\(time required\\)\\(:\\)\\(.*$\\)" 4 'cook-time-face)

                              ;; course
                              ("\\(>>\\) \\(course\\)\\(:\\)\\(.*$\\)" 1 'font-lock-comment-face)
                              ("\\(>>\\) \\(course\\)\\(:\\)\\(.*$\\)" 3 'font-lock-comment-face)
                              ("\\(>>\\) \\(course\\)\\(:\\)\\(.*$\\)" 2 'cook-course-keyword-face)
                              ("\\(>>\\) \\(course\\)\\(:\\)\\(.*$\\)" 4 'cook-course-face)

                              ;; servings
                              ("\\(>>\\) \\(servings\\)\\(:\\)\\(.*$\\)" 1 'font-lock-comment-face)
                              ("\\(>>\\) \\(servings\\)\\(:\\)\\(.*$\\)" 3 'font-lock-comment-face)
                              ("\\(>>\\) \\(servings\\)\\(:\\)\\(.*$\\)" 2 'cook-servings-keyword-face)
                              ("\\(>>\\) \\(servings\\)\\(:\\)\\(.*$\\)" 4 'cook-servings-face)

                              ("^\\(?1:=+\\)\\(?2:[^\n]*\\)$" 1 'cook-section-char-face)
                              ("^\\(?1:=+\\)\\(?2:[^\n]*\\)$" 2 'cook-section-name-face)

                              ;; timer
                              (,cook-timer-re 1 'cook-timer-char-face)
                              (,cook-timer-re 2 'cook-timer-name-face)
                              (,cook-timer-re 3 'cook-timer-face)

                              ;; cookware
                              (,cook-cookware-re 1 'cook-cookware-char-face)
                              (,cook-cookware-re 2 'cook-cookware-face)
                              (,cook-cookware-re 3 'cook-cookware-quantity-face nil t)

                              ;; ingredients
                              (,cook-ingredient-font-lock-re 1 'cook-ingredient-char-face)
                              (,cook-ingredient-font-lock-re 2 'cook-ingredient-face)
                              (,cook-ingredient-font-lock-re 3 'cook-ingredient-face nil t)
                              (,cook-ingredient-font-lock-re 4 'cook-ingredient-quantity-face nil t)
                              (,cook-ingredient-font-lock-re 5 'cook-ingredient-preparation-face nil t)))

(defface cook-source-author-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for author keyword.")
(defface cook-source-author-face
  '((t :inherit font-lock-string-face))
  "Face for author.")

(defface cook-servings-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for servings keyword.")

(defface cook-servings-face
  '((t :inherit font-lock-string-face))
  "Face for servings.")

(defface cook-time-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for time required keyword.")

(defface cook-time-face
  '((t :inherit font-lock-string-face))
  "Face for time required.")

(defface cook-course-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for course keyword.")

(defface cook-course-face
  '((t :inherit font-lock-string-face))
  "Face for course.")

(defface cook-ingredient-char-face
  '((t :inherit font-lock-string-face))
  "Face for ingredient char.")

(defface cook-ingredient-face
  '((t :inherit font-lock-keyword-face))
  "Face for ingredient name.")

(defface cook-ingredient-quantity-face
  '((t :inherit font-lock-string-face))
  "Face for ingredient quantity.")

(defface cook-ingredient-preparation-face
  '((t :inherit font-lock-string-face))
  "Face for ingredient preparation")

(defface cook-cookware-char-face
  '((t :inherit font-lock-string-face))
  "Face for cookware char.")

(defface cook-cookware-face
  '((t :inherit font-lock-keyword-face))
  "Face for cookware name.")

(defface cook-cookware-quantity-face
  '((t :inherit font-lock-string-face))
  "Face for cookware quantity.")

(defface cook-timer-char-face
  '((t :inherit font-lock-string-face))
  "Face for timer char.")

(defface cook-timer-name-face
  '((t :inherit font-lock-string-face))
  "Face for timer names")

(defface cook-timer-face
  '((t :inherit font-lock-string-face))
  "Face for timer.")

(defface cook-section-char-face
  '((t :inherit font-lock-string-face))
  "Face for section char")

(defface cook-section-name-face
  '((t :inherit font-lock-string-face))
  "Face for section name")


;;; Ingredient Parsing ===================================================================

(defun cook-parse-ingredient (ingredient-str)
  "Parse an string INGREDIENT-STR into a list.

The returned is in the format (INGREDIENT QUANTITY UNITS).  For example,
\"@olive oil{2%tbsp}\" gets parsed to (\"olive oil\" 2 \"tbsp\")."
  (declare (ftype (function (string) list)))
  (string-match cook-ingredient-parsing-re ingredient-str)
  (list (match-string 1 ingredient-str)
        (match-string 2 ingredient-str)
        (match-string 3 ingredient-str)))
(defun cook-ingredients-list ()
  "Return the ingredients list for the current buffer.

Each element is of the form (INGREDIENT QUANTITY UNITS), where UNITS can be nil."
  (declare (ftype (function () list)))
  (let ((ingredients '()))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward cook-ingredient-parsing-re nil t)
        (push (cook-parse-ingredient (match-string-no-properties 0)) ingredients)))
    ingredients))

(defun cook-show-ingredients ()
  "Show ingredients list."
  (declare (ftype (function () string)))
  (interactive)
  (let ((ingredients (cook-ingredients-list)))
    (with-temp-buffer
      (dolist (ingredient ingredients)
        (dolist (part ingredient)
          (insert (or part "") ?\t))
        (delete-char -1)
        (newline))
      (unless (bobp)
        (delete-char -1))
      (message "%s" (buffer-string)))))

;;;###autoload
(define-derived-mode cook-mode text-mode "cook"
  "A mode for cooklang recipes."
  (setq font-lock-defaults '(cook-mode-font-lock))
  (setq-local comment-start "[-"
              comment-end "-]")
  (setq-local syntax-propertize-function (syntax-propertize-rules ("^---$" (0 "!"))))
  (when cook-mode-show-images
    (font-lock-add-keywords nil
                            '((cook-mode-image-overlay-font-lock (0  'font-lock-keyword-face t))))))

(modify-syntax-entry ?\[ "(]1nc" cook-mode-syntax-table)
(modify-syntax-entry ?\] ")[4nc" cook-mode-syntax-table)
(modify-syntax-entry ?{ "(}" cook-mode-syntax-table)
(modify-syntax-entry ?} "){" cook-mode-syntax-table)
(modify-syntax-entry ?\( "()" cook-mode-syntax-table)
(modify-syntax-entry ?\) ")(" cook-mode-syntax-table)
(modify-syntax-entry ?- ". 123b" cook-mode-syntax-table)
(modify-syntax-entry ?\n "> b" cook-mode-syntax-table)

(keymap-set cook-mode-map "C-c C-i" #'cook-show-ingredients)
(keymap-set cook-mode-map "C-c C-t" #'cook-mode-toggle-show-images)
(keymap-set cook-mode-map "C-c C-n" #'cook-yaml-narrow)

(defun cook-mode-toggle-show-images ()
  "Toggle whether to show images from comments."
  (interactive)
  (setq cook-mode-show-images (not cook-mode-show-images))
  (if cook-mode-show-images
      (font-lock-add-keywords nil
                            '((cook-mode-image-overlay-font-lock (0 'font-lock-keyword-face t))))
    (mapc #'delete-overlay (overlays-in (point-min) (point-max)))
    (font-lock-remove-keywords nil
                               '((cook-mode-image-overlay-font-lock (0 'font-lock-keyword-face t)))))
  (font-lock-flush))

;; From org-mode
(defun cook-mode-remove-overlay (ov after _beg _end &optional _len)
  "Remove inline-display overlay if a corresponding region is modified."
  (let ((inhibit-modification-hooks t))
    (when (and ov after)
      (delete-overlay ov))))

(defun cook-mode-image-overlay-font-lock (&optional limit)
  (when (re-search-forward cook-image-filename-re limit t)
    (let ((beg (match-beginning 0))
          (end (match-end 0))
          (imgfile (match-string 1)))
      (when (file-exists-p imgfile)
        (let ((img (create-image (expand-file-name imgfile)
                                 nil nil :width cook-image-width))
              (ov (make-overlay beg end)))
          (overlay-put ov 'display img)
          (overlay-put ov 'face 'default)
          (overlay-put ov 'modification-hooks '(cook-mode-remove-overlay)))))))

(provide 'cook-mode)
;;; cook-mode.el ends here
