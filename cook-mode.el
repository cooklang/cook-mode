;;; cook-mode.el Emacs mode for cooklang -*- lexical-binding: t; -*-
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
(require 'generic-x)

(defface cook-source-author-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for author keyword")
(defface cook-source-author-face
  '((t :inherit font-lock-string-face))
  "Face for author")

(defface cook-servings-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for servings keyword")

(defface cook-servings-face
  '((t :inherit font-lock-string-face))
  "Face for servings")

(defface cook-time-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for time required keyword")

(defface cook-time-face
  '((t :inherit font-lock-string-face))
  "Face for time required")


(defface cook-ingredient-char-face
  '((t :inherit font-lock-string-face))
  "Face for ingredient char")

(defface cook-ingredient-face
  '((t :inherit font-lock-keyword-face))
  "Face for ingredient name")

(defface cook-ingredient-quantity-face
  '((t :inherit font-lock-string-face))
  "Face for ingredient quantity")

(defface cook-cookware-char-face
  '((t :inherit font-lock-string-face))
  "Face for cookware char")

(defface cook-cookware-face
  '((t :inherit font-lock-keyword-face))
  "Face for cookware name")

(defface cook-cookware-quantity-face
  '((t :inherit font-lock-string-face))
  "Face for cookware quantity")




(define-generic-mode
    'cook-mode ;; mode name
  '("//") ;; comment
  '() ;; keywords
  '(
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

    ("~{[^}]*}" . 'font-lock-builtin-face)

    ("\\(?1:#\\)\\(?:\\(?2:[A-z\s-]*\\){\\(?3:[^}]*\\)}\\|\\(?2:[A-z]*\\)\\)" 1 'cook-cookware-char-face)
    ("\\(?1:#\\)\\(?:\\(?2:[A-z\s-]*\\){\\(?3:[^}]*\\)}\\|\\(?2:[A-z]*\\)\\)" 2 'cook-cookware-face)
    ("\\(?1:#\\)\\(?:\\(?:[A-z\s-]*\\){\\(?3:[^}]*\\)}\\|\\(?:[A-z]*\\)\\)" 3 'cook-cookware-quantity-face)

    ("\\(?1:@\\)\\(?:\\(?2:[A-z\s-]*\\){\\(?3:[^}]*\\)}\\|\\(?2:[A-z]*\\)\\)" 1 'cook-ingredient-char-face)
    ("\\(?1:@\\)\\(?:\\(?2:[A-z\s-]*\\){\\(?3:[^}]*\\)}\\|\\(?2:[A-z]*\\)\\)" 2 'cook-ingredient-face)
    ("\\(?:@\\)\\(?:\\(?:[A-z\s-]*\\){\\(?3:[^}]*\\)}\\|\\(:[A-z]*\\)\\)" 3 'cook-ingredient-quantity-face)
    ) ;; fonts
  '("\\.cook$")
  nil
  "A mode for cook recipes" ;; description
    )
(provide 'cook-mode)
;;; cook-mode.el ends here
