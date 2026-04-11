;;; cook-ts-mode.el --- Tree-sitter major mode for cooklang -*- lexical-binding: t -*-

;; Copyright (C) 2026 Alexandre Avanian

;; Author: Alexandre Avanian <https://github.com/aavanian>
;; Homepage: https://github.com/cooklang/cook-mode
;; Keywords: cooking cooklang tree-sitter
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.2"))

;;; Commentary:
;; Tree-sitter-based major mode for cooklang recipe files.
;; Derives from text-mode. Self-contained: does not require cook-mode.

;;; Code:

(require 'treesit)
(require 'rx)

;;; Font-lock rules ============================================================

(defvar cook-ts-mode--font-lock-rules
  (treesit-font-lock-rules

   :language 'cooklang
   :feature 'comment
   `((comment) @font-lock-comment-face
     (block_comment) @font-lock-comment-face)

   :language 'cooklang
   :feature 'keyword
   `((metadata key: (metadata_key) @font-lock-keyword-face))

   :language 'cooklang
   :feature 'metadata
   `((metadata ":" @font-lock-delimiter-face)
     (metadata value: (metadata_value) @font-lock-string-face))

   :language 'cooklang
   :feature 'section
   `((section_name) @font-lock-type-face)

   :language 'cooklang
   :feature 'ingredient
   `("@" @font-lock-punctuation-face
     (ingredient name: (ingredient_name) @font-lock-constant-face))

   :language 'cooklang
   :override t
   :feature 'ingredient
   `((ingredient name: (recipe_reference) @font-lock-string-face))

   :language 'cooklang
   :feature 'cookware
   `("#" @font-lock-punctuation-face
     (cookware name: (cookware_name) @font-lock-constant-face))

   :language 'cooklang
   :feature 'timer
   `("~" @font-lock-punctuation-face
     (timer name: (timer_name) @font-lock-constant-face))

   :language 'cooklang
   :feature 'note
   `((recipe_note ">" @font-lock-punctuation-face)
     (recipe_note text: (recipe_note_text) @font-lock-string-face))

   :language 'cooklang
   :feature 'number
   `((quantity_value) @font-lock-number-face)

   :language 'cooklang
   :feature 'unit
   `((quantity_unit) @font-lock-type-face)

   :language 'cooklang
   :feature 'operator
   `((percent_separator) @font-lock-operator-face)

   :language 'cooklang
   :feature 'string
   `((preparation content: (preparation_content) @font-lock-string-face))

   :language 'cooklang
   :feature 'punctuation
   `([ "(" ")" "{" ] @font-lock-bracket-face
     "---" @font-lock-delimiter-face)))

(defvar cook-ts-mode--frontmatter-fallback-rules
  (treesit-font-lock-rules
   :language 'cooklang
   :feature 'frontmatter
   `((frontmatter_content) @font-lock-string-face))
  "Fallback rules for frontmatter when yaml grammar is unavailable.")

;;; YAML injection =============================================================

(defvar-local cook-ts-mode--cooklang-parser nil
  "Buffer-local cooklang parser.")

(defvar-local cook-ts-mode--yaml-parser nil
  "Buffer-local YAML parser, nil if yaml grammar unavailable.")

(defvar-local cook-ts-mode--yaml-query nil
  "Compiled query for locating frontmatter_content nodes.")

(defvar-local cook-ts-mode--imenu-query nil
  "Compiled query for locating section_name nodes.")

(defun cook-ts-mode--update-yaml-ranges (&optional _beg _end _len)
  "Re-derive YAML parser ranges from the cooklang tree."
  (when (and cook-ts-mode--cooklang-parser
             cook-ts-mode--yaml-parser
             cook-ts-mode--yaml-query)
    (when-let* ((root (treesit-parser-root-node cook-ts-mode--cooklang-parser))
                (captures (treesit-query-capture root cook-ts-mode--yaml-query)))
      (treesit-parser-set-included-ranges
       cook-ts-mode--yaml-parser
       (mapcar (lambda (pair)
                 (let ((node (cdr pair)))
                   (cons (treesit-node-start node)
                         (treesit-node-end node))))
               captures)))))

;;; Imenu ======================================================================

(defun cook-ts-mode--imenu-create-index ()
  "Build imenu index for cooklang buffer.
Returns sections as an alist under the \\\"Sections\\\" key."
  (when cook-ts-mode--cooklang-parser
    (let* ((root (treesit-parser-root-node cook-ts-mode--cooklang-parser))
           (captures (treesit-query-capture root cook-ts-mode--imenu-query))
           sections)
      (dolist (pair captures)
        (let* ((node (cdr pair))
               (raw  (treesit-node-text node t))
               ;; Section name text includes surrounding "= =" delimiters; strip them.
               (name (if (string-match "\\`=?[[:space:]]*\\(.*?\\)[[:space:]]*=?\\'" raw)
                         (match-string 1 raw)
                       raw))
               (pos  (copy-marker (treesit-node-start node))))
          (push (cons name pos) sections)))
      (when sections
        (list (cons "Sections" (nreverse sections)))))))

;;; Ingredient extraction ======================================================

(defun cook-ts-mode-ingredients ()
  "Return ingredients as a list of (NAME QUANTITY UNIT) triples.
Each element may have nil QUANTITY or UNIT when absent.
Uses tree-sitter parsing, so ingredients inside comments are ignored."
  (let* ((root (treesit-buffer-root-node 'cooklang))
         (captures (treesit-query-capture root '((ingredient) @ingredient)))
         result)
    (dolist (pair captures)
      (let* ((node (cdr pair))
             (name-node (treesit-node-child-by-field-name node "name"))
             (name-type (when name-node (treesit-node-type name-node))))
        ;; Skip recipe references (@./path/to/Recipe) — they are not ingredients
        (when (and name-node (string= name-type "ingredient_name"))
          (let* ((name (treesit-node-text name-node t))
                 (qty-node (car (treesit-filter-child
                                 node
                                 (lambda (n) (string= (treesit-node-type n) "quantity"))
                                 t)))
                 (val-node (when qty-node
                             (treesit-node-child-by-field-name qty-node "value")))
                 (unit-node (when qty-node
                              (treesit-node-child-by-field-name qty-node "unit")))
                 (qty  (when val-node  (treesit-node-text val-node t)))
                 (unit (when unit-node (treesit-node-text unit-node t))))
            (push (list name qty unit) result)))))
    (nreverse result)))

;;; Ingredient display ==========================================================

(defun cook-ts-mode-show-ingredients ()
  "Show ingredients from the current buffer in the echo area."
  (interactive)
  (let* ((ingredients (cook-ts-mode-ingredients))
         (table (apply #'concat
                       (mapcar (lambda (i)
                                 (pcase i
                                   (`(,name ,qty ,unit)
                                    (if unit
                                        (format "%s\t%s\t%s\n" name (or qty "") unit)
                                      (format "%s\t%s\n" name (or qty ""))))))
                               ingredients))))
    (if (string-empty-p table)
        (message "No ingredients found.")
      (message "%s" table))))

;;; Image overlays =============================================================

(defvar-local cook-ts-mode--image-overlays nil
  "List of overlays showing inline images.")

(defun cook-ts-mode--image-path-p (text)
  "Return non-nil if TEXT (stripped of [- -] wrapper) looks like an image path."
  (string-match-p (rx "." (or "jpg" "jpeg" "png" "gif" "webp" "svg") eos)
                  (string-trim text)))

(defun cook-ts-mode--block-comment-image-path (node)
  "Return the image file path from block_comment NODE, or nil."
  (let* ((raw (treesit-node-text node t))
         ;; Strip [- and -] delimiters added by the external scanner
         (inner (replace-regexp-in-string
                 (rx bos (? "[" "-" (* space)) (group (*? nonl)) (* space) (? "-" "]") eos)
                 "\\1"
                 (string-trim raw))))
    (when (cook-ts-mode--image-path-p inner)
      inner)))

(defun cook-ts-mode--clear-image-overlays ()
  "Remove all image overlays from the current buffer."
  (mapc #'delete-overlay cook-ts-mode--image-overlays)
  (setq-local cook-ts-mode--image-overlays nil))

(defun cook-ts-mode--make-image-overlays ()
  "Create overlays for block comments whose content is an image path."
  (cook-ts-mode--clear-image-overlays)
  (when-let* ((root (treesit-buffer-root-node 'cooklang))
              (captures (treesit-query-capture root '((block_comment) @bc))))
    (dolist (pair captures)
      (let* ((node (cdr pair))
             (path (cook-ts-mode--block-comment-image-path node)))
        (when path
          (let* ((abs (expand-file-name path (file-name-directory
                                              (or buffer-file-name default-directory))))
                 (start (treesit-node-start node))
                 (end   (treesit-node-end node)))
            (when (and (file-readable-p abs)
                       (image-type-available-p
                        (intern (file-name-extension abs))))
              (let* ((img (create-image abs nil nil :max-width 400))
                     (ov  (make-overlay start end)))
                (overlay-put ov 'display img)
                (overlay-put ov 'cook-ts-image t)
                (push ov cook-ts-mode--image-overlays)))))))))

(defun cook-ts-mode-toggle-show-images ()
  "Toggle inline display of images referenced in block comments."
  (interactive)
  (if cook-ts-mode--image-overlays
      (cook-ts-mode--clear-image-overlays)
    (cook-ts-mode--make-image-overlays)))

;;; Major mode setup ===========================================================

(defun cook-ts-mode--setup ()
  "Set up tree-sitter for cooklang."
  (setq-local cook-ts-mode--cooklang-parser (treesit-parser-create 'cooklang))

  ;; Compile frontmatter query now that the parser exists
  (setq-local cook-ts-mode--yaml-query
              (treesit-query-compile 'cooklang '((frontmatter_content) @content)))
  (setq-local cook-ts-mode--imenu-query
              (treesit-query-compile 'cooklang '((section_name) @name)))

  (setq-local treesit-font-lock-settings cook-ts-mode--font-lock-rules)
  (setq-local treesit-font-lock-feature-list
              '((comment)
                (keyword metadata section ingredient cookware timer note)
                (punctuation number unit operator string)))

  ;; Frontmatter: inject YAML highlighting if grammar available,
  ;; otherwise fall back to plain string face.
  (if (treesit-language-available-p 'yaml)
      (progn
        (setq-local cook-ts-mode--yaml-parser (treesit-parser-create 'yaml))
        (setq-local treesit-font-lock-settings
                    (append treesit-font-lock-settings
                            (treesit-font-lock-rules
                             :language 'yaml
                             :feature 'yaml-keyword
                             `((block_mapping_pair key: (flow_node) @font-lock-keyword-face))
                             :language 'yaml
                             :feature 'yaml-string
                             `((block_mapping_pair value: (flow_node) @font-lock-string-face)))))
        (setq-local treesit-font-lock-feature-list
                    '((comment)
                      (keyword metadata section ingredient cookware timer note)
                      (punctuation number unit operator string yaml-keyword yaml-string)))
        (add-hook 'after-change-functions
                  #'cook-ts-mode--update-yaml-ranges nil t)
        (cook-ts-mode--update-yaml-ranges))
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings
                        cook-ts-mode--frontmatter-fallback-rules))
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (keyword metadata section ingredient cookware timer note)
                  (punctuation number unit operator string frontmatter))))

  ;; Imenu: sections as index entries, using explicit cooklang parser
  (setq-local imenu-create-index-function #'cook-ts-mode--imenu-create-index)

  ;; Comment syntax for M-;
  (setq-local comment-start "-- ")
  (setq-local comment-end "")

  (treesit-major-mode-setup))

(defvar cook-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i") #'cook-ts-mode-show-ingredients)
    (define-key map (kbd "C-c C-i") #'cook-ts-mode-toggle-show-images)
    map)
  "Keymap for Cook TS major mode.")

;;;###autoload
(define-derived-mode cook-ts-mode text-mode "Cook TS"
  "Major mode for cooklang recipe files, powered by tree-sitter."
  :group 'cook-ts

  (unless (treesit-ready-p 'cooklang)
    (error "Tree-sitter grammar for cooklang is not available"))

  (cook-ts-mode--setup))

;;;###autoload
(when (treesit-ready-p 'cooklang)
  ;; Upgrade cook-mode → cook-ts-mode automatically when both are loaded.
  (add-to-list 'major-mode-remap-alist '(cook-mode . cook-ts-mode))
  ;; Also register directly for standalone use (without cook-mode).
  (add-to-list 'auto-mode-alist '("\\.cook\\'" . cook-ts-mode)))

(provide 'cook-ts-mode)
;;; cook-ts-mode.el ends here
