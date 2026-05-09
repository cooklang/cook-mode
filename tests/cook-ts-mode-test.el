;;; cook-ts-mode-test.el --- Tests for cook-ts-mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'ert)
(require 'ert-x)
(require 'treesit)
(require 'cook-ts-mode)

;;; Font-face tests ============================================================

(ert-deftest cook-ts-mode-test-font-face ()
  (skip-unless (treesit-ready-p 'cooklang))
  (ert-test-erts-file (ert-resource-file "font-face.erts")))

(ert-deftest cook-ts-mode-test-yaml-injection ()
  "YAML keys in frontmatter get font-lock-keyword-face."
  (skip-unless (treesit-ready-p 'cooklang))
  (skip-unless (treesit-ready-p 'yaml))
  (ert-test-erts-file (ert-resource-file "yaml-injection.erts")))

;;; Ingredient extraction tests ================================================

(ert-deftest cook-ts-mode-test-ingredient-extraction-basic ()
  "Tree-sitter extraction returns correct (name qty unit) triples."
  (skip-unless (treesit-ready-p 'cooklang))
  (with-temp-buffer
    (insert "Add @flour{2%cups} and @butter{1%tbsp} to the mix.\n")
    (cook-ts-mode)
    (should (equal (cook-ts-mode-ingredients)
                   '(("flour" "2" "cups") ("butter" "1" "tbsp"))))))

(ert-deftest cook-ts-mode-test-ingredient-extraction-no-unit ()
  "Ingredient with quantity but no unit returns nil for unit."
  (skip-unless (treesit-ready-p 'cooklang))
  (with-temp-buffer
    (insert "Add @salt{to taste}.\n")
    (cook-ts-mode)
    (should (equal (cook-ts-mode-ingredients)
                   '(("salt" "to taste" nil))))))

(ert-deftest cook-ts-mode-test-ingredient-extraction-no-quantity ()
  "Ingredient with no braces returns nil for qty and unit."
  (skip-unless (treesit-ready-p 'cooklang))
  (with-temp-buffer
    (insert "Season with @salt and @pepper.\n")
    (cook-ts-mode)
    (should (equal (cook-ts-mode-ingredients)
                   '(("salt" nil nil) ("pepper" nil nil))))))

(ert-deftest cook-ts-mode-test-ingredient-extraction-ignores-comments ()
  "Ingredients mentioned inside comments are not extracted."
  (skip-unless (treesit-ready-p 'cooklang))
  (with-temp-buffer
    ;; Block comment and line comment both contain @ingredient syntax.
    ;; Only the real @onion ingredient should appear.
    (insert "[- @garlic{2%cloves} -]\n")
    (insert "-- @olive oil{2%tbsp} is optional\n")
    (insert "Chop @onion{1}.\n")
    (cook-ts-mode)
    (should (equal (cook-ts-mode-ingredients)
                   '(("onion" "1" nil))))))

(ert-deftest cook-ts-mode-test-ingredient-extraction-skips-references ()
  "Recipe references (@./path/Recipe) are not included in ingredient list."
  (skip-unless (treesit-ready-p 'cooklang))
  (with-temp-buffer
    (insert "Serve with @./sauces/Hollandaise{150%g} and @butter{50%g}.\n")
    (cook-ts-mode)
    ;; Only the real ingredient @butter appears; recipe reference is skipped.
    (should (equal (cook-ts-mode-ingredients)
                   '(("butter" "50" "g"))))))

;;; Mode setup tests ===========================================================

(ert-deftest cook-ts-mode-test-comment-start ()
  "comment-start is set to \"-- \" in cook-ts-mode."
  (skip-unless (treesit-ready-p 'cooklang))
  (with-temp-buffer
    (cook-ts-mode)
    (should (equal comment-start "-- "))
    (should (equal comment-end ""))))

(ert-deftest cook-ts-mode-test-imenu-sections ()
  "Sections appear in the imenu index."
  (skip-unless (treesit-ready-p 'cooklang))
  (with-temp-buffer
    (insert "= Prep =\nChop @onion{1}.\n\n= Cook =\nFry in #pan{}.\n")
    (cook-ts-mode)
    (let* ((index (cook-ts-mode--imenu-create-index))
           (sections (cdr (assoc "Sections" index))))
      (should sections)
      (should (assoc "Prep" sections))
      (should (assoc "Cook" sections)))))

;;; Ingredient display tests ===================================================

(ert-deftest cook-ts-mode-test-show-ingredients-nil-qty ()
  "show-ingredients displays empty string for nil quantity, not the string \"nil\"."
  (let (output)
    (cl-letf (((symbol-function 'cook-ts-mode-ingredients)
               (lambda () '(("flour" nil "cups"))))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq output (apply #'format fmt args)))))
      (cook-ts-mode-show-ingredients))
    (should (string-match-p "flour" output))
    (should (string-match-p "cups" output))
    (should-not (string-match-p "nil" output))))

;;; Image overlay tests ========================================================

(ert-deftest cook-ts-mode-test-block-comment-image-path-strips-delimiters ()
  "block-comment-image-path correctly strips [- -] delimiters."
  (cl-letf (((symbol-function 'treesit-node-text)
             (lambda (_node _no-prop) "[- brigadeiro.jpg -]")))
    (should (equal (cook-ts-mode--block-comment-image-path nil)
                   "brigadeiro.jpg"))))

(ert-deftest cook-ts-mode-test-image-overlay-no-file ()
  "No overlay is created when the referenced image file does not exist."
  (skip-unless (treesit-ready-p 'cooklang))
  (with-temp-buffer
    (insert "[- nonexistent-image-xyz.png -]\n")
    (cook-ts-mode)
    (cook-ts-mode--make-image-overlays)
    (should (null cook-ts-mode--image-overlays))))

(ert-deftest cook-ts-mode-test-image-path-detection ()
  "cook-ts-mode--image-path-p recognises common image extensions."
  (should (cook-ts-mode--image-path-p "photo.jpg"))
  (should (cook-ts-mode--image-path-p "photo.jpeg"))
  (should (cook-ts-mode--image-path-p "icon.png"))
  (should (cook-ts-mode--image-path-p "animation.gif"))
  (should (cook-ts-mode--image-path-p "icon.svg"))
  (should-not (cook-ts-mode--image-path-p "recipe.cook"))
  (should-not (cook-ts-mode--image-path-p "notes.txt"))
  (should-not (cook-ts-mode--image-path-p ""))
  (should-not (cook-ts-mode--image-path-p "just text")))

(provide 'cook-ts-mode-test)
;;; cook-ts-mode-test.el ends here
