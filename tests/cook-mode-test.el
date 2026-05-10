;;; cook-mode-test.el --- cook-mode tests -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cook-mode)
(require 'ert)
(require 'ert-font-lock)

(defvar cook-mode-test-buffer
  (with-current-buffer (generate-new-buffer "brigadeiro.cook")
    (insert ">> servings: 20
>> time required: 45 minutes


[- brigadeiro.jpg -]

In a #medium saucepan{} over medium heat, combine @cocoa{3%tbsp}, @butter{1%tbsp} and @condensed milk{1%can}.

Cook, stirring, until thickened, about ~{10%minutes}.

Remove from heat and let rest until cool enough to handle.

Form into small balls and roll them over [- colored or chocolate -] @sprinkles{}.
-- Or you can eat it using a spoon")
    (current-buffer)))

(ert-deftest cook-mode-test-parse-ingredient ()
  (should (equal (cook-parse-ingredient "@foo{bar}")
                 '("foo" "bar" nil)))
  (should (equal (cook-parse-ingredient "@foo{20%tbsp}")
                 '("foo" "20" "tbsp"))))

(ert-deftest cook-mode-test-ingredients-list ()
  (with-current-buffer cook-mode-test-buffer
   (should (equal (cook-ingredients-list)
                  '(("cocoa" "3" "tbsp")
                    ("butter" "1" "tbsp")
                    ("condensed milk" "1" "can")
                    ("sprinkles" "" nil))))))

(ert-deftest cook-mode-test-show-ingredients ()
  (with-current-buffer cook-mode-test-buffer
    (should (string= (cook-show-ingredients)
                     "cocoa\t3\ttbsp
butter\t1\ttbsp
condensed milk\t1\tcan
sprinkles\t\t"))))

(ert-font-lock-deftest cook-mode-test-font-lock-source
  cook-mode
  "
>> author: John Doe
[- <- font-lock-comment-face -]
[- ^ cook-source-author-keyword-face -]
[-       ^ font-lock-comment-face -]
[-         ^ cook-source-author-face -]")

(ert-font-lock-deftest cook-mode-test-font-lock-author
  cook-mode
  "
>> source: https://example.com
[- <- font-lock-comment-face -]
[- ^ cook-source-author-keyword-face -]
[-       ^ font-lock-comment-face -]
[-         ^ cook-source-author-face -]")

(ert-font-lock-deftest cook-mode-test-font-lock-time-required
  cook-mode
  "
>> time required: 20 mins
[- <- font-lock-comment-face -]
[- ^ cook-time-keyword-face -]
[-              ^ font-lock-comment-face -]
[-                ^ cook-time-face -]")

(ert-font-lock-deftest cook-mode-test-font-lock-course
  cook-mode
  "
>> course: 4
[- <- font-lock-comment-face -]
[- ^ cook-course-keyword-face -]
[-       ^ font-lock-comment-face -]
[-         ^ cook-course-face -]")

(ert-font-lock-deftest cook-mode-test-font-lock-servings
  cook-mode
  "
>> servings: 20
[- <- font-lock-comment-face -]
[- ^ cook-servings-keyword-face -]
[-         ^ font-lock-comment-face -]
[-           ^ cook-servings-face -]")

(ert-font-lock-deftest cook-mode-test-font-lock-timer
  cook-mode
  "
cook for ~{20%minutes}
[- ^ nil -]
[-       ^ cook-timer-char-face -]
[-         ^ cook-timer-face -]")

(ert-font-lock-deftest cook-mode-test-font-lock-cookware-string
  cook-mode
  "
put into #medium saucepan{2}
[-       ^ cook-cookware-char-face -]
[-        ^ cook-cookware-face -]
[-                        ^ cook-cookware-quantity-face -]")

(ert-font-lock-deftest cook-mode-test-font-lock-cookware-word
  cook-mode
  "
put into #pan
[-       ^ cook-cookware-char-face -]
[-        ^ cook-cookware-face -]")

(ert-font-lock-deftest cook-mode-test-font-lock-ingredient
  cook-mode
  "
crack @eggs{4}
[-    ^ cook-ingredient-char-face -]
[-     ^ cook-ingredient-face -]
[-          ^ cook-ingredient-quantity-face -]")

(ert-font-lock-deftest cook-mode-test-font-lock-ingredient-unit
  cook-mode
  "
add @flour{125%g}
[-  ^ cook-ingredient-char-face -]
[-   ^ cook-ingredient-face -]
[-         ^ cook-ingredient-quantity-face -]
[-            ^ cook-ingredient-char-face -]
[-             ^ cook-ingredient-quantity-face -]")

(provide 'cook-mode-test)

;;; cook-mode-test.el ends here
