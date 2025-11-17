;; Note: we want to retain dynamic binding for these tests because the
;; ERT "fixtures" rely on it.

;; To run the tests from within Emacs, you must `eval-buffer` this test
;; buffer first. Then, run tests using `ert-run-tests-interactively`.
;; But, to avoid having to evaluate the changes (which may affect the live
;; environment), it may be preferable to `make test` at the shell, instead.

;; Notes:
;; - If you see "lisp nesting exceeds max-lisp-eval-depth"
;;   while running these tests, it could be that you have a duplicate
;;   "body" invocation within one of the nested fixtures. Since these
;;   are dynamically bound, every fixture needs to have a distinct
;;   name for the body argument.
;; - If you see errors like "(void-function t)", "(void-function nil)"
;;   and "invalid function nil . 0"
;;   then you probably are using a fixture without wrapping the body
;;   in a lambda

;; Add source paths to load path so the tests can find the source files
;; Adapted from:
;; https://github.com/Lindydancer/cmake-font-lock/blob/47687b6ccd0e244691fb5907aaba609e5a42d787/test/cmake-font-lock-test-setup.el#L20-L27
(defvar elacarte-test-setup-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(dolist (dir '("." ".."))
  (add-to-list 'load-path
               (concat elacarte-test-setup-directory dir)))

;;

(require 'elacarte)

(require 'cl-lib)

;;
;; Fixtures
;;


;; fixture recipe from:
;; https://www.gnu.org/software/emacs/manual/html_node/ert/Fixtures-and-Test-Suites.html


(defmacro with-fixture (fixture &rest test)
  "Run TEST using FIXTURE."
  (declare (indent 1))
  `(,fixture
    (lambda ()
      ,@test)))

(defconst elacarte-test-recipes-file "test-recipes.eld")

(defun fixture-empty-cookbook (body)
  (let* ((default-directory elacarte-test-setup-directory)
         (elacarte-recipes-file elacarte-test-recipes-file))
    (unwind-protect
        (progn
          (with-temp-file elacarte-recipes-file
            ;; it must be contain a lisp datum
            ;; rather than be truly empty
            (insert "()"))
          (funcall body))
      (delete-file elacarte-test-recipes-file))))

(defun fixture-1-recipe-cookbook (body)
  (let* ((default-directory elacarte-test-setup-directory)
         (elacarte-recipes-file elacarte-test-recipes-file))
    (unwind-protect
        (progn
          (with-temp-file elacarte-recipes-file
            ;; it must be contain a lisp datum
            ;; rather than be truly empty
            (insert "((ela1 :host gitclub :repo \"my/ela1\"))"))
          (funcall body))
      (delete-file elacarte-test-recipes-file))))

(defun fixture-2-recipe-cookbook (body)
  (let* ((default-directory elacarte-test-setup-directory)
         (elacarte-recipes-file elacarte-test-recipes-file))
    (unwind-protect
        (progn
          (with-temp-file elacarte-recipes-file
            ;; it must be contain a lisp datum
            ;; rather than be truly empty
            (insert
             (concat "((ela1 :host gitclub :repo \"my/ela1\")" "\n"
                     "(ela-two :host gitplace :repo \"my/ela-two\"))")))
          (funcall body))
      (delete-file elacarte-test-recipes-file))))

;;
;; Utilities
;;

(defun contains-recipe-p (r recipes)
  "Does RECIPES contain R?"
  (member r (mapcar #'car recipes)))

(defun recipe-has-property-p (recipe prop value)
  "Does RECIPE have the property PROP with value VALUE?"
  (equal (plist-get (cdr recipe)
                    prop)
         value))

;;
;; Tests
;;

(ert-deftest add-recipe-test ()

  (with-fixture fixture-empty-cookbook
    (elacarte-add-recipe '(abc :host myhost :repo "my/abc"))
    (should (contains-recipe-p 'abc
                               (elacarte--read elacarte-recipes-file))))
  ;; does not replace existing
  (with-fixture fixture-1-recipe-cookbook
    (elacarte-add-recipe '(ela1 :host myhost :repo "my/abc"))
    (let ((recipes (elacarte--read elacarte-recipes-file)))
      (should (= 1 (length recipes)))
      (should (recipe-has-property-p (car recipes) :host 'gitclub))))

  ;; replaces existing
  (with-fixture fixture-1-recipe-cookbook
    (elacarte-add-recipe '(ela1 :host myhost :repo "my/abc")
                         'replace)
    (let ((recipes (elacarte--read elacarte-recipes-file)))
      (should (= 1 (length recipes)))
      (should (recipe-has-property-p (car recipes) :host 'myhost))))

  ;; marks as auto
  (with-fixture fixture-empty-cookbook
    (elacarte-add-recipe '(ela1 :host myhost :repo "my/abc")
                         nil
                         'auto)
    (should (recipe-has-property-p (car (elacarte--read elacarte-recipes-file))
                                   :auto t))))

(ert-deftest remove-recipe-test ()

  ;; error if not present
  (with-fixture fixture-empty-cookbook
    (should-error (elacarte-remove-recipe '(abc :host myhost :repo "my/abc"))))

  ;; remove if present
  (with-fixture fixture-1-recipe-cookbook
    (elacarte-remove-recipe 'ela1 'noconfirm)
    (let ((recipes (elacarte--read elacarte-recipes-file)))
      (should (null recipes))))

  ;; does not remove other recipes
  (with-fixture fixture-2-recipe-cookbook
    (elacarte-remove-recipe 'ela1 'noconfirm)
    (let ((recipes (elacarte--read elacarte-recipes-file)))
      (should (= 1 (length recipes)))
      (should (equal "ela-two" (elacarte--package-name (car recipes)))))))
