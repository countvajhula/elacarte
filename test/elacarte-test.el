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

(defun fixture-empty-recipes-file (body)
  (let* ((default-directory elacarte-test-setup-directory)
         (recipes-file "another-recipes.eld"))
    (unwind-protect
        (progn
          (with-temp-file recipes-file
            ;; it must be contain a lisp datum
            ;; rather than be truly empty
            (insert "()"))
          (funcall body))
      (delete-file recipes-file))))

(defun fixture-2-recipe-file (body)
  (let* ((default-directory elacarte-test-setup-directory)
         (recipes-file "another-recipes.eld"))
    (unwind-protect
        (progn
          (with-temp-file recipes-file
            ;; it must be contain a lisp datum
            ;; rather than be truly empty
            (insert
             (concat "((ela1 :host gitclub :repo \"my/ela1\")" "\n"
                     "(ela-two :host gitplace :repo \"my/ela-two\"))")))
          (funcall body))
      (delete-file recipes-file))))

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

(ert-deftest clean-room-install-test ()
  ;; installs it and returns normalized recipe containing the
  ;; :local-repo name
  (let ((recipe (elacarte-clean-room-install '(abc :type nil :host myhost :repo "my/abc"))))
    ;; note we use :type nil so that this doesn't actually attempt to
    ;; install the package
    (should (equal (plist-get recipe :local-repo)
                   "abc")))

  (let ((recipe (elacarte-clean-room-install '(abc :type nil :host myhost :repo "my/abc"))))
    ;; the location of the repo's recipes.eld file
    (should (plist-get recipe :recipes))))

(ert-deftest add-recipes-in-file-test ()
  ;; empty file - does nothing
  (with-fixture fixture-empty-cookbook
    (with-fixture fixture-empty-recipes-file
      (elacarte-add-recipes-in-file recipes-file)
      (should (null (elacarte--read elacarte-recipes-file)))))

  ;; 2 recipes - adds both
  (with-fixture fixture-empty-cookbook
    (with-fixture fixture-2-recipe-file
      (elacarte-add-recipes-in-file recipes-file)
      (should (contains-recipe-p 'ela1
                                 (elacarte--read elacarte-recipes-file)))
      (should (contains-recipe-p 'ela-two
                                 (elacarte--read elacarte-recipes-file))))))

(ert-deftest primary-recipe-p-test ()
  ;; if repo ids match, it's primary
  (let ((normalized-pointer-recipe '(:type nil :local-repo "abc"))
        (recipe '(another :type nil :local-repo "abc")))
    (should (elacarte--primary-recipe-p recipe normalized-pointer-recipe)))

  ;; if repo ids don't match, it's not
  (let ((normalized-pointer-recipe '(:type nil :local-repo "abc"))
        (recipe '(another :type nil :local-repo "another")))
    (should-not (elacarte--primary-recipe-p recipe normalized-pointer-recipe)))

  ;; primary override isn't considered primary
  (let ((normalized-pointer-recipe '(:type nil :local-repo "abc"))
        ;; TODO: we can't use "another" here as it's already been
        ;; registered as having been installed, so Straight seems to
        ;; do some form of check for the path (which we don't actually
        ;; install in these tests as we're using :type nil) and
        ;; doesn't find it.
        (recipe '(another-too :type nil :local-repo "another-too" :primary t)))
    (should-not (elacarte--primary-recipe-p recipe normalized-pointer-recipe))))

(ert-deftest pointer-recipe-p-test ()
  ;; if repo ids match, it's not a pointer
  (let ((normalized-pointer-recipe '(:type nil :local-repo "abc"))
        (recipe '(another :type nil :local-repo "abc")))
    (should-not (elacarte--pointer-recipe-p recipe normalized-pointer-recipe)))

  ;; if repo ids don't match, it is
  (let ((normalized-pointer-recipe '(:type nil :local-repo "abc"))
        (recipe '(another :type nil :local-repo "another")))
    (should (elacarte--pointer-recipe-p recipe normalized-pointer-recipe)))

  ;; primary override is still considered a pointer
  (let ((normalized-pointer-recipe '(:type nil :local-repo "abc"))
        ;; TODO: see above re: "another-too"
        (recipe '(another-too :type nil :local-repo "another-too" :primary t)))
    (should (elacarte--pointer-recipe-p recipe normalized-pointer-recipe))))

(ert-deftest primary-override-p-test ()
  (let ((recipe '(abc :type nil :local-repo "abc")))
    (should-not (elacarte--primary-override-p recipe)))
  (let ((recipe '(abc :type nil :local-repo "abc" :primary t)))
    (should (elacarte--primary-override-p recipe))))

(ert-deftest repo-id-test ()
  (let ((normalized-recipe '(:type nil :local-repo "my-abc")))
    (should (equal "my-abc"
                   (elacarte--repo-id normalized-recipe)))))

(ert-deftest package-name-test ()
  (let ((recipe '(abc :type nil :local-repo "my-abc")))
    (should (equal "abc"
                   (elacarte--package-name recipe)))))

(ert-deftest recipes-file-test ()
  (let ((normalized-recipe '(:type nil :local-repo "my-abc" :recipes "/path/to/abc/recipes.eld")))
    (should (equal "/path/to/abc/recipes.eld"
                   (elacarte--recipes-file normalized-recipe)))))

(ert-deftest pointer-recipe-for-url-test ()
  (let ((url "/path/to/abc"))
    (should (equal '(abc :repo "/path/to/abc")
                   (elacarte--pointer-recipe-for-url url))))
  (let ((url "https://git.club/abc"))
    (should (equal '(abc :repo "https://git.club/abc")
                   (elacarte--pointer-recipe-for-url url))))
  (let ((url "https://git.club/abc.el"))
    (should (equal '(abc.el :repo "https://git.club/abc.el")
                   (elacarte--pointer-recipe-for-url url))))
  (let ((url "git@github.com:my-user/abc"))
    (should (equal '(abc.el :repo "git@github.com:my-user/abc")
                   (elacarte--pointer-recipe-for-url url)))))
