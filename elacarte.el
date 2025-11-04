;;; elacarte.el --- A la carte Emacs package recipes. -*- lexical-binding:t -*-

;; Author: Sid Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/elacarte
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT a part of Gnu Emacs.

;; This work is part of the world.  You are free to do whatever you
;; like with it and it isn't owned by anybody, not even the
;; creators.  Attribution would be appreciated and is a valuable
;; contribution in itself, but it is not strictly necessary nor
;; required.  If you'd like to learn more about this way of doing
;; things and how it could lead to a peaceful, efficient, and creative
;; world, and how you can help, visit https://drym.org.
;;
;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.

;;; Commentary:

;; Elacarte provides tools to conveniently manage package recipes in a
;; local recipe repository. It aims to make it easy for package
;; authors to distribute packages and for users to curate their own
;; package sources, as a viable alternative to centralized archives.

;;; Code:

(require 'cl-lib)
(require 'url)

(defvar elacarte-base-dir
  (expand-file-name "elacarte" user-emacs-directory)
  "The base path for elacarte operations.")

(defvar elacarte-repo-name "xelpa"
  "The name of the local recipe repository to be built.")

(defvar elacarte-recipes-file
  (expand-file-name "recipes.el" elacarte-base-dir)
  "The master file containing the list of local recipes.")

(defvar elacarte-temp-dir
  (expand-file-name "tmp" elacarte-base-dir)
  "Temporary directory for `elacarte` operations, like cloning repos.")

(defun elacarte-add-recipe (recipe &optional replace)
  "Add or update RECIPE in `elacarte-recipes-file'.

When called interactively, prompts for the recipe as a Lisp expression.
If a recipe for the same package already exists, it is
replaced only if a prefix argument is provided (in Lisp, if
REPLACE is non-nil). Otherwise, an error is signaled.

The file is created if it does not exist."
  (interactive (list (read-from-minibuffer "Enter recipe: ") current-prefix-arg))

  (let* (;; When called interactively, RECIPE is a string. We must parse it.
         ;; `read-from-string` returns (OBJECT . CHARS-READ), so we take the CAR.
         (recipe (if (stringp recipe) (car (read-from-string recipe)) recipe))
         (package-name (car recipe))
         (existing-recipes nil)
         (old-recipe nil))

    ;; 1. Read existing recipes, creating the file if it's missing.
    (unless (file-exists-p elacarte-recipes-file)
      (make-directory (file-name-directory elacarte-recipes-file) t)
      (with-temp-file elacarte-recipes-file
        (insert "()")))

    (setq existing-recipes (with-temp-buffer
                             (insert-file-contents elacarte-recipes-file)
                             ;; Use `read-from-string` to handle empty files gracefully.
                             (car (read-from-string (buffer-string)))))

    ;; Check if a recipe for this package already exists.
    (setq old-recipe (assoc package-name existing-recipes))

    ;; 2. If the recipe exists and `replace` is nil, signal an error.
    (when (and old-recipe (not replace))
      (user-error "Recipe for '%s' already exists. Use a prefix argument to replace it." package-name))

    ;; 3. Remove any old recipe for the same package to handle updates.
    (let ((updated-recipes (cl-remove-if
                            (lambda (r) (equal (car r) package-name))
                            existing-recipes)))

      ;; 4. Add the new recipe to the front and write back to disk.
      (with-temp-file elacarte-recipes-file
        (let ((print-level nil) (print-length nil)) ; Ensure full printing.
          ;; Use `pp` to pretty-print for better readability.
          (pp (cons recipe updated-recipes) (current-buffer)))))

    (message "Recipe for '%s' %s %s"
             package-name
             (if old-recipe "updated in" "added to")
             (file-name-nondirectory elacarte-recipes-file))))

(defun elacarte-remove-recipe (package-name)
  "Remove the recipe for PACKAGE-NAME from `elacarte-recipes-file'."
  (interactive
   (let ((recipes (with-temp-buffer
                    (insert-file-contents elacarte-recipes-file)
                    (car (read-from-string (buffer-string))))))
     (list (intern (completing-read "Remove recipe for package: "
                                    (mapcar #'car recipes))))))
  (let* ((existing-recipes (with-temp-buffer
                             (insert-file-contents elacarte-recipes-file)
                             (car (read-from-string (buffer-string)))))
         (recipe-to-remove (assoc package-name existing-recipes)))
    (unless recipe-to-remove
      (user-error "No recipe found for package '%s'" package-name))

    (when (y-or-n-p (format "Really remove recipe for '%s'?" package-name))
      (let ((updated-recipes (cl-remove-if
                              (lambda (r) (equal (car r) package-name))
                              existing-recipes)))
        (with-temp-file elacarte-recipes-file
          (let ((print-level nil) (print-length nil))
            (pp updated-recipes (current-buffer))))
        (message "Recipe for '%s' removed." package-name)))))

(defun elacarte--get-content-from-disk (file-path)
  "Return content of FILE-PATH as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun elacarte--get-content-from-web (url)
  "Return content of URL as a string."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (search-forward "\n\n") ; Skip HTTP headers
    (buffer-substring-no-properties (point) (point-max))))

(defun elacarte--add-recipes-from-list (recipes replace noconfirm)
  "Add RECIPES, prompting for confirmation unless NOCONFIRM is non-nil.
REPLACE is passed to `elacarte-add-recipe'."
  (let ((package-names (mapcar #'car recipes)))
    (when (or noconfirm
              (y-or-n-p
               (format "The following %d recipes will be added. Proceed?\n%s"
                       (length recipes)
                       (mapconcat #'symbol-name package-names ", "))))
      (dolist (recipe recipes)
        (elacarte-add-recipe recipe replace))
      (message "Successfully processed %d recipes." (length recipes)))))

(defun elacarte-add-recipes-by-file-url (url &optional replace noconfirm)
  "Fetch recipes from URL and add them to `elacarte-recipes-file'.

URL can be a local file path or a web URL pointing to a file
formatted like `elacarte-recipes-file'.

Prompts for confirmation before adding unless NOCONFIRM is non-nil. If
REPLACE is non-nil (or with a prefix argument interactively), existing
recipes will be overwritten."
  (interactive (list (read-string "URL or file path: ") current-prefix-arg))
  (let* ((content (if (string-match-p "^https?://" url)
                      (elacarte--get-content-from-web url)
                    (elacarte--get-content-from-disk url)))
         (recipes (car (read-from-string content))))
    (elacarte--add-recipes-from-list recipes replace noconfirm)))

(defun elacarte-add-recipes-by-repo-url (url &optional replace)
  "Clone git repository from URL and add recipes from its `recipes.el` file.

URL can be a remote URL or a local file path (including `~/`).
The repository is cloned to a temporary location and deleted
after the operation.

Prompts for confirmation before adding. If REPLACE is non-nil
(or with a prefix argument interactively), existing recipes
will be overwritten."
  (interactive (list (read-string "Repository URL or local path: ") current-prefix-arg))
  (make-directory elacarte-temp-dir t)
  (let* ((expanded-url (if (or (string-prefix-p "/" url)
                               (string-prefix-p "~" url))
                           (expand-file-name url)
                         url))
         (repo-name (file-name-nondirectory (file-name-sans-extension expanded-url)))
         (clone-dir (expand-file-name repo-name elacarte-temp-dir)))
    (unwind-protect
        (progn
          (message "Cloning repository from %s..." expanded-url)
          (unless (zerop (call-process "git" nil nil nil "clone" expanded-url clone-dir))
            (user-error "Failed to clone repository: %s" expanded-url))
          (message "Cloning complete.")

          (let* ((recipes-file (expand-file-name "recipes.el" clone-dir)))
            (unless (file-exists-p recipes-file)
              (user-error "No `recipes.el` file found in repository root."))
            (elacarte-add-recipes-by-file-url recipes-file replace)))
      ;; Cleanup: delete the temporary repository.
      (when (file-directory-p clone-dir)
        (delete-directory clone-dir t)
        (message "Cleaned up temporary repository: %s" clone-dir)))))

(defun elacarte-build-recipe-repository (&optional repo-name)
  "Build the local recipe repository from `elacarte-recipes-file'.
This parses the master recipe list and generates the individual
recipe files and the necessary protocol implementation file in
the `elacarte-base-dir'.
If REPO-NAME is nil, defaults to `elacarte-repo-name'."
  (interactive)
  (let* ((repo-name (or repo-name elacarte-repo-name))
         (repo-dir (expand-file-name repo-name elacarte-base-dir))
         (recipes-dir (expand-file-name "recipes" repo-dir))
         (protocol-file (expand-file-name (concat repo-name ".el") repo-dir))
         (recipes nil))
    (unless (file-exists-p elacarte-recipes-file)
      (user-error "Recipes file not found: %s" elacarte-recipes-file))

    (message "Building '%s' recipe repository..." repo-name)

    ;; 1. Clean and create the target directories.
    (when (file-directory-p repo-dir)
      (delete-directory repo-dir 'recursive))
    (make-directory recipes-dir 'parents)

    ;; 2. Generate and write the recipe protocol implementation file from the template.
    (let* ((template-file (expand-file-name "recipe-repo.el.template"
                                            (file-name-directory (locate-library "elacarte"))))
           (template-content (with-temp-buffer
                               (insert-file-contents template-file)
                               (buffer-string)))
           (protocol-content (format template-content repo-name)))
      (with-temp-file protocol-file
        (insert protocol-content)))

    ;; 3. Read the master list of recipes.
    (setq recipes (with-temp-buffer
                    (insert-file-contents elacarte-recipes-file)
                    (car (read-from-string (buffer-string)))))

    ;; 4. Write each recipe to its own file.
    (dolist (recipe recipes)
      (let* ((recipe-id (car recipe))
             (package-name (if (symbolp recipe-id) (symbol-name recipe-id) recipe-id))
             (target-file (expand-file-name package-name recipes-dir)))
        (with-temp-file target-file
          (prin1 recipe (current-buffer)))))

    (message "Successfully built %d recipes and protocol file in %s"
             (length recipes)
             repo-dir)))

(defun elacarte-register-recipe-repository (&optional repo-name)
  "Register the local recipe repository REPO-NAME with straight.el.
This function assumes the repository has already been built with
`elacarte-build-recipe-repository'. It performs the three steps
necessary to make the repository known to the current Emacs session.
If REPO-NAME is nil, defaults to `elacarte-repo-name'.
Interactively, also uses the value of `elacarte-repo-name'."
  (interactive (list elacarte-repo-name))
  (let* ((repo-name (or repo-name elacarte-repo-name))
         (repo-dir (expand-file-name repo-name elacarte-base-dir)))
    (when (file-directory-p repo-dir)
      (message "--- Registering '%s' recipe repository ---" repo-name)

      ;; 1. Make the package known to straight.el. `:build nil` is crucial.
      (straight-use-package
       `(,(intern repo-name) :type git :local-repo ,repo-dir :build nil))

      ;; 2. Load the recipe protocol implementation.
      (add-to-list 'load-path repo-dir)
      (require (intern repo-name))

      ;; 3. Add to the head of the list of repositories to search.
      (add-to-list 'straight-recipe-repositories (intern repo-name))

      (message "--- '%s' registration complete ---" repo-name))))


(provide 'elacarte)

;;; elacarte.el ends here
