;;; elacarte.el --- A la carte Emacs package recipes. -*- lexical-binding: t -*-

;; Author: Sid Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/elacarte
;; Version: 0.1
;; Package-Requires: ((emacs "24.1") (straight))

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
(require 'straight)

(defvar elacarte-base-dir
  (expand-file-name "elacarte" user-emacs-directory)
  "The base path for elacarte operations.")

(defvar elacarte-repo-name "elacarte-cookbook"
  "The name of the local recipe repository to be built.")

(defvar elacarte-recipes-filename "recipes.eld"
  "The conventional name of the file containing recipes.

Any project would advertise recipes using this filename at the top
level of the source repository, and Elacarte would allow users to
conveniently install those packages. A user's Emacs configuration
would have such a file at the top level of their .emacs.d, and it
would house the user's curated and preferred recipes.")

(defvar elacarte-recipes-file
  (expand-file-name elacarte-recipes-filename
                    elacarte-base-dir)
  "The master file containing the list of local recipes.")

(defvar elacarte-temp-dir
  (expand-file-name "tmp" elacarte-base-dir)
  "Temporary directory for `elacarte` operations, like cloning repos.")

(defun elacarte--get-content-from-disk (file-path)
  "Return content of FILE-PATH as a string."
  (unless (file-exists-p file-path)
    (user-error "File not found: %s" file-path))
  (with-temp-buffer
    (insert-file-contents-literally file-path)
    (buffer-string)))

(defun elacarte--read-data (file)
  "Read a Lisp datum from FILE."
  (car
   (read-from-string
    (elacarte--get-content-from-disk file))))

(defun elacarte--write (file string)
  "Write STRING to FILE."
  (with-temp-file file
    (insert string)))

(defun elacarte--pretty-print (obj)
  "A readable string representation of OBJ."
  (let ((print-level nil) (print-length nil)) ; Ensure full printing.
    (pp-to-string obj)))

(defun elacarte--remove-recipe (package-name recipes)
  "Remove PACKAGE-NAME from RECIPES."
  (cl-remove-if
   (lambda (r) (equal (car r) package-name))
   recipes))

(defun elacarte-remove-recipe (package-name)
  "Remove the recipe for PACKAGE-NAME from `elacarte-recipes-file'."
  (interactive
   (let ((recipes (elacarte--read-data elacarte-recipes-file)))
     (list (intern (completing-read "Remove recipe for package: "
                                    (mapcar #'car recipes))))))
  (let* ((existing-recipes (elacarte--read-data elacarte-recipes-file))
         (recipe-to-remove (assoc package-name existing-recipes)))
    (unless recipe-to-remove
      (user-error "No recipe found for package '%s'" package-name))

    (when (y-or-n-p (format "Really remove recipe for '%s'?" package-name))
      (let ((updated-recipes (elacarte--remove-recipe package-name
                                                      existing-recipes)))
        (elacarte--write elacarte-recipes-file
                         (elacarte--pretty-print updated-recipes))
        (message "Recipe for '%s' removed." package-name)))))

(defun elacarte-add-recipe (recipe &optional replace auto)
  "Add or update RECIPE in `elacarte-recipes-file'.

When called interactively, prompts for the recipe as a Lisp expression.
If a recipe for the same package already exists, it is
replaced only if a prefix argument is provided (in Lisp, if
REPLACE is non-nil). Otherwise, an error is signaled.

If AUTO is set, the recipe gains an `:auto t` property. This tells
Elacarte that this is a recipe it should automatically manage,
including overwriting it if it is updated in the project source
repository. Typically, for canonical recipes that are advertised by
the project repo, this should be non-nil. For bespoke recipes added
manually by the user, this should be nil.

The file is created if it does not exist."
  (interactive (list (read-from-minibuffer "Enter recipe: ")
                     current-prefix-arg
                     ;; auto is nil when called interactively, i.e.,
                     ;; this recipe is managed by the user, not by
                     ;; Elacarte
                     nil))

  (let* (;; When called interactively, RECIPE is a string. We must parse it.
         ;; `read-from-string` returns (OBJECT . CHARS-READ), so we take the CAR.
         (recipe (if (stringp recipe) (car (read-from-string recipe)) recipe))
         (package-name (car recipe))
         ;; recipes added by elacarte are managed automatically
         ;; we mark these out in the recipes file to distinguish
         ;; them from bespoke recipes added by the user
         (recipe (if auto
                     (cons package-name
                           (plist-put (cdr recipe) :auto t))
                   recipe)))

    ;; 1. Read existing recipes, creating the file if it's missing.
    (unless (file-exists-p elacarte-recipes-file)
      (make-directory (file-name-directory elacarte-recipes-file) t)
      (elacarte--write elacarte-recipes-file
                       "()"))

    ;; Check if a recipe for this package already exists.
    (let* ((existing-recipes (elacarte--read-data elacarte-recipes-file))
           (old-recipe (assoc package-name existing-recipes))
           (old-auto (plist-get (cdr old-recipe) :auto)))

      ;; 2. If the recipe exists and `replace` is nil, and it isn't an
      ;; automatically maintained (i.e., by Elacarte) recipe, then
      ;; signal an error.
      (if (and old-recipe (not old-auto) (not replace))
          ;; do nothing for this recipe
          (warn "Recipe for '%s' already exists. Use a prefix argument to replace it." package-name)

        ;; 3. Remove any old recipe for the same package to handle updates.
        (let ((updated-recipes (elacarte--remove-recipe package-name
                                                        existing-recipes)))

          ;; 4. Add the new recipe to the front and write back to disk.
          (elacarte--write elacarte-recipes-file
                           (elacarte--pretty-print
                            (cons recipe updated-recipes))))

        (message "Recipe for '%s' %s %s"
                 package-name
                 (if old-recipe "updated in" "added to")
                 (file-name-nondirectory elacarte-recipes-file))))))

(defun elacarte--clean-room-install (recipe)
  "Install RECIPE in a clean room environment.

Return the normalized recipe that contains details of the actual
installation such as the location of the :local-repo."
  ;; We create a true "clean room" by let-binding the base-dir
  ;; and all of straight.el's in-memory caches.
  (let ((package-name (elacarte--package-name recipe))
        (straight-base-dir elacarte-temp-dir)
        (straight-allow-recipe-inheritance nil)
        (straight--success-cache (make-hash-table :test 'equal))
        (straight--recipe-cache (make-hash-table :test 'equal))
        (straight--repo-cache (make-hash-table :test 'equal))
        (straight--profile-cache (make-hash-table :test 'equal)))
    (message "Ensuring package repo for '%s' is available (clean room)..." package-name)
    ;; Ensure the recipe is installed (idempotent)
    ;; This call has the side effect of cloning the repo AND
    ;; registering the normalized recipe in the (temporary)
    ;; `straight--recipe-cache`.
    (straight-use-package-no-build recipe)
    ;; Return the normalized recipe that straight.el just created,
    ;; which contains details of the actual installation such as
    ;; the :local-repo for the package.
    (let* ((normalized-recipe (gethash package-name straight--recipe-cache))
           (repo-name (elacarte--repo-name normalized-recipe))
           (recipes-file (expand-file-name elacarte-recipes-filename
                                           (straight--repos-dir repo-name))))
      (plist-put normalized-recipe :recipes recipes-file))))

(defun elacarte--cleanup-temp-dir ()
  "Delete `elacarte-temp-dir' if present."
  (if (file-directory-p elacarte-temp-dir)
      (progn (delete-directory elacarte-temp-dir t)
             (message "Cleaned up temporary repositories."))
    (message "Nothing to clean up.")))

(defun elacarte-add-recipes-in-file (recipes-file)
  "A low-level utility to add all recipes in RECIPES-FILE.

This does not do any validation or traversal, and simply adds the
recipes in the file to `elacarte-recipes-file', replacing any existing
recipes for the same packages.

This should generally not be used except by tools implementing
higher-level functionality."
  (let ((recipes (elacarte--read-data recipes-file)))
    (message "Adding recipes in '%s'..." recipes-file)
    (dolist (recipe recipes)
      ;; Compare the recipe's repo with the repo we are currently in.
      (message "  -> Adding recipe for '%s'"
               (elacarte--package-name recipe))
      (elacarte-add-recipe recipe :replace :auto))
    (message "Successfully processed %d recipes." (length recipes))))

(defun elacarte-traverse-recipes-file (recipes-file)
  "Traverse RECIPES-FILE and recursively add recipes discovered.

This treats all recipes in the file as pointers. If they are in fact
primary, the repo will be redundantly rebuilt (which is idempotent,
however), and if not, the upstream repo will be cloned and built and
the recipes there discovered. This ensures that the true, canonical
recipes are discovered for each of the referenced packages, even if
there is some initial redundancy."
  (interactive "fRecipes file: ")
  (message "Traversing recipes in %s" recipes-file)
  (let ((visited-repos (make-hash-table :test 'equal)))
    (elacarte--traverse-recipes-file recipes-file
                                     nil
                                     'replace
                                     'noconfirm
                                     nil
                                     visited-repos)
    (elacarte--cleanup-temp-dir)))

(defun elacarte--primary-recipe-p (recipe normalized-pointer)
  "Is RECIPE primary in relation to NORMALIZED-POINTER?

NORMALIZED-POINTER is a recipe pointing to a repository containing Emacs packages.

Primary recipes are those that either point to the containing repo
(and not to a third party (e.g., dependency) repo) or which have
`:primary t`, an overriding flag that allows projects to provide
recipes on behalf of another package, necessary in rare cases.
Pointers point to a different repo, where, typically, primary recipes
for that repo may be discovered."
  (let* ((repo-name (elacarte--repo-name normalized-pointer))
         (normalized-recipe (elacarte--clean-room-install recipe)))
    (or (equal repo-name
               (elacarte--repo-name normalized-recipe))
        (elacarte--primary-override-p normalized-recipe))))

(defun elacarte--pointer-recipe-p (recipe normalized-pointer)
  "Is RECIPE a pointer in relation to NORMALIZED-POINTER?

NORMALIZED-POINTER is a recipe pointing to a repository containing Emacs packages.

Primary recipes are those that either point to the containing repo
(and not to a third party (e.g., dependency) repo) or which have
`:primary t`, an overriding flag that allows projects to provide
recipes on behalf of another package, necessary in rare cases.
Pointers point to a different repo, where, typically, primary recipes
for that repo may be discovered."
  (not
   (elacarte--primary-recipe-p recipe
                               normalized-pointer)))

(defun elacarte--get-recipes (pointer &optional criteria)
  "Get the recipes at the repo referenced in the POINTER recipe.

CRITERIA is a predicate to use to filter the recipes. It will be
called with each recipe as the first argument and the normalized
POINTER recipe as the second argument."
  (let* ((criteria (or criteria (lambda (_r _p) t)))
         (normalized-pointer (elacarte--clean-room-install pointer))
         (recipes-file (elacarte--recipes-file normalized-pointer))
         (recipes (when recipes-file (elacarte--read-data recipes-file))))
    (seq-filter (lambda (r)
                  (funcall criteria
                           r
                           normalized-pointer))
                recipes)))

(defun elacarte-get-primary-recipes (pointer)
  "Get the primary recipes at the repo referenced in the POINTER recipe."
  (elacarte--get-recipes pointer
                         #'elacarte--primary-recipe-p))

(defun elacarte-get-pointer-recipes (pointer)
  "Get the pointer recipes at the repo referenced in the POINTER recipe."
  (elacarte--get-recipes pointer
                         #'elacarte--pointer-recipe-p))

(defun elacarte--repo-name (normalized-recipe)
  "Get the unique repo name of the NORMALIZED-RECIPE."
  (plist-get normalized-recipe :local-repo))

(defun elacarte--package-name (recipe)
  "Get the package name of the RECIPE."
  (symbol-name (car recipe)))

(defun elacarte--recipes-file (recipe)
  "Get the recipe file of the RECIPE."
  (plist-get recipe :recipes))

(defun elacarte--primary-override-p (recipe)
  "Is RECIPE a primary override?

This means a recipe in a downstream repo that presumes to provide a
primary recipe for an upstream repo because it won't be found upstream
for some reason. This should be a last resort for the downstream
repo, as cannot be responsible for the accuracy of third party
recipes."
  (plist-get recipe :primary))

(defun elacarte--traverse-recipes-file (recipes-file
                                        normalized-pointer
                                        replace
                                        noconfirm
                                        notraverse
                                        visited-repos)
  "Recursively add recipes starting from RECIPE-FILE, prompting unless NOCONFIRM.
This function implements the core logic to traverse and add recipes,
which distinguishes two types of recipe:
  1. Primary recipes, which refer to the source repo itself
  2. Pointer recipes, which refer to dependencies in other repos

Each repo is only responsible for providing correct and complete
recipes for *primary* recipes. The secondary, pointer recipes are only
consulted to discover the location of the next repo whose advertised
recipes will be traversed. Of course, these upstream repos are
similarly responsible for *their* primary recipes, which will be added
when they are encountered there.

If the dependency repo does not advertise its own recipes, then the
pointer recipe could include a `:primary t` field to override the
traversal and have Elacarte treat it as a primary recipe. This should
be used rarely, and it would be better for the dependency repo to
advertise its recipes (consider submitted a pull request or issue).

CURRENT-REPO-NAME is the :local-repo string of the repository we are
currently scanning, which uniquely identifies the repository for our
purposes.
REPLACE is passed to `elacarte-add-recipe'.
VISITED-REPOS is a hash-table to track processed packages."
  (let ((recipes (elacarte--read-data recipes-file)))
    (message "Found recipes in '%s', traversing..."
             (elacarte--repo-name normalized-pointer))
    ;; Pass the repo-name we just found as the "current"
    ;; repo name for the next step.
    (when (or noconfirm
              (y-or-n-p
               (format "The following %d recipes will be added. Proceed?\n%s"
                       (length recipes)
                       (mapconcat #'identity (mapcar #'elacarte--package-name recipes) ", "))))
      (dolist (recipe recipes)
        ;; We must run a "clean room" check for *every* recipe
        ;; to find out what its :local-repo is.
        ;; Compare the recipe's repo with the repo we are currently in.
        (if (elacarte--primary-recipe-p recipe
                                        normalized-pointer)
            (progn
              (message "  -> Adding primary recipe for '%s'"
                       (elacarte--package-name recipe))
              (elacarte-add-recipe recipe replace :auto))
          (unless notraverse
            (message "  -> Found pointer recipe for '%s', traversing..."
                     (elacarte--package-name recipe))
            (elacarte--follow-pointer recipe replace noconfirm notraverse visited-repos))))
      ;; --- RECURSION STOP CONDITION 3: COMPLETED TRAVERSAL OF RECIPES FILE ---
      (message "Successfully processed %d recipes." (length recipes)))))

(defun elacarte--follow-pointer (pointer replace noconfirm notraverse visited-repos)
  "A helper to clone the repo in POINTER and add its recipes.
This function is the recursive part of `elacarte-discover-recipes'.
REPLACE, NOCONFIRM, and VISITED-REPOS are passed down the
recursive chain.

POINTER itself, a recipe, need not be a complete recipe, just,
sufficient to locate the source repository where its advertised
recipes may be discovered."
  ;; 1. Use straight.el to ensure the package repo is cloned
  ;; in a "clean room" environment, and obtain the "normalized" recipe
  ;; for that specific installation.
  (let* ((normalized-pointer (elacarte--clean-room-install pointer))
         ;; We get the *actual* :local-repo name from this recipe.
         ;; This may be different from the package name.
         ;; This is our unique repository identifier that we use
         ;; to track "visited" repos during recipe discovery
         (repo-name (elacarte--repo-name normalized-pointer))
         (recipes-file (elacarte--recipes-file normalized-pointer)))
    (if (gethash repo-name visited-repos)
        ;; --- RECURSION STOP CONDITION 1: REPO ALREADY VISITED ---
        (message "Repository '%s' already traversed. Skipping." repo-name)
      ;; 1. Mark this repo as visited.
      (puthash repo-name t visited-repos)

      ;; 2. Check for the recipes.eld file.
      (if (file-exists-p recipes-file)
          ;; 3. Found a recipes file. Read it and continue the recursion.
          (elacarte--traverse-recipes-file recipes-file
                                           normalized-pointer
                                           replace
                                           noconfirm
                                           notraverse
                                           visited-repos)
        ;; --- RECURSION STOP CONDITION 2: NO RECIPES FILE ---
        (message "No '%s' file found in '%s'. Stopping traversal."
                 elacarte-recipes-filename repo-name)))))

(defun elacarte-discover-recipes (pointer &optional replace noconfirm notraverse)
  "Clone a package from POINTER and recursively add its advertised recipes.
POINTER is a `straight.el`-style recipe. This function will
clone the repository specified in the recipe, read its
`recipes.eld` file, add the recipes found within, and then
recursively do the same for all recipes found therein.

POINTER itself is only consulted to discover *where* to find valid
package recipes. It is not itself added to `elacarte-recipes-file'.
Therefore, only fields like `:host` and `:repo`, etc., are required,
and it need not be a complete recipe for a package. The pointed-to
repo is expected to advertise those in its `recipes.eld`.

This function is idempotent: it will not re-clone a repository
that is already installed.

Prompts for confirmation before adding unless NOCONFIRM is non-nil.
If REPLACE is non-nil (or with a prefix argument
interactively), existing recipes will be overwritten."
  (interactive (list (car (read-from-string (read-from-minibuffer "Enter recipe: ")))
                     current-prefix-arg
                     ;; `noconfirm` is nil when interactive
                     nil
                     ;; `notraverse` is nil when interactive
                     nil))
  (message "Discovering recipes for %S" pointer)
  ;; 1. Create a new, empty hash table to track visited repositories
  ;;    for this session.
  (let ((visited-repos (make-hash-table :test 'equal)))
    ;; 2. Start the traversal of recipes to discover
    ;;    all relevant recipes. We do not add the initial recipe
    ;;    to the master list, as it's just a pointer.
    (elacarte--follow-pointer pointer replace noconfirm notraverse visited-repos)

    ;; 3. Clean up the temporary clone directory *after* the
    ;;    entire recursive process is complete.
    (elacarte--cleanup-temp-dir)))

(defun elacarte--pointer-recipe-for-url (url)
  "Generate a pointer recipe to URL."
  (let* ((normalized-url (if (or (string-prefix-p "/" url) (string-prefix-p "~" url))
                             ;; For local paths, expand and remove trailing slashes.
                             (directory-file-name (expand-file-name url))
                           ;; For remote URLs, just use as-is.
                           url))
         (basename (file-name-nondirectory (file-name-sans-extension normalized-url)))
         (repo-name (intern basename)))
    `(,repo-name :repo ,url)))

(defun elacarte-discover-recipes-by-url (url &optional replace noconfirm notraverse)
  "Clone git repository from URL and add its advertised recipes.
This is a convenience wrapper around `elacarte-discover-recipes'.

URL can be a remote URL or a local file path (including `~/`).
It attempts to create a simple recipe from the URL.

Prompts for confirmation before adding unless NOCONFIRM is non-nil.
If REPLACE is non-nil (or with a prefix argument
interactively), existing recipes will be overwritten."
  (interactive (list (read-string "Repository URL or local path: ")
                     current-prefix-arg
                     ;; noconfirm and notraverse are nil
                     ;; when interactive
                     nil
                     nil))
  ;; Create a minimal recipe to pass to the main function.
  (elacarte-discover-recipes (elacarte--pointer-recipe-for-url url)
                             replace
                             noconfirm
                             notraverse))

(defun elacarte-build-recipe-repository (&optional repo-name)
  "Create a local recipe repository serving recipes from `elacarte-recipes-file'.
This simply generates the necessary protocol implementation file in
the `elacarte-base-dir' that serves recipes from the master recipe list.
If REPO-NAME is nil, defaults to `elacarte-repo-name'."
  (interactive)
  (let* ((repo-name (or repo-name elacarte-repo-name))
         (protocol-file (expand-file-name (concat repo-name ".el")
                                          elacarte-base-dir)))
    (unless (file-exists-p elacarte-recipes-file)
      (user-error "Recipes file not found: %s" elacarte-recipes-file))

    (message "Building '%s' recipe repository..." repo-name)

    (let* ((template-file (expand-file-name "recipe-repo.el.template"
                                            (file-name-directory (locate-library "elacarte"))))
           (template-content (elacarte--get-content-from-disk template-file))
           (protocol-content (format template-content
                                     repo-name
                                     elacarte-recipes-file)))
      (elacarte--write protocol-file
                       protocol-content))

    (let ((recipes (elacarte--read-data elacarte-recipes-file)))
      (message "Successfully built recipe repository %s serving %d recipes from %s"
               protocol-file
               (length recipes)
               elacarte-recipes-file))))

(defun elacarte-register-recipe-repository (&optional repo-name)
  "Register the local recipe repository REPO-NAME with straight.el.
This function assumes the repository has already been built with
`elacarte-build-recipe-repository'. It performs the three steps
necessary to make the repository known to the current Emacs session.
If REPO-NAME is nil, defaults to `elacarte-repo-name'.
Interactively, also uses the value of `elacarte-repo-name'."
  (interactive (list elacarte-repo-name))
  (let* ((repo-name (or repo-name elacarte-repo-name))
         (protocol-file (expand-file-name (concat repo-name ".el")
                                          elacarte-base-dir)))
    (when (file-exists-p protocol-file)
      (message "--- Registering '%s' recipe repository ---" repo-name)

      ;; 1. Make the package known to straight.el. `:build nil` is crucial.
      (straight-use-package
       `(,(intern repo-name) :type nil :local-repo ,elacarte-base-dir :build nil))

      ;; 2. Load the recipe protocol implementation.
      (add-to-list 'load-path elacarte-base-dir)
      (require (intern repo-name))

      ;; 3. Add to the head of the list of repositories to search.
      (add-to-list 'straight-recipe-repositories (intern repo-name))

      (message "--- '%s' registration complete ---" repo-name))))

(defun elacarte-update-recipe (package-name)
  "Update the recipe for PACKAGE-NAME from its source repo.
This function is intended to be run from the
`straight-use-package-pre-build-functions' hook."
  ;; Note: for recipes that have :local-repo pointing to a local directory,
  ;; its recipes will not be considered primary recipes as the repo name
  ;; derived for those recipes would be something canonical whereas
  ;; the repo name of the pointer would be the manually specified one.
  ;; Effectively, such repos have :auto nil - which is what we'd want
  (message "Elacarte: Checking for recipe updates for '%s'..." package-name)

  ;; 1. Get the recipe from the *current* Emacs session (not a clean room).
  (let* ((pointer (gethash package-name straight--recipe-cache))
         (pointer (and pointer (cons (intern package-name) pointer))))
    (if pointer
        ;; 2. Add all primary recipes (don't traverse) from the repo's recipes.eld
        (progn
          ;; It appears that Straight will not reinstall if
          ;; :local-repo is non-nil, which is great, as we can
          ;; just use the actual recipe here as a pointer
          (dolist (r (elacarte-get-primary-recipes pointer))
            (message "Adding recipe for %s" (elacarte--package-name r))
            (elacarte-add-recipe r nil :auto))
          ;; 3. Clean up the temporary clone directory
          (elacarte--cleanup-temp-dir))
      (warn "elacarte-update-recipe: No recipe found for '%s'" package-name))))

(defun elacarte-activate ()
  "Hook into Straight, ensuring that recipes are updated before packages are built."
  (add-hook 'straight-use-package-pre-build-functions
            #'elacarte-update-recipe))

(defun elacarte-deactivate ()
  "Hook into Straight, ensuring that recipes are updated before packages are built."
  (remove-hook 'straight-use-package-pre-build-functions
               #'elacarte-update-recipe))


(provide 'elacarte)

;;; elacarte.el ends here
