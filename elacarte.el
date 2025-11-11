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
         ;; recipes added by elacarte are managed automatically
         ;; we mark these out in the recipes file to distinguish
         ;; them from bespoke recipes added by the user
         (package-name (car recipe))
         (existing-recipes nil)
         (old-recipe nil)
         (old-auto nil))

    (when auto
      (setq recipe
            (cons (car recipe)
                  (plist-put (cdr recipe) :auto t))))

    ;; 1. Read existing recipes, creating the file if it's missing.
    (unless (file-exists-p elacarte-recipes-file)
      (make-directory (file-name-directory elacarte-recipes-file) t)
      (elacarte--write elacarte-recipes-file
                       "()"))

    (setq existing-recipes (elacarte--read-data elacarte-recipes-file))

    ;; Check if a recipe for this package already exists.
    (setq old-recipe (assoc package-name existing-recipes))
    (setq old-auto (plist-get (cdr old-recipe) :auto))

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
               (file-name-nondirectory elacarte-recipes-file)))))

(defun elacarte--install-clean-room (recipe)
  "Install RECIPE in a clean room environment.

Return the normalized recipe that contains details of the actual
installation such as the location of the :local-repo."
  ;; We create a true "clean room" by let-binding the base-dir
  ;; and all of straight.el's in-memory caches.
  (let ((package-name (symbol-name (car recipe)))
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
    (gethash package-name straight--recipe-cache)))

(defun elacarte-discover-recipes-from-file (recipes-file current-repo-name replace noconfirm notraverse visited-repos)
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

CURRENT-REPO-NAME is the :local-repo string of the repository we are
currently scanning, which uniquely identifies the repository for our
purposes.
REPLACE is passed to `elacarte-add-recipe'.
VISITED-REPOS is a hash-table to track processed packages."
  (let* ((content (elacarte--get-content-from-disk recipes-file))
         (recipes (car (read-from-string content))))
    (message "Found recipes in '%s', traversing..." current-repo-name)
    ;; Pass the local-repo-name we just found as the "current"
    ;; repo name for the next step.
    (let ((package-names (mapcar #'car recipes)))
      (when (or noconfirm
                (y-or-n-p
                 (format "The following %d recipes will be added. Proceed?\n%s"
                         (length recipes)
                         (mapconcat #'symbol-name package-names ", "))))
        (dolist (recipe recipes)
          (let ((package-name (symbol-name (car recipe))))
            ;; We must run a "clean room" check for *every* recipe
            ;; to find out what its :local-repo is.
            (let* ((normalized-recipe (elacarte--install-clean-room recipe))
                   (recipe-local-repo (plist-get normalized-recipe :local-repo)))
              ;; Compare the recipe's repo with the repo we are currently in.
              (if (equal recipe-local-repo current-repo-name)
                  (progn
                    (message "  -> Adding primary recipe for '%s'" package-name)
                    (elacarte-add-recipe recipe replace :auto))
                (unless notraverse
                  (message "  -> Found pointer recipe for '%s', traversing..." package-name)
                  (elacarte--discover-recipes recipe replace noconfirm notraverse visited-repos))))))
        ;; --- RECURSION STOP CONDITION 3: COMPLETED TRAVERSAL OF RECIPES FILE ---
        (message "Successfully processed %d recipes." (length recipes))))))

(defun elacarte--discover-recipes (recipe replace noconfirm notraverse visited-repos)
  "A helper to clone the repo in RECIPE and add its recipes.
This function is the recursive part of `elacarte-discover-recipes'.
REPLACE, NOCONFIRM, and VISITED-REPOS are passed down the
recursive chain.

RECIPE itself is considered to be only a pointer - it need not be a
complete recipe, just, sufficient to locate the source repository
where its advertised recipes may be discovered."
  ;; 1. Use straight.el to ensure the package repo is cloned
  ;; in a "clean room" environment, and obtain the "normalized" recipe
  ;; for that specific installation.
  (let* ((normalized-recipe (elacarte--install-clean-room recipe))
         ;; We get the *actual* :local-repo name from this recipe.
         ;; This may be different from the package name.
         ;; This is our unique repository identifier that we use
         ;; to track "visited" repos during recipe discovery
         (local-repo-name (plist-get normalized-recipe :local-repo))
         (repo-path (straight--repos-dir local-repo-name))
         (recipes-file (expand-file-name elacarte-recipes-filename repo-path)))
    (if (gethash local-repo-name visited-repos)
        ;; --- RECURSION STOP CONDITION 1: REPO ALREADY VISITED ---
        (message "Repository '%s' already traversed. Skipping." local-repo-name)
      ;; 1. Mark this repo as visited.
      (puthash local-repo-name t visited-repos)

      ;; 2. Check for the recipes.eld file.
      (if (file-exists-p recipes-file)
          ;; 3. Found a recipes file. Read it and continue the recursion.
          (elacarte-discover-recipes-from-file recipes-file
                                               local-repo-name
                                               replace
                                               noconfirm
                                               notraverse
                                               visited-repos)
        ;; --- RECURSION STOP CONDITION 2: NO RECIPES FILE ---
        (message "No '%s' file found in '%s'. Stopping traversal."
                 elacarte-recipes-filename local-repo-name)))))

(defun elacarte-discover-recipes (recipe &optional replace noconfirm notraverse)
  "Clone a package from RECIPE and recursively add its advertised recipes.
RECIPE is a `straight.el`-style recipe. This function will
clone the repository specified in the recipe, read its
`recipes.eld` file, add the recipes found within, and then
recursively do the same for all recipes found therein.

RECIPE itself is treated as a *pointer* and is only consulted to
discover *where* to find valid package recipes. It is not itself added
to `elacarte-recipes-file'. Therefore, only fields like `:host` and
`:repo`, etc., are required, and it need not be a complete recipe for
a package. The pointed-to repo is expected to advertise those in its
`recipes.eld`.

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
  (message "Discovering recipes for %S" recipe)
  ;; 1. Create a new, empty hash table to track visited repositories
  ;;    for this session.
  (let ((visited-repos (make-hash-table :test 'equal)))
    ;; 2. Start the traversal of recipes to discover
    ;;    all relevant recipes. We do not add the initial recipe
    ;;    to the master list, as it's just a pointer.
    (elacarte--discover-recipes recipe replace noconfirm notraverse visited-repos)

    ;; 3. Clean up the temporary clone directory *after* the
    ;;    entire recursive process is complete.
    (when (file-directory-p elacarte-temp-dir)
      (delete-directory elacarte-temp-dir t)
      (message "Cleaned up temporary repositories."))))

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
  (let* ((normalized-url (if (or (string-prefix-p "/" url) (string-prefix-p "~" url))
                             ;; For local paths, expand and remove trailing slashes.
                             (directory-file-name (expand-file-name url))
                           ;; For remote URLs, just use as-is.
                           url))
         (basename (file-name-nondirectory (file-name-sans-extension normalized-url)))
         (repo-name (intern basename))
         ;; Create a minimal recipe to pass to the main function.
         (recipe `(,repo-name :repo ,url)))
    (elacarte-discover-recipes recipe replace noconfirm notraverse)))

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
           (template-content (elacarte--get-content-from-disk template-file))
           (protocol-content (format template-content repo-name)))
      (elacarte--write protocol-file
                       protocol-content))

    ;; 3. Read the master list of recipes.
    (setq recipes (elacarte--read-data elacarte-recipes-file))

    ;; 4. Write each recipe to its own file.
    (dolist (recipe recipes)
      (let* ((recipe-id (car recipe))
             (package-name (if (symbolp recipe-id) (symbol-name recipe-id) recipe-id))
             (target-file (expand-file-name package-name recipes-dir)))
        (elacarte--write target-file
                         (prin1-to-string recipe))))

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

(defun elacarte-update-recipe (package-name)
  "Update the recipe for PACKAGE-NAME from its source repo.
This function is intended to be run from the
`straight-use-package-pre-build-functions' hook."
  (message "Elacarte: Checking for recipe updates for '%s'..." package-name)

  ;; 1. Get the recipe from the *current* Emacs session (not a clean room).
  (let ((recipe (gethash package-name straight--recipe-cache)))
    (if recipe
        ;; 2. Get the *true* :local-repo name from that recipe.
        (let* ((local-repo-name (plist-get recipe :local-repo))
               (repo-path (straight--repos-dir local-repo-name))
               (recipes-file (expand-file-name elacarte-recipes-filename repo-path)))
          ;; 3. Add all primary recipes (don't traverse) from the repo's recipes.eld
          (elacarte-discover-recipes-from-file recipes-file
                                               current-repo-name
                                               nil  ; don't replace bespoke recipes
                                               :noconfirm ; don't ask for confirmation
                                               ;; and, mainly, don't traverse into pointer recipes
                                               :notraverse
                                               ;; this won't be used anyway
                                               (make-hash-table :test 'equal)))
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
