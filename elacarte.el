;;; elacarte.el --- A la carte Emacs package recipes. -*- lexical-binding: t -*-

;; Author: Sid Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/elacarte
;; Version: 0.2
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

(defvar elacarte-cookbooks-dir
  (expand-file-name "cookbooks" elacarte-base-dir)
  "The path where cookbooks are kept.")

(defvar elacarte-elpa-dir
  (expand-file-name ".elpa" elacarte-base-dir)
  "The path where recipe repositories are generated.

These generated files implement Straight.el's recipe repository
protocol to serve recipes from cookbooks.")

(defvar elacarte-temp-dir
  (expand-file-name "tmp" elacarte-base-dir)
  "Temporary directory for `elacarte` operations, like cloning repos.")

(defconst elacarte-recipes-filename "recipes.eld"
  "The conventional file advertising project recipes.

Any project would advertise recipes using this filename at the top
level of the source repository, and Elacarte would allow users to
conveniently install those packages.")

(defconst elacarte-cookbook-default-filename "my-cookbook.eld"
  "The default filename to use for your cookbook.")

(defgroup elacarte nil
  "Authoritative Emacs package recipes."
  :group 'applications)

(defcustom elacarte-cookbook
  (expand-file-name elacarte-cookbook-default-filename
                    elacarte-cookbooks-dir)
  "The master file containing the list of local recipes.

This file houses your curated and preferred recipes to be used by
Emacs for finding and building packages. These recipes are
\"authoritative\" in the sense that they are either discovered from
the package source (e.g., Git repo) maintained by the authors
themselves, or explicitly overridden by you."
  :type 'string
  :group 'elacarte)

(defun elacarte--recipe-repository-name (&optional cookbook)
  "The name of the recipe repository for COOKBOOK"
  (let ((cookbook (or cookbook elacarte-cookbook)))
    (concat "elacarte-"
            (file-name-base cookbook))))

(defun elacarte--get-content-from-disk (file-path)
  "Return content of FILE-PATH as a string."
  (unless (file-exists-p file-path)
    (user-error "File not found: %s" file-path))
  (with-temp-buffer
    (insert-file-contents-literally file-path)
    (buffer-string)))

(defun elacarte--read (file)
  "Read a Lisp datum from FILE."
  (car
   (read-from-string
    (elacarte--get-content-from-disk file))))

(defun elacarte--write (file string)
  "Write STRING to FILE."
  (let ((dir (file-name-directory file)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (with-temp-file file
    (insert string)))

(defun elacarte--pretty-print (obj)
  "A readable string representation of OBJ."
  (let ((print-level nil) (print-length nil)) ; Ensure full printing.
    (pp-to-string obj)))

(defun elacarte--make-cookbook (cookbook)
  "Create a COOKBOOK.

If the file doesn't already exist, create an empty cookbook, i.e., one
containing no recipes (but still formatted as a cookbook).

If the file already exists, then do nothing."
  (unless (file-exists-p cookbook)
    (elacarte--write cookbook
                     "()")))

(defun elacarte--remove-recipe (package-name recipes)
  "Remove PACKAGE-NAME from RECIPES."
  (cl-remove-if
   (lambda (r) (equal (car r) package-name))
   recipes))

(defun elacarte-remove-recipe-from-cookbook (package-name cookbook &optional noconfirm)
  "Remove recipe for PACKAGE-NAME from COOKBOOK."
  (let* ((existing-recipes (elacarte--read cookbook))
         (recipe-to-remove (assoc package-name existing-recipes)))
    (unless recipe-to-remove
      (user-error "No recipe found for package '%s'" package-name))

    (when (or noconfirm
              (y-or-n-p (format "Really remove recipe for '%s'?" package-name)))
      (let ((updated-recipes (elacarte--remove-recipe package-name
                                                      existing-recipes)))
        (elacarte--write cookbook
                         (elacarte--pretty-print updated-recipes))
        (message "Recipe for '%s' removed." package-name)))))

(defun elacarte-remove-recipe (package-name &optional noconfirm)
  "Remove the recipe for PACKAGE-NAME from `elacarte-cookbook'."
  (interactive
   (let ((recipes (elacarte--read elacarte-cookbook)))
     (list (intern (completing-read "Remove recipe for package: "
                                    (mapcar #'car recipes))))))
  (elacarte-remove-recipe-from-cookbook package-name
                                        elacarte-cookbook
                                        noconfirm))

(defun elacarte-add-recipe-to-cookbook (recipe cookbook &optional replace auto)
  "Add or update RECIPE in COOKBOOK."
  (let* ((package-name (car recipe))
         ;; recipes added by elacarte are managed automatically
         ;; we mark these out in the recipes file to distinguish
         ;; them from bespoke recipes added by the user
         (recipe (if auto
                     (cons package-name
                           (plist-put (cdr recipe) :auto t))
                   recipe)))

    ;; 1. Read existing recipes, creating the file if it's missing.
    (elacarte--make-cookbook cookbook)

    ;; Check if a recipe for this package already exists.
    (let* ((existing-recipes (elacarte--read cookbook))
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
          (elacarte--write cookbook
                           (elacarte--pretty-print
                            (cons recipe updated-recipes))))

        (message "Recipe for '%s' %s %s"
                 package-name
                 (if old-recipe "updated in" "added to")
                 (file-name-nondirectory cookbook))))))

(defun elacarte-add-recipe (recipe &optional replace auto)
  "Add or update RECIPE in `elacarte-cookbook'.

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
  ;; When called interactively, RECIPE is a string. We must parse it.
  ;; `read-from-string` returns (OBJECT . CHARS-READ), so we take the CAR.
  (let ((recipe (if (stringp recipe) (car (read-from-string recipe)) recipe)))
    (elacarte-add-recipe-to-cookbook recipe
                                     elacarte-cookbook
                                     replace
                                     auto)))

(defun elacarte-clean-room-install (recipe)
  "Install RECIPE in a clean room environment.

Return the normalized recipe that contains details of the actual
installation such as the location of the :local-repo."
  ;; We create a true "clean room" by let-binding the base-dir
  ;; and all of straight.el's in-memory caches.
  ;;
  ;; Note: At first it seems it would be nice if we could clean up the
  ;; temp path at the end of this function, so that it is fully
  ;; self-contained (currently we rely on high level public APIs such
  ;; as `elacarte-discover-recipes' to do this cleanup). All relevant
  ;; information for the clean room installation that the caller may
  ;; be interested in could be encoded in the returned normalized
  ;; recipe, so that the operation is "functional" and the on-disk
  ;; changes need not be retained outside the scope of this
  ;; function. As we do not expect repos to be visited more than once
  ;; during recipe discovery, retaining the disk changes to gain by
  ;; the "idempotent" property of the clean room installation should
  ;; not make any practical difference (as the recipes would not be
  ;; installed more than once, anyway).
  ;; Unfortunately, that's not quite the case, since during recipe
  ;; discovery, each repo is in fact cloned at least twice. The first
  ;; time, it is cloned via the pointer to it. Subsequently, every
  ;; recipe it contains (including at least one that would point to
  ;; itself) requires a clean room installation in order to ascertain
  ;; whether it is primary or secondary. It is only that the repo is
  ;; not visited more than once *via pointer*.
  ;; If we wanted to make the operation of this function more
  ;; functional, some options could be:
  ;;   1. Change the algorithm for determining whether a recipe is
  ;;      primary vs secondary so that cloning the repo isn't
  ;;      necessary...
  ;;   2. Follow pointer, identify its primary and pointer recipes,
  ;;      add primary recipes, and only then traverse pointers,
  ;;      leveraging the context of having just followed a pointer (in
  ;;      some way) to determine primary vs secondary efficiently.
  (let ((package-name (car recipe))
        (package-name-str (elacarte--package-name recipe))
        (straight-base-dir elacarte-temp-dir)
        (straight-allow-recipe-inheritance nil)
        (straight--success-cache (make-hash-table :test 'equal))
        (straight--recipe-cache (make-hash-table :test 'equal))
        (straight--repo-cache (make-hash-table :test 'equal))
        (straight--profile-cache (make-hash-table :test 'equal)))
    (message "Ensuring package repo for '%s' is available (clean room)..." package-name-str)
    ;; Ensure the recipe is installed (idempotent)
    ;; This call has the side effect of cloning the repo AND
    ;; registering the normalized recipe in the (temporary)
    ;; `straight--recipe-cache`.
    ;; Note that it :type is nil, this will not attempt to
    ;; do anything and will just return the normalized recipe,
    ;; which may be useful for testing.
    (straight-use-package-no-build recipe)
    ;; Return the normalized recipe that straight.el just created,
    ;; which contains details of the actual installation such as
    ;; the :local-repo for the package.
    (let* ((normalized-recipe (cons package-name
                                    (gethash package-name-str
                                             straight--recipe-cache)))
           (repo-id (elacarte--repo-id normalized-recipe))
           (recipes-file (expand-file-name elacarte-recipes-filename
                                           (straight--repos-dir repo-id))))
      (cons package-name
            (plist-put (cdr normalized-recipe)
                       :recipes recipes-file)))))

(defun elacarte--cleanup-temp-dir ()
  "Delete `elacarte-temp-dir' if present."
  (if (file-directory-p elacarte-temp-dir)
      (progn (delete-directory elacarte-temp-dir t)
             (message "Cleaned up temporary repositories."))
    (message "Nothing to clean up.")))

(defun elacarte--primary-recipe-p (recipe normalized-pointer)
  "Is RECIPE primary in relation to NORMALIZED-POINTER?

NORMALIZED-POINTER is a recipe pointing to a repository containing
Emacs packages. It is expected to be \"normalized,\" that is, it
should have a :local-repo property indicating its canonical name.

Primary recipes are those that either point to the containing repo
(and not to a third party (e.g., dependency) repo) or which have
`:primary t`, an overriding flag that allows projects to provide
recipes on behalf of another package, necessary in rare cases.
Pointers point to a different repo, where, typically, primary recipes
for that repo may be discovered."
  (let* ((repo-id (elacarte--repo-id normalized-pointer))
         (normalized-recipe (elacarte-clean-room-install recipe)))
    (equal repo-id
           (elacarte--repo-id normalized-recipe))))

(defun elacarte--pointer-recipe-p (recipe normalized-pointer)
  "Is RECIPE a pointer in relation to NORMALIZED-POINTER?

NORMALIZED-POINTER is a recipe pointing to a repository containing
Emacs packages. It is expected to be \"normalized,\" that is, it
should have a :local-repo property indicating its canonical name.

See `elacarte--primary-recipe-p' regarding primary vs pointer recipes.
Note that pointer recipes marked as primary overrides (i.e., `:primary
t`) will still be considered pointers by this function."
  (not
   (elacarte--primary-recipe-p recipe
                               normalized-pointer)))

(defun elacarte--get-recipes (pointer &optional criteria)
  "Get the recipes at the repo referenced in the POINTER recipe.

CRITERIA is a predicate to use to filter the recipes. It will be
called with each recipe as the first argument and the normalized
POINTER recipe as the second argument."
  (let* ((criteria (or criteria (lambda (_r _p) t)))
         (normalized-pointer (elacarte-clean-room-install pointer))
         (recipes-file (elacarte--recipes-file normalized-pointer))
         (recipes (when recipes-file (elacarte--read recipes-file))))
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

(defun elacarte--package-name (recipe)
  "Get the package name of the RECIPE."
  (symbol-name (car recipe)))

(defun elacarte--repo-id (normalized-recipe)
  "Get the unique repo name of the NORMALIZED-RECIPE."
  (plist-get (cdr normalized-recipe) :local-repo))

(defun elacarte--recipes-file (normalized-recipe)
  "Get the recipe file of the NORMALIZED-RECIPE."
  (plist-get (cdr normalized-recipe) :recipes))

(defun elacarte--primary-override-p (recipe)
  "Is RECIPE a primary override?

This means a recipe in a downstream repo that presumes to provide a
primary recipe for an upstream repo because it won't be found upstream
for some reason. This should be a last resort for the downstream
repo, as it cannot be responsible for the accuracy of third party
recipes."
  (plist-get (cdr recipe) :primary))

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
  (let ((recipes (elacarte--read recipes-file)))
    (message "Found recipes in '%s', traversing..."
             (elacarte--repo-id normalized-pointer))
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
        (if (or (elacarte--primary-recipe-p recipe
                                            normalized-pointer)
                (elacarte--primary-override-p recipe))
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
  (let* ((normalized-pointer (elacarte-clean-room-install pointer))
         ;; We get the *actual* :local-repo name from this recipe.
         ;; This may be different from the package name.
         ;; This is our unique repository identifier that we use
         ;; to track "visited" repos during recipe discovery
         (repo-id (elacarte--repo-id normalized-pointer))
         (recipes-file (elacarte--recipes-file normalized-pointer)))
    (if (gethash repo-id visited-repos)
        ;; --- RECURSION STOP CONDITION 1: REPO ALREADY VISITED ---
        (message "Repository '%s' already traversed. Skipping." repo-id)
      ;; 1. Mark this repo as visited.
      (puthash repo-id t visited-repos)

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
                 elacarte-recipes-filename repo-id)))))

(defun elacarte-discover-recipes (pointer &optional replace noconfirm notraverse)
  "Clone a package from POINTER and recursively add its advertised recipes.
POINTER is a `straight.el`-style recipe. This function will
clone the repository specified in the recipe, read its
`recipes.eld` file, add the recipes found within, and then
recursively do the same for all recipes found therein.

POINTER itself is only consulted to discover *where* to find valid
package recipes. It is not itself added to `elacarte-cookbook'.
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

(defun elacarte-add-recipes-in-file (recipes-file)
  "A low-level utility to add all recipes in RECIPES-FILE.

This does not do any validation or traversal, and simply adds the
recipes in the file to `elacarte-cookbook', replacing any existing
recipes for the same packages.

This should generally not be used except by tools implementing
higher-level functionality."
  (let ((recipes (elacarte--read recipes-file)))
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

(defun elacarte--pointer-recipe-for-url (url)
  "Generate a pointer recipe to URL."
  (let* ((normalized-url (if (or (string-prefix-p "/" url) (string-prefix-p "~" url))
                             ;; For local paths, expand and remove trailing slashes.
                             (directory-file-name (expand-file-name url))
                           ;; For remote URLs, just use as-is.
                           url))
         (basename (file-name-nondirectory normalized-url))
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

(defun elacarte--generate-recipe-repository (&optional cookbook)
  "Create a local recipe repository serving recipes from COOKBOOK.
This simply generates the necessary protocol implementation file in
the `elacarte-elpa-dir' that could serve recipes from the master
recipe list. It does not actually inform Straight about this file.
If COOKBOOK is nil, defaults to `elacarte-cookbook'."
  (interactive)
  (let* ((cookbook (or cookbook elacarte-cookbook))
         (repo-name (elacarte--recipe-repository-name cookbook))
         (protocol-file (expand-file-name (concat repo-name ".el")
                                          elacarte-elpa-dir)))
    (unless (file-exists-p cookbook)
      (user-error "Recipes file not found: %s" cookbook))

    (message "Building '%s' recipe repository..." repo-name)

    (let* ((template-file (expand-file-name "recipe-repo.el.template"
                                            (file-name-directory (locate-library "elacarte"))))
           (template-content (elacarte--get-content-from-disk template-file))
           (protocol-content (format template-content
                                     repo-name
                                     cookbook)))
      (elacarte--write protocol-file
                       protocol-content))

    (let ((recipes (elacarte--read cookbook)))
      (message "Successfully built recipe repository %s serving %d recipes from %s"
               protocol-file
               (length recipes)
               cookbook))))

(defun elacarte--serve-recipe-repository (&optional cookbook)
  "Register a local recipe repository for COOKBOOK with straight.el.
This function assumes the repository file has already been generated with
`elacarte--generate-recipe-repository'. It performs three steps
necessary to make the repository known to the current Emacs session:

1. It uses straight-use-package to register the recipe repository as a
package.

2. It loads the module implementing Staight's recipe protocol so that
Straight can use it to serve recipes.

3. It adds this recipe repository to the head of
`straight-recipe-repositories' so that it is prioritized over all
other recipe sources.

If COOKBOOK is nil, defaults to `elacarte-cookbook'.
Interactively, also uses the value of `elacarte-cookbook'."
  (interactive (list elacarte-cookbook))
  (let* ((cookbook (or cookbook elacarte-cookbook))
         (repo-name (elacarte--recipe-repository-name cookbook))
         (protocol-file (expand-file-name (concat repo-name ".el")
                                          elacarte-elpa-dir)))
    (when (file-exists-p protocol-file)
      (message "--- Registering '%s' recipe repository ---" repo-name)

      ;; 1. Make the package known to straight.el. `:build nil` is crucial.
      (straight-use-package
       `(,(intern repo-name) :type nil :local-repo ,elacarte-elpa-dir :build nil))

      ;; 2. Load the recipe protocol implementation.
      (add-to-list 'load-path elacarte-elpa-dir)
      (require (intern repo-name))

      ;; 3. Add to the head of the list of repositories to search.
      (add-to-list 'straight-recipe-repositories (intern repo-name))

      (message "--- '%s' registration complete ---" repo-name))))

(defun elacarte-use-cookbook (&optional cookbook)
  "Build a recipe repository for COOKBOOK and register it with Straight."
  (elacarte--make-cookbook cookbook)
  (elacarte--generate-recipe-repository cookbook)
  (elacarte--serve-recipe-repository cookbook))

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

;; TODO: synchronization is untested
(defun elacarte-enable-synchronization ()
  "Hook into Straight, ensuring that recipes are updated before packages are built."
  (add-hook 'straight-use-package-pre-build-functions
            #'elacarte-update-recipe))

(defun elacarte-disable-synchronization ()
  "Disable recipe synchronization."
  (remove-hook 'straight-use-package-pre-build-functions
               #'elacarte-update-recipe))


(provide 'elacarte)
;;; elacarte.el ends here
