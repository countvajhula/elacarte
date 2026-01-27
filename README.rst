========
elacarte
========

-------------------------------------------------------
*Decentralized, authoritative Emacs package cookbooks.*
-------------------------------------------------------

Elacarte is a decentralized recipe discovery engine for Emacs. It allows package authors to advertise how their packages should be built directly within their own repositories, and empowers users to aggregate these authoritative recipes into a curated, local "cookbook."

The Core Philosophy: Appropriate Authority
==========================================

The current Emacs ecosystem often relies on centralized archives to define how a package is built. Elacarte restores a natural **chain of authority**:

    **You (The User) > Package Authors > Centralized Archives**

1. **You** are the ultimate authority on what runs in your Emacs. You can override any recipe.
2. **Package authors** are the authority on how their code should be built and what it depends on.
3. **Centralized archives** are excellent for *discovery*, but they should not be the bottleneck for *distribution*.

Why Elacarte?
=============

While tools like ``straight.el`` and ``elpaca`` allow you to install from any Git repo, there is a missing link: *Who defines the recipe?*

The Problems Today
------------------

* **Fragile configs:** To install a package not on a centralized archive such as ELPA, you must write an "inline recipe" in your init file. This conflates configuration with installation, leading to fragile ordering issues in your config that are *not* addressed by Use Package's ``:after`` keyword (which affects *loading* rather than *installation*).
* **Coordination bottlenecks:** If a developer refactors their folder structure, they must submit a PR to a centralized archive and wait for approval before users can safely update.
* **Testing friction:** Code quality checks such as testing whether a package is "installable" are currently opaque, often requiring specialized third-party interactions and sandboxes instead of simple local checks.

The Elacarte Solution
---------------------

* **Local Cookbook:** Elacarte enables you to conveniently curate a local, authoritative cookbook containing recipes. This keeps your ``init.el`` focused on *configuration* (how you use the package) while Elacarte informs *installation* (how you get and build the package), consistently across your entire config.
* **Decentralized discovery:** Authors include a ``recipes.eld`` in their repo. Elacarte finds it, reads it, and handles the rest. If a package depends on another non-standard package, Elacarte follows the "pointer" to the dependency’s repo and discovers its authoritative recipe, recursively.
* **Self-contained:** Elacarte enables tools to find the information they need locally and enables you to conveniently provide that information, avoiding opaque third party interactions, powering tools like `Elci <https://github.com/countvajhula/elci>`_.

How It Works: Primary vs. Pointer
---------------------------------

Elacarte traverses a graph of repositories to build your local cookbook using two simple concepts:

1. **Primary recipes**: These describe packages contained within the current repository. *The repo is the authority for these*.

2. **Pointer recipes**: These describe dependencies hosted elsewhere. Elacarte doesn't just trust a third-party's version of a dependency's recipe; it follows the pointer to the source to find the primary recipe there.

This ensures you always get the canonical build instructions defined by the people who actually wrote the code.

Ultimate Authority
------------------

Elacarte's cookbook can be easily edited by you, and it takes precedence over canonical recipes provided by developers and community-maintained archives (like MELPA).

This gives you ultimate authority over the recipes used in your own Emacs, without necessitating any fragile workarounds such as employing inline recipes.

Technical Implementation
========================

Elacarte operates as a **pre-processor for your package manager**. It doesn't replace the build logic of ``straight.el``; instead, it enables you to curate a local, overriding, recipe repository ("cookbook") that Straight is made aware of via ``straight-recipe-repositories``.

More generally, Elacarte allows you to create and manipulate *cookbooks* which contain authoritative recipes. You can use any of them or even more than one of them in tandem as the recipe authority in your Emacs.

Cookbook
--------

Elacarte maintains a master file at ``~/.emacs.d/elacarte/cookbooks/cookbook.eld``. This file is the source of truth for your personal Emacs flavor, and it can be maintained and versioned along with the rest of your init config.

* **Auto-updates:** Canonical recipes added by "discovery" are marked with ``:auto t``. When you update a package via ``straight``, Elacarte can automatically re-scan the repo for recipe changes and update your cookbook.
* **User overrides:** You can always manually add or edit the recipes in the cookbook by editing the file directly. Without the ``:auto t`` flag, such recipes will not be automatically maintained by Elacarte.

Recipe Discovery
----------------

"Discovery" is the process by which Elacarte traverses and adds new authoritative recipes to your local cookbook.

When you run ``elacarte-discover-recipes``, the package performs a "clean room" installation:

* It creates a temporary directory (``~/.emacs.d/elacarte/tmp``), and also temporarily clears ``straight.el``’s internal caches within the scope of the discovery process, ensuring your global Emacs state remains untouched.
* It clones the target repository into the temp directory to read its ``recipes.eld``, prompting to add these recipes to your cookbook.
* It does this recursively until all primary recipes are discovered and added.

Integration with Straight.el
----------------------------

The cookbook at ``~/.emacs.d/elacarte/cookbooks/my-cookbook.eld`` is served by a corresponding auto-generated file, ``~/.emacs.d/elacarte/.elpa/elacarte-my-cookbook.el``, which implements the Straight recipe protocol. At initialization time, this local cookbook is registered and pushed to the front of ``straight-recipe-repositories``, ensuring that if a recipe exists in your Elacarte cookbook, **it takes precedence** over MELPA, GNU ELPA, or any other archive.

What about Elpaca?
------------------

Although Elacarte is implemented in Straight, it could support interfacing with either Straight or Elpaca in user config. But support for Elpaca isn't implemented yet. Help with this would be valuable and greatly appreciated.

Quick Start (for Straight.el users)
===================================

Place this config early in your Emacs init configuration, right after bootstrapping Straight and Use Package.

1. **Install Elacarte**

.. code-block:: elisp

   (use-package elacarte
     :straight
     (elacarte :host github :repo "countvajhula/elacarte")
     :custom
     (elacarte-cookbook "~/.emacs.d/elacarte/cookbooks/cookbook.eld")
     :config
     (elacarte-use-cookbook elacarte-cookbook)

This config generates a local recipe repository at ``~/.emacs.d/elacarte/cookbooks/cookbook.eld`` that ``straight.el`` can use.

2. **Discover a new package**

Point Elacarte to a repository URL for a package you are interested in (and which advertises its recipes in a ``recipes.eld`` --- e.g., `Symex.el <https://github.com/drym-org/symex.el>`_ or `Mantra <https://github.com/countvajhula/mantra>`_). It will prompt you to add all discovered recipes (including dependencies) to your local cookbook.

.. code-block:: text

   M-x elacarte-discover-recipes-by-url RET https://github.com/drym-org/symex.el RET

And that's it! Now you can configure the package in your Emacs config with ``use-package``, as usual, without inline recipes or worrying about dependencies or load order.

Customizing
-----------

By default, Elacarte APIs use the cookbook you've configured in ``elacarte-cookbook``. If you'd like to use a different cookbook, just pass the desired cookbook (as a filename, e.g., ``~/.emacs.d/elacarte/cookbooks/work-laptop.eld``) as an argument to functions that accept a cookbook argument. For the interactive APIs that don't, such as the main recipe discovery APIs, you can let-bind ``elacarte-cookbook`` to whatever cookbook you'd like to use during invocation of the API.

For Package Authors
===================

To make your package "Elacarte-ready," simply place a ``recipes.eld`` file at the root of your repository, containing a `Straight.el-compatible recipe <https://github.com/radian-software/straight.el?tab=readme-ov-file#the-recipe-format>`__ that Emacs should use to install your package (real-world examples are included below). This is the same kind as you might publish on a central archive such as MELPA. You can optionally include pointers to dependencies --- this is *required* if these dependencies are not listed on centralized archives. Your ``recipes.eld`` should contain a *list* of recipes, even if there is only one recipe. It should resemble:

.. code-block:: elisp

   ( ; recipes.eld always contains a *list* of recipes
    (my-package :host github :repo "me/my-package") ; and any special :files, etc.
    (my-dependency :host github :repo "someuser/my-dependency")
   )

Now, when a user points Elacarte to your repo, it will automatically know how to build your package and where to find its dependencies.

Note that the dependency recipe is treated as a *pointer* by Elacarte. It only needs to include enough information to be able to *find* the authoritative source, and need not be a full recipe (any non-addressing information in the recipe will simply be ignored by Elacarte, which will look for the primary recipe upstream).

If the dependency repository isn't Elacarte-ready, please consider submitting a pull request or issue to add the canonical recipe upstream. If that's not possible for some reason, you can specify that your dependency recipe is a primary override by using ``:primary t``, which will cause Elacarte to use the recipe as is and prevent further traversal. **This is strongly discouraged**, however, and should only be done as a last resort, as it goes against the principle of appropriate authority.

Sample ``recipes.eld``
----------------------

Here are some examples of projects advertising their recipes in a ``recipes.eld``, that you could follow in your own projects:

Single-package project
~~~~~~~~~~~~~~~~~~~~~~

- `Mantra <https://github.com/countvajhula/mantra/blob/main/recipes.eld>`__

Multi-package project with custom dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- `Symex <https://github.com/drym-org/symex.el/blob/main/recipes.eld>`__

*Elacarte is currently a proof-of-concept. It is intended to be used alongside package managers such as Straight (currently supported) or Elpaca (as yet unsupported) to complete the toolset of the modern Emacs user.*

Testing
-------

Manually
~~~~~~~~

Using Elacarte gives you end-to-end control over the installation and configuration of your package. You can test the validity of your install recipe simply by installing the package yourself! To do this, you could:

1. Run ``M-x elacarte-discover-recipes-by-url RET https://host.com/my/repo/url RET``.

Or, if you prefer, evaluate this expression using ``C-x C-e``.

.. code-block:: elisp

   (elacarte-discover-recipes-by-url "https://host.com/my/repo/url")

Note that the URL here can even be a local file path, like ``~/path/to/my-package``. However, keep in mind that Elacarte will still follow the instructions *inside* the recipe. If you are trying to test recipe changes prior to pushing them remotely, it is often better to simply **override** the recipe directly in your Emacs's Elacarte cookbook (e.g., by adding a temporary ``:local-repo`` entry) to verify the build logic.

2. Use your package in your own config.

.. code-block:: elisp

   (use-package my-package)

Automatically
~~~~~~~~~~~~~

Try `Elci <https://github.com/countvajhula/elci>`_. It's built using Elacarte and gives you automated and transparent code quality checks --- including for installability --- that can be run both locally as well as on hosted CI infrastructure.

Non-Ownership
=============

The freely released, copyright-free work in this repository represents an investment in a better way of doing things called attribution-based economics. Attribution-based economics is based on the simple idea that we gain more by giving more, not by holding on to things that, truly, we could only create because we, in our turn, received from others. As it turns out, an economic system based on attribution -- where those who give more are more empowered -- is significantly more efficient than capitalism while also being stable and fair (unlike capitalism, on both counts), giving it transformative power to elevate the human condition and address the problems that face us today along with a host of others that have been intractable since the beginning. You can help make this a reality by releasing your work in the same way -- freely into the public domain in the simple hope of providing value. Learn more about attribution-based economics at `drym.org <https://drym.org>`_, tell your friends, do your part.
