((elacarte
  :host github
  :repo "countvajhula/elacarte"
  :type git
  :files ("*.el" "recipe-repo.el.template"))
 (straight
  :primary t  ; override upstream traversal for now - see docs in elacarte.el
  :type git
  :host github
  :repo "radian-software/straight.el"
  :files ("straight*.el")))
