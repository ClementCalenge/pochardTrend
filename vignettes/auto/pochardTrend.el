(TeX-add-style-hook
 "pochardTrend"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("hyperref" "colorlinks=true" "linkcolor=blue" "urlcolor=blue" "citecolor=blue")))
   (add-to-list 'LaTeX-verbatim-environments-local "VerbatimOut")
   (add-to-list 'LaTeX-verbatim-environments-local "SaveVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb*")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "fancyvrb"
    "color"
    "url"
    "amsfonts"
    "epsfig"
    "hyperref"
    "longtable"
    "ucs"
    "savesym"
    "amsmath"
    "rotating"
    "appendix")
   (TeX-add-symbols
    '("INT" 1))
   (LaTeX-add-labels
    "sec:descr"
    "sec:datasets"
    "sec:model"
    "sec:model-fit-model"
    "sec:model-fit-1"
    "sec:model-check"
    "sec:gauss-distr-rand"
    "sec:absence-autoc"
    "sec:model-interpretation"
    "sec:estim-popul-decl"
    "sec:estim-var-param")
   (LaTeX-add-environments
    "Default")
   (LaTeX-add-lengths
    "defaultparindent"))
 :latex)

