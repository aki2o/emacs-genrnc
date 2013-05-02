(require 'genrnc)
(require 'tenv)
(require 'auto-complete-nxml)
(require 'el-expectations)

(expectations
  (desc "genrnc system xsd url")
  (expect '("abs" "and" "annotation" "annotation-xml" "apply" "approx" "arccos" "arccosh" "arccot"
            "arccoth" "arccsc" "arccsch" "arcsec" "arcsech" "arcsin" "arcsinh" "arctan" "arctanh"
            "arg" "bind" "bvar" "card" "cartesianproduct" "cbytes" "ceiling" "cerror" "ci" "cn"
            "codomain" "complexes" "compose" "condition" "conjugate" "cos" "cosh" "cot" "coth" "cs"
            "csc" "csch" "csymbol" "curl" "declare" "degree" "determinant" "diff" "divergence" "divide"
            "domain" "domainofapplication" "emptyset" "eq" "equivalent" "eulergamma" "exists" "exp"
            "exponentiale" "factorial" "factorof" "false" "floor" "fn" "forall" "gcd" "geq" "grad"
            "gt" "ident" "image" "imaginary" "imaginaryi" "implies" "in" "infinity" "int" "integers"
            "intersect" "interval" "inverse" "lambda" "laplacian" "lcm" "leq" "limit" "list" "ln" 
            "log" "logbase" "lowlimit" "lt" "maction" "maligngroup" "malignmark" "math" "matrix" 
            "matrixrow" "max" "mean" "median" "menclose" "merror" "mfenced" "mfrac" "mglyph" "mi" 
            "min" "minus" "mlabeledtr" "mlongdiv" "mmultiscripts" "mn" "mo" "mode" "moment" "momentabout" 
            "mover" "mpadded" "mphantom" "mprescripts" "mroot" "mrow" "ms" "mscarries" "mscarry" 
            "msgroup" "msline" "mspace" "msqrt" "msrow" "mstack" "mstyle" "msub" "msubsup" "msup" 
            "mtable" "mtd" "mtext" "mtr" "munder" "munderover" "naturalnumbers" "neq" "none" "not" 
            "notanumber" "notin" "notprsubset" "notsubset" "or" "otherwise" "outerproduct" "partialdiff" 
            "pi" "piece" "piecewise" "plus" "power" "primes" "product" "prsubset" "quotient" "rationals" 
            "real" "reals" "reln" "rem" "root" "scalarproduct" "sdev" "sec" "sech" "selector" "sep" 
            "set" "setdiff" "share" "sin" "sinh" "subset" "sum" "tan" "tanh" "tendsto" "times" "transpose" 
            "true" "union" "uplimit" "variance" "vector" "vectorproduct" "xor")
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (genrnc-user-schemas-directory tdir)
           (tfile (tenv-get-tmp-file "genrnc_system" "test.html" t t))
           (cnt 0))
      (genrnc--init-schema-directory t)
      (stub genrnc--read-typeid => "MathML3")
      (stub y-or-n-p => t)
      (genrnc-clear-cache)
      (genrnc--log-enable-logging)
      (genrnc--log-clear-log)
      (genrnc-regist-url "http://www.w3.org/Math/XMLSchema/mathml3/mathml3.xsd")
      (while (and (< cnt 60)
                  (not (with-current-buffer " *log4e-genrnc*"
                         (save-excursion
                           (goto-char (point-min))
                           (re-search-forward
                            "finish generate 'http://www.w3.org/Math/XMLSchema/mathml3/mathml3.xsd'. state:" nil t)))))
        (incf cnt)
        (sleep-for 1))
      (with-current-buffer (find-file-noselect tfile)
        (rng-set-document-type-and-validate "MathML3")
        (erase-buffer)
        (goto-char (point-min))
        (insert "<")
        (auto-complete-nxml-get-candidates)))))

