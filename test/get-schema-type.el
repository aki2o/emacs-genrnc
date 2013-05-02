(require 'genrnc)
(require 'el-expectations)

(expectations
  (desc "get-schema-type url xsd")
  (expect "xsd"
    (genrnc--get-schema-type "http://www.w3.org/Math/XMLSchema/mathml3/mathml3.xsd")))

(expectations
  (desc "get-schema-type url rng")
  (expect "rng"
    (genrnc--get-schema-type "http://relaxng.org/relaxng.rng")))

(expectations
  (desc "get-schema-type url rnc")
  (expect "rnc"
    (genrnc--get-schema-type "http://syntax.whattf.org/relaxng/html5.rnc")))

(expectations
  (desc "get-schema-type url dtd")
  (expect "dtd"
    (genrnc--get-schema-type "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")))

(expectations
  (desc "get-schema-type url ent")
  (expect "dtd"
    (genrnc--get-schema-type "http://www.w3.org/TR/xhtml1/DTD/xhtml-symbol.ent")))

(expectations
  (desc "get-schema-type url mod")
  (expect "dtd"
    (genrnc--get-schema-type "http://www.w3.org/TR/xhtml-modularization/DTD/xhtml-inlstyle-1.mod")))

(expectations
  (desc "get-schema-type path")
  (expect "xsd"
    (genrnc--get-schema-type "~/kml/2.2.0/ogckml22.xsd")))

(expectations
  (desc "get-schema-type icase")
  (expect "xsd"
    (genrnc--get-schema-type "~/kml/2.2.0/ogckml22.XsD")))

(expectations
  (desc "get-schema-type unknown")
  (expect (mock (completing-read "[GENRNC] Select schema type of '~/kml/2.2.0/ogckml22.txt' (xsd|rng|rnc|dtd): "
                                 '("xsd" "rng" "rnc" "dtd")
                                 nil
                                 t))
    (genrnc--get-schema-type "~/kml/2.2.0/ogckml22.txt")))

