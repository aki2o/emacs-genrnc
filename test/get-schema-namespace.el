(require 'genrnc)
(require 'el-expectations)

(expectations
  (desc "get-schema-namespace 1")
  (expect '("m" . "http://www.w3.org/1998/Math/MathML")
    (stub genrnc--get-namespace-alist-in-schema-file => '(("m" . "http://www.w3.org/1998/Math/MathML")))
    (genrnc--get-schema-namespace "1"))
  (desc "get-schema-namespace 2")
  (expect '("" . "http://www.springframework.org/schema/hadoop")
    (stub genrnc--get-namespace-alist-in-schema-file => '(("" . "http://www.springframework.org/schema/hadoop")
                                                          ("beans" . "http://www.springframework.org/schema/beans")
                                                          ("tool" . "http://www.springframework.org/schema/tool")
                                                          ("xsi" . "http://www.w3.org/2001/XMLSchema-instance")))
    (genrnc--get-schema-namespace "1")))

