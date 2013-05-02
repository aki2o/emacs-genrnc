(require 'genrnc)
(require 'el-expectations)

(expectations
  (desc "regist-file")
  (expect (mock (genrnc--generate (expand-file-name "./schema/tool") "xsd"))
    (stub genrnc--load-index-count => nil)
    (stub genrnc--get-schema-type => "xsd")
    (stub genrnc--update-namespace-files => nil)
    (genrnc-regist-file "./schema/tool")))

