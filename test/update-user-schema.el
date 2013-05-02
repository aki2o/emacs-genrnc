(require 'genrnc)
(require 'el-expectations)

(expectations
  (desc "update-user-schema")
  (expect (mock (genrnc--generate "http://www.springframework.org/schema/tool" "xsd" nil t))
    (stub genrnc--init-schema-directory => nil)
    (stub genrnc--get-index-from-typeid => nil)
    (stub genrnc--get-schema-location => "http://www.springframework.org/schema/tool")
    (stub genrnc--get-schema-type => "xsd")
    (stub genrnc--update-namespace-files => nil)
    (genrnc-update-user-schema "SpringFrameWork Tool")))

