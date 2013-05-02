(require 'genrnc)
(require 'el-expectations)

(expectations
  (desc "regist-url url")
  (expect (mock (genrnc--generate "http://www.springframework.org/schema/tool" "xsd"))
    (stub genrnc--load-index-count => nil)
    (stub genrnc--get-schema-type => "xsd")
    (stub genrnc--update-namespace-files => nil)
    (genrnc-regist-url "http://www.springframework.org/schema/tool")))

(expectations
  (desc "regist-url url https")
  (expect (mock (genrnc--generate "https://www.springframework.org/schema/tool" "xsd"))
    (stub genrnc--load-index-count => nil)
    (stub genrnc--get-schema-type => "xsd")
    (stub genrnc--update-namespace-files => nil)
    (genrnc-regist-url "https://www.springframework.org/schema/tool")))

(expectations
  (desc "regist-url not url")
  (expect (mock (message *))
    (stub genrnc--load-index-count => nil)
    (stub genrnc--get-schema-type => "xsd")
    (stub genrnc--update-namespace-files => nil)
    (genrnc-regist-url "/etc/schema/tool")))

