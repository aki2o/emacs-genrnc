(require 'genrnc)
(require 'el-expectations)

(expectations
  (desc "rename-user-schema remove")
  (expect (mock (genrnc--remove-schemas-defining "SpringFrameWork Tool"))
    (stub genrnc--init-schema-directory => nil)
    (stub genrnc--get-index-from-typeid => "1")
    (stub genrnc--get-schema-location => "http://www.springframework.org/schema/tool")
    (stub genrnc--read-typeid => "New Tool")
    (stub genrnc--store-schemas-defining => nil)
    (genrnc-rename-user-schema "SpringFrameWork Tool")))

(expectations
  (desc "rename-user-schema store")
  (expect (mock (genrnc--store-schemas-defining "1" "New Tool"))
    (stub genrnc--init-schema-directory => nil)
    (stub genrnc--get-index-from-typeid => "1")
    (stub genrnc--get-schema-location => "http://www.springframework.org/schema/tool")
    (stub genrnc--read-typeid => "New Tool")
    (stub genrnc--remove-schemas-defining => nil)
    (genrnc-rename-user-schema "SpringFrameWork Tool")))

