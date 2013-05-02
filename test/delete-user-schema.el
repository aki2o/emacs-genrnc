(require 'genrnc)
(require 'el-expectations)

(expectations
  (desc "delete-user-schema answer no")
  (expect (mock (message *))
    (stub y-or-n-p => nil)
    (genrnc-delete-user-schema "SpringFrameWork Tool")))

(expectations
  (desc "delete-user-schema answer yes")
  (expect (mock (genrnc--remove-schema "1" t))
    (stub y-or-n-p => t)
    (stub genrnc--init-schema-directory => nil)
    (stub genrnc--get-index-from-typeid => "1")
    (genrnc-delete-user-schema "SpringFrameWork Tool")))

