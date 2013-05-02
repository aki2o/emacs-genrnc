(require 'genrnc)
(require 'el-expectations)

(expectations
  (desc "generate read typeid")
  (expect (mock (genrnc--read-typeid "http://www.springframework.org/schema/tool"))
    (stub genrnc--get-index => "1")
    (stub genrnc--get-schemas-typeid-by-user => nil)
    (stub genrnc--schema-p => t)
    (stub genrnc--schema-state => 'failed)
    (stub genrnc--exist-schema => t)
    (stub genrnc--user-file-p => nil)
    (genrnc--generate "http://www.springframework.org/schema/tool" "xsd")))

(expectations
  (desc "generate already start")
  (expect (mock (genrnc--info "already finished or started generate '%s'. state:[%s]" "http://www.springframework.org/schema/tool"))
    (stub genrnc--get-index => "1")
    (stub genrnc--get-schemas-typeid-by-user => "SpringFrameWork Tool")
    (stub genrnc--schema-p => t)
    (stub genrnc--schema-state => 'loading)
    (stub genrnc--exist-schema => t)
    (stub genrnc--user-file-p => nil)
    (genrnc--generate "http://www.springframework.org/schema/tool" "xsd")))

;; (expectations
;;   (desc "generate already exist")
;;   (expect (mock (genrnc--finish-generate "http://www.springframework.org/schema/tool" 'succeed t nil "SpringFrameWork Tool" nil))
;;     (stub genrnc--get-index => "1")
;;     (stub genrnc--get-schemas-typeid-by-user => "SpringFrameWork Tool")
;;     (stub genrnc--schema-p => nil)
;;     (stub genrnc--schema-state => 'loading)
;;     (stub genrnc--exist-schema => t)
;;     (stub genrnc--user-file-p => nil)
;;     (genrnc--generate "http://www.springframework.org/schema/tool" "xsd")))

;; (expectations
;;   (desc "generate url")
;;   (expect (mock (genrnc--generate-from-url "http://www.springframework.org/schema/tool" "xsd" t "SpringFrameWork Tool" nil))
;;     (stub genrnc--get-index => "1")
;;     (stub genrnc--get-schemas-typeid-by-user => "SpringFrameWork Tool")
;;     (stub genrnc--schema-p => nil)
;;     (stub genrnc--schema-state => 'loading)
;;     (stub genrnc--exist-schema => nil)
;;     (stub genrnc--user-file-p => nil)
;;     (stub genrnc--finish-generate => nil)
;;     (genrnc--generate "http://www.springframework.org/schema/tool" "xsd")))

