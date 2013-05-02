(require 'genrnc)
(require 'el-expectations)

(expectations
  (desc "finish-generate succeed")
  (expect (mock (genrnc--store-schemas-defining "1"))
    (stub genrnc--get-index-with-create => "1")
    (stub genrnc--update-schemas-prefix-defining => nil)
    (puthash "http://www.springframework.org/schema/tool"
             (make-genrnc--schema :name "http://www.springframework.org/schema/tool"
                                  :state 'loading)
             genrnc--hash-schema-cache)
    (cc:dataflow-clear genrnc--dfenv "http://www.springframework.org/schema/tool")
    (genrnc--finish-generate "http://www.springframework.org/schema/tool"
                             'succeed
                             nil
                             nil
                             "SpringFrameWork Tool")))
    
(expectations
  (desc "finish-generate succeed root")
  (expect (mock (genrnc--update-schemas-prefix-defining "1"))
    (stub genrnc--get-index-with-create => "1")
    (stub genrnc--store-schemas-defining => nil)
    (puthash "http://www.springframework.org/schema/tool"
             (make-genrnc--schema :name "http://www.springframework.org/schema/tool"
                                  :state 'loading)
             genrnc--hash-schema-cache)
    (cc:dataflow-clear genrnc--dfenv "http://www.springframework.org/schema/tool")
    (genrnc--finish-generate "http://www.springframework.org/schema/tool"
                             'succeed
                             t
                             nil
                             "SpringFrameWork Tool")))
    
(expectations
  (desc "finish-generate succeed set")
  (expect (mock (cc:dataflow-set genrnc--dfenv "http://www.springframework.org/schema/tool" *))
    (stub genrnc--get-index-with-create => "1")
    (stub genrnc--store-schemas-defining => nil)
    (stub genrnc--update-schemas-prefix-defining => nil)
    (puthash "http://www.springframework.org/schema/tool"
             (make-genrnc--schema :name "http://www.springframework.org/schema/tool"
                                  :state 'loading)
             genrnc--hash-schema-cache)
    (cc:dataflow-clear genrnc--dfenv "http://www.springframework.org/schema/tool")
    (genrnc--finish-generate "http://www.springframework.org/schema/tool"
                             'succeed
                             nil
                             nil
                             "SpringFrameWork Tool")))
    
(expectations
  (desc "finish-generate errored")
  (expect 'failed
    (stub genrnc--get-index-with-create => "1")
    (puthash "http://www.springframework.org/schema/tool"
             (make-genrnc--schema :name "http://www.springframework.org/schema/tool"
                                  :state 'loading)
             genrnc--hash-schema-cache)
    (cc:dataflow-clear genrnc--dfenv "http://www.springframework.org/schema/tool")
    (ad-with-auto-activation-disabled
     (flet ((genrnc--store-schemas-defining (idx &optional typeid)
                                            (error "Test Error!")))
       (genrnc--finish-generate "http://www.springframework.org/schema/tool" 'succeed t)))
    (genrnc--schema-state (gethash "http://www.springframework.org/schema/tool" genrnc--hash-schema-cache))))

(expectations
  (desc "finish-generate errored set")
  (expect (mock (cc:dataflow-set genrnc--dfenv "http://www.springframework.org/schema/tool" *))
    (stub genrnc--get-index-with-create => "1")
    (remhash "http://www.springframework.org/schema/tool" genrnc--hash-schema-cache)
    (cc:dataflow-clear genrnc--dfenv "http://www.springframework.org/schema/tool")
    (genrnc--finish-generate "http://www.springframework.org/schema/tool" 'loading t)))

