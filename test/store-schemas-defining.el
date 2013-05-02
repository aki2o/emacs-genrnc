(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "store-schemas-defining not exist")
  (expect "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">
\t<uri pattern=\"\" typeId=\"SpringFrameWork Tool\"/>
\t<namespace ns=\"http://www.springframework.org/schema/tool\" typeId=\"SpringFrameWork Tool\"/>
\t<documentElement prefix=\"tool\" localName=\"SpringFrameWork_Tool\" typeId=\"SpringFrameWork Tool\"/>
\t<typeId id=\"SpringFrameWork Tool\" uri=\"1.rnc\"/>
</locatingRules>
"
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (genrnc-user-schemas-directory tdir))
      (genrnc--init-schema-directory t)
      (stub genrnc--get-schema-namespace => '("tool" . "http://www.springframework.org/schema/tool"))
      (genrnc--store-schemas-defining "1" "SpringFrameWork Tool")
      (with-current-buffer (genrnc--get-schemas-file-buffer)
        (buffer-string))))
  (desc "store-schemas-defining exist")
  (expect "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">
\t<uri pattern=\"\" typeId=\"SpringFrameWork Tool\"/>
\t<namespace ns=\"http://www.springframework.org/schema/tool\" typeId=\"SpringFrameWork Tool\"/>
\t<documentElement prefix=\"bean\" localName=\"SpringFrameWork_Tool\" typeId=\"SpringFrameWork Tool\"/>
\t<typeId id=\"SpringFrameWork Tool\" uri=\"5.rnc\"/>
</locatingRules>
"
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (genrnc-user-schemas-directory tdir))
      (stub genrnc--get-schema-namespace => '("bean" . "http://www.springframework.org/schema/tool"))
      (genrnc--store-schemas-defining "5" "SpringFrameWork Tool")
      (with-current-buffer (genrnc--get-schemas-file-buffer)
        (buffer-string))))
  (desc "store-schemas-defining not ns")
  (expect "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">
\t<uri pattern=\"\" typeId=\"SpringFrameWork Tool\"/>
\t<documentElement prefix=\"\" localName=\"SpringFrameWork_Tool\" typeId=\"SpringFrameWork Tool\"/>
\t<typeId id=\"SpringFrameWork Tool\" uri=\"5.rnc\"/>
</locatingRules>
"
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (genrnc-user-schemas-directory tdir))
      (stub genrnc--get-schema-namespace => nil)
      (genrnc--store-schemas-defining "5" "SpringFrameWork Tool")
      (with-current-buffer (genrnc--get-schemas-file-buffer)
        (buffer-string))))
  (desc "store-schemas-defining not typeid")
  (expect "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">
\t<uri pattern=\"\" typeId=\"SpringFrameWork Tool\"/>
\t<documentElement prefix=\"\" localName=\"SpringFrameWork_Tool\" typeId=\"SpringFrameWork Tool\"/>
\t<typeId id=\"SpringFrameWork Tool\" uri=\"5.rnc\"/>
\t<uri pattern=\"\" typeId=\"[genrnc-5]\"/>
\t<namespace ns=\"http://www.springframework.org/schema/tool\" typeId=\"[genrnc-5]\"/>
\t<documentElement prefix=\"tool\" localName=\"genrnc-5\" typeId=\"[genrnc-5]\"/>
\t<typeId id=\"[genrnc-5]\" uri=\"5.rnc\"/>
</locatingRules>
"
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (genrnc-user-schemas-directory tdir))
      (stub genrnc--get-schema-namespace => '("tool" . "http://www.springframework.org/schema/tool"))
      (genrnc--store-schemas-defining "5")
      (with-current-buffer (genrnc--get-schemas-file-buffer)
        (buffer-string)))))

