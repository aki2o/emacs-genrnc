(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "update-schemas-prefix-defining")
  (expect "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">
<uri pattern=\"\" typeId=\"SpringFrameWork Tool\"/>
<namespace ns=\"http://www.springframework.org/schema/tool\" typeId=\"SpringFrameWork Tool\"/>
<documentElement prefix=\"tool\" localName=\"SpringFrameWork_Tool\" typeId=\"SpringFrameWork Tool\"/>
<typeId id=\"SpringFrameWork Tool\" uri=\"11.rnc\"/>
</locatingRules>
"
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (sfile (tenv-get-tmp-file "genrnc" genrnc--schemas-file-name))
           (genrnc-user-schemas-directory tdir))
      (tenv-update-file sfile t
                        "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">\n"
                        "<uri pattern=\"\" typeId=\"SpringFrameWork Tool\"/>\n"
                        "<namespace ns=\"http://www.springframework.org/schema/tool\" typeId=\"SpringFrameWork Tool\"/>\n"
                        "<documentElement prefix=\"\" localName=\"SpringFrameWork_Tool\" typeId=\"SpringFrameWork Tool\"/>\n"
                        "<typeId id=\"SpringFrameWork Tool\" uri=\"11.rnc\"/>\n"
                        "</locatingRules>\n")
      (stub genrnc--get-namespace-alist-in-schema-file => '(("" . "http://www.springframework.org/schema/hadoop")
                                                            ("beans" . "http://www.springframework.org/schema/beans")
                                                            ("tool" . "http://www.springframework.org/schema/tool")
                                                            ("xsi" . "http://www.w3.org/2001/XMLSchema-instance")))
      (genrnc--update-schemas-prefix-defining "1")
      (with-current-buffer (genrnc--get-schemas-file-buffer)
        (buffer-string)))))

