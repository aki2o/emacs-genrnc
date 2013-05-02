(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "remove-schemas-defining not exist")
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
                        "<documentElement prefix=\"tool\" localName=\"SpringFrameWork_Tool\" typeId=\"SpringFrameWork Tool\"/>\n"
                        "<typeId id=\"SpringFrameWork Tool\" uri=\"11.rnc\"/>\n"
                        "</locatingRules>\n")
      (genrnc--remove-schemas-defining "SpringFrameWork Bean")
      (with-current-buffer (genrnc--get-schemas-file-buffer)
        (buffer-string))))
  (desc "remove-schemas-defining exist")
  (expect "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">
</locatingRules>
"
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (sfile (tenv-get-tmp-file "genrnc" genrnc--schemas-file-name))
           (genrnc-user-schemas-directory tdir))
      (genrnc--remove-schemas-defining "SpringFrameWork Tool")
      (with-current-buffer (genrnc--get-schemas-file-buffer)
        (buffer-string)))))

