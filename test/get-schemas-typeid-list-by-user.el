(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "get-schemas-typeid-list-by-user 0")
  (expect nil
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (sfile (tenv-get-tmp-file "genrnc" genrnc--schemas-file-name))
           (genrnc-user-schemas-directory tdir))
      (tenv-update-file sfile t
                        "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">\n"
                        "<uri pattern=\"\" typeId=\"SpringFrameWork Tool\"/>\n"
                        "<namespace ns=\"http://www.springframework.org/schema/tool\" typeId=\"SpringFrameWork Tool\"/>\n"
                        "<documentElement prefix=\"tool\" localName=\"SpringFrameWork_Tool\" typeId=\"SpringFrameWork Tool\"/>\n"
                        "</locatingRules>\n")
      (genrnc--get-schemas-typeid-list-by-user))))

(expectations
  (desc "get-schemas-typeid-list-by-user 1")
  (expect '("SpringFrameWork Tool")
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
      (genrnc--get-schemas-typeid-list-by-user))))

(expectations
  (desc "get-schemas-typeid-list-by-user 2")
  (expect '("SpringFrameWork Tool")
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (sfile (tenv-get-tmp-file "genrnc" genrnc--schemas-file-name))
           (genrnc-user-schemas-directory tdir))
      (tenv-update-file sfile t
                        "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">\n"
                        "<typeId id=\"[genrnc-11]\" uri=\"11.rnc\"/>\n"
                        "<uri pattern=\"\" typeId=\"SpringFrameWork Tool\"/>\n"
                        "<namespace ns=\"http://www.springframework.org/schema/tool\" typeId=\"SpringFrameWork Tool\"/>\n"
                        "<documentElement prefix=\"tool\" localName=\"SpringFrameWork_Tool\" typeId=\"SpringFrameWork Tool\"/>\n"
                        "<typeId id=\"SpringFrameWork Tool\" uri=\"11.rnc\"/>\n"
                        "</locatingRules>\n")
      (genrnc--get-schemas-typeid-list-by-user))))

