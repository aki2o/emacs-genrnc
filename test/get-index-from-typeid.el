(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "get-index-from-typeid exist")
  (expect "11"
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (sfile (tenv-get-tmp-file "genrnc" genrnc--schemas-file-name t))
           (genrnc-user-schemas-directory tdir))
      (tenv-update-file sfile t
                        "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">\n"
                        "<typeId id=\"SpringFrameWork Tool\" uri=\"11.rnc\"/>\n"
                        "</locatingRules>\n")
      (genrnc--get-index-from-typeid "SpringFrameWork Tool")))
  (desc "get-index-from-typeid not exist")
  (expect nil
    (genrnc--get-index-from-typeid "SpringFrameWork Bean")))

