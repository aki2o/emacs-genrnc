(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "exist-schema")
  (expect t
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (sfile (tenv-get-tmp-file "genrnc" "1.rnc"))
           (genrnc-user-schemas-directory tdir))
      (tenv-update-file sfile t "hoge")
      (stub genrnc--get-index => "1")
      (genrnc--exist-schema ""))))

