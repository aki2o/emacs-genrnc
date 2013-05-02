(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "user-file-p not exist")
  (expect nil
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (sfile (tenv-get-tmp-file "genrnc" "1.rnc"))
           (genrnc-user-schemas-directory tdir))
      (tenv-update-file sfile t "hoge")
      (delete-file sfile)
      (genrnc--user-file-p (concat tdir "/1.rnc")))))

(expectations
  (desc "user-file-p exist")
  (expect t
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (sfile (tenv-get-tmp-file "genrnc" "1.rnc"))
           (genrnc-user-schemas-directory tdir))
      (tenv-update-file sfile t "hoge")
      (genrnc--user-file-p (concat tdir "/1.rnc")))))

