(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "store-schema")
  (expect "This is a test!"
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (genrnc-user-schemas-directory tdir))
      (genrnc--init-schema-directory t)
      (with-temp-buffer
        (insert "This is a test!")
        (genrnc--store-schema "hoge" (current-buffer) "rnc"))
      (and (file-exists-p (concat tdir "/1.rnc"))
           (with-current-buffer (find-file-noselect (concat tdir "/1.rnc"))
             (buffer-string))))))

