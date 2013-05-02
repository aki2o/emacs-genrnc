(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "get-schema-location exist")
  (expect "http://www.springframework.org/schema/tool"
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (genrnc-user-schemas-directory tdir))
      (with-current-buffer (genrnc--get-index-file-buffer)
        (insert "5 http://www.springframework.org/schema/tool\n")
        (save-buffer))
      (genrnc--get-schema-location "5")))
  (desc "get-schema-location not exist")
  (expect nil
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (genrnc-user-schemas-directory tdir))
      (genrnc--get-schema-location "6"))))

