(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "get-index exist")
  (expect "5"
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (genrnc-user-schemas-directory tdir)
           (genrnc--index-count 0))
      (genrnc--init-schema-directory t)
      (with-current-buffer (genrnc--get-index-file-buffer)
        (goto-char (point-max))
        (insert "5 http://www.springframework.org/schema/tool\n")
        (save-buffer))
      (genrnc--get-index "http://www.springframework.org/schema/tool")))
  (desc "get-index not exist")
  (expect nil
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (genrnc-user-schemas-directory tdir))
      (genrnc--get-index "http://www.springframework.org/schema/bean"))))

