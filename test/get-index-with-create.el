(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "get-index-with-create not exist")
  (expect "1"
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (genrnc-user-schemas-directory tdir))
      (setq genrnc--index-count 0)
      (genrnc--init-schema-directory t)
      (genrnc--get-index-with-create "http://www.springframework.org/schema/tool")))
  (desc "get-index-with-create add entry")
  (expect "\n1 http://www.springframework.org/schema/tool\n"
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (genrnc-user-schemas-directory tdir))
      (with-current-buffer (genrnc--get-index-file-buffer)
        (buffer-string))))
  (desc "get-index-with-create not exist 2")
  (expect "2"
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (genrnc-user-schemas-directory tdir))
      (genrnc--get-index-with-create "http://www.springframework.org/schema/bean")))
  (desc "get-index-with-create add entry 2")
  (expect "\n1 http://www.springframework.org/schema/tool\n2 http://www.springframework.org/schema/bean\n"
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (genrnc-user-schemas-directory tdir))
      (with-current-buffer (genrnc--get-index-file-buffer)
        (buffer-string))))
  (desc "get-index-with-create exist")
  (expect "1"
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (genrnc-user-schemas-directory tdir))
      (genrnc--get-index-with-create "http://www.springframework.org/schema/tool")))
  (desc "get-index-with-create not add entry")
  (expect "\n1 http://www.springframework.org/schema/tool\n2 http://www.springframework.org/schema/bean\n"
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (genrnc-user-schemas-directory tdir))
      (setq genrnc--index-count 0)
      (with-current-buffer (genrnc--get-index-file-buffer)
        (buffer-string)))))

