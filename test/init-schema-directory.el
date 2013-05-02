(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "init-schema-directory not exist")
  (expect t
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t))
           (genrnc-user-schemas-directory tdir))
      (genrnc--init-schema-directory)
      (and (file-directory-p tdir)
           (file-exists-p (concat tdir "/" genrnc--schemas-file-name))
           (file-exists-p (concat tdir "/trang.jar"))
           (file-exists-p (concat tdir "/xsd2rng.jar"))
           (file-exists-p (concat tdir "/xsd2rng.xsl"))))))

(expectations
  (desc "init-schema-directory force")
  (expect "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">\n</locatingRules>\n"
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (sfile (tenv-get-tmp-file "genrnc" genrnc--schemas-file-name))
           (tfile (tenv-get-tmp-file "genrnc" "hoge.txt"))
           (genrnc-user-schemas-directory tdir))
      (tenv-update-file sfile t
                        "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">\n"
                        "<typeId id=\"\" uri=\"\"/>\n"
                        "</locatingRules>\n")
      (tenv-update-file tfile t "hoge\n")
      (genrnc--init-schema-directory t)
      (and (file-directory-p tdir)
           (file-exists-p sfile)
           (file-exists-p (concat tdir "/trang.jar"))
           (file-exists-p (concat tdir "/xsd2rng.jar"))
           (file-exists-p (concat tdir "/xsd2rng.xsl"))
           (not (file-exists-p tfile))
           (with-current-buffer (find-file-noselect sfile)
             (buffer-string))))))

