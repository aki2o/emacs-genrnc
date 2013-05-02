(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "get-schemas-file-buffer not exist")
  (expect "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">\n</locatingRules>\n"
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (sfile (tenv-get-tmp-file "genrnc" genrnc--schemas-file-name t))
           (genrnc-user-schemas-directory tdir)
           (buff (genrnc--get-schemas-file-buffer)))
      (and (file-exists-p sfile)
           (buffer-live-p buff)
           (string= (buffer-name buff) genrnc--schemas-file-buffer-name)
           (string= (buffer-file-name buff) sfile)
           (with-current-buffer buff
             (buffer-string))))))

(expectations
  (desc "get-schemas-file-buffer exist")
  (expect "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">\n<typeId id=\"\" uri=\"\"/>\n</locatingRules>\n"
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (sfile (tenv-get-tmp-file "genrnc" genrnc--schemas-file-name))
           (genrnc-user-schemas-directory tdir))
      (tenv-update-file sfile t
                        "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">\n"
                        "<typeId id=\"\" uri=\"\"/>\n"
                        "</locatingRules>\n")
      (let* ((buff (genrnc--get-schemas-file-buffer)))
        (and (buffer-live-p buff)
             (string= (buffer-name buff) genrnc--schemas-file-buffer-name)
             (string= (buffer-file-name buff) sfile)
             (with-current-buffer buff
               (buffer-string)))))))

