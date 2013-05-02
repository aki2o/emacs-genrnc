(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "get-index-file-buffer not exist")
  (expect "\n"
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (ifile (tenv-get-tmp-file "genrnc" genrnc--index-file-name))
           (genrnc-user-schemas-directory tdir))
      (tenv-update-file ifile t "\n")
      (delete-file ifile)
      (let* ((buff (genrnc--get-index-file-buffer)))
        (and (file-exists-p ifile)
             (buffer-live-p buff)
             (string= (buffer-name buff) genrnc--index-file-buffer-name)
             (string= (buffer-file-name buff) ifile)
             (with-current-buffer buff
               (buffer-string))))))
  (desc "get-index-file-buffer exist")
  (expect "\n1 http://www.w3.org/Math/XMLSchema/mathml3/mathml3.xsd\n"
    (let* ((tdir (tenv-get-tmp-directory "genrnc" nil t))
           (ifile (tenv-get-tmp-file "genrnc" genrnc--index-file-name))
           (genrnc-user-schemas-directory tdir))
      (tenv-update-file ifile nil
                        "1 http://www.w3.org/Math/XMLSchema/mathml3/mathml3.xsd\n")
      (let* ((buff (genrnc--get-index-file-buffer)))
        (and (file-exists-p ifile)
             (buffer-live-p buff)
             (string= (buffer-name buff) genrnc--index-file-buffer-name)
             (string= (buffer-file-name buff) ifile)
             (with-current-buffer buff
               (buffer-string)))))))

