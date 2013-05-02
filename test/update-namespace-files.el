(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "update-namespace-files nothing")
  (expect 0
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (sfile (tenv-get-tmp-file "genrnc" genrnc--schemas-file-name))
           (genrnc-user-schemas-directory tdir)
           (rng-schema-locating-files (list sfile)))
      (tenv-update-file sfile t
                        "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">\n"
                        "</locatingRules>\n")
      (setq genrnc--hash-namespace-files (make-hash-table :test 'equal))
      (genrnc--update-namespace-files)
      (loop for k being the hash-keys in genrnc--hash-namespace-files
            count k))))

(expectations
  (desc "update-namespace-files one namespace")
  (expect 1
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (sfile (tenv-get-tmp-file "genrnc" genrnc--schemas-file-name))
           (rfile (tenv-get-tmp-file "genrnc" "1.rnc"))
           (genrnc-user-schemas-directory tdir)
           (rng-schema-locating-files (list sfile)))
      (tenv-update-file sfile t
                        "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">\n"
                        "<namespace ns=\"http://www.springframework.org/schema/tool\" typeId=\"SpringFrameWork Tool\"/>\n"
                        "<typeId id=\"SpringFrameWork Tool\" uri=\"1.rnc\"/>\n"
                        "</locatingRules>\n")
      (tenv-update-file rfile t "hoge")
      (setq genrnc--hash-namespace-files (make-hash-table :test 'equal))
      (genrnc--update-namespace-files)
      (loop for k being the hash-keys in genrnc--hash-namespace-files
            count k)))
  (desc "update-namespace-files get nsfiles")
  (expect t
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (nsfiles (gethash "http://www.springframework.org/schema/tool" genrnc--hash-namespace-files)))
      (string= (concat tdir "/1.rnc") (pop nsfiles)))))


