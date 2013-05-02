(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "remove-schema-all default")
  (expect '("." ".." "1.rnc" "2.rnc")
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (sfiles (loop for i in '("1" "2")
                         append (loop for x in '("xsd" "rng" "dtd" "rnc")
                                      collect (tenv-get-tmp-file
                                               "genrnc" (concat i "." x) t))))
           (genrnc-user-schemas-directory tdir))
      (loop for f in sfiles
            do (tenv-update-file f t "hoge"))
      (stub genrnc--get-schemas-typeid-list => '("SpringFrameWork Tool"))
      (stub genrnc--remove-schemas-defining => nil)
      (genrnc--remove-schema-all)
      (directory-files tdir))))

(expectations
  (desc "remove-schema-all remove-definition-p")
  (expect '("." "..")
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (sfiles (loop for i in '("1" "2")
                         append (loop for x in '("xsd" "rng" "dtd" "rnc")
                                      collect (tenv-get-tmp-file
                                               "genrnc" (concat i "." x) t))))
           (genrnc-user-schemas-directory tdir))
      (loop for f in sfiles
            do (tenv-update-file f t "hoge"))
      (stub genrnc--get-schemas-typeid-list => '("SpringFrameWork Tool"))
      (stub genrnc--remove-schemas-defining => nil)
      (genrnc--remove-schema-all t)
      (directory-files tdir))))

(expectations
  (desc "remove-schema-all remove-definition")
  (expect (mock (genrnc--remove-schemas-defining "SpringFrameWork Tool"))
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (sfiles (loop for i in '("1" "2")
                         append (loop for x in '("xsd" "rng" "dtd" "rnc")
                                      collect (tenv-get-tmp-file
                                               "genrnc" (concat i "." x) t))))
           (genrnc-user-schemas-directory tdir))
      (loop for f in sfiles
            do (tenv-update-file f t "hoge"))
      (stub genrnc--get-schemas-typeid-list => '("SpringFrameWork Tool"))
      (genrnc--remove-schema-all t)
      (directory-files tdir))))

