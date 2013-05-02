(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "remove-schema default")
  (expect '("." ".." "1.rnc" "2.dtd" "2.rnc" "2.rng" "2.xsd")
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (sfiles (loop for i in '("1" "2")
                         append (loop for x in '("xsd" "rng" "dtd" "rnc")
                                      collect (tenv-get-tmp-file
                                               "genrnc" (concat i "." x) t))))
           (genrnc-user-schemas-directory tdir))
      (loop for f in sfiles
            do (tenv-update-file f t "hoge"))
      (genrnc--remove-schema "1")
      (directory-files tdir))))

(expectations
  (desc "remove-schema remove-definition-p")
  (expect '("." ".." "2.dtd" "2.rnc" "2.rng" "2.xsd")
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (sfiles (loop for i in '("1" "2")
                         append (loop for x in '("xsd" "rng" "dtd" "rnc")
                                      collect (tenv-get-tmp-file
                                               "genrnc" (concat i "." x) t))))
           (genrnc-user-schemas-directory tdir))
      (loop for f in sfiles
            do (tenv-update-file f t "hoge"))
      (stub genrnc--get-schemas-typeid-list => nil)
      (stub genrnc--remove-schemas-defining => nil)
      (genrnc--remove-schema "1" t)
      (directory-files tdir))))

(expectations
  (desc "remove-schema fext")
  (expect '("." ".." "1.dtd" "1.rnc" "1.xsd" "2.dtd" "2.rnc" "2.rng" "2.xsd")
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (sfiles (loop for i in '("1" "2")
                         append (loop for x in '("xsd" "rng" "dtd" "rnc")
                                      collect (tenv-get-tmp-file
                                               "genrnc" (concat i "." x) t))))
           (genrnc-user-schemas-directory tdir))
      (loop for f in sfiles
            do (tenv-update-file f t "hoge"))
      (stub genrnc--get-schemas-typeid-list => nil)
      (stub genrnc--remove-schemas-defining => nil)
      (genrnc--remove-schema "1" nil "rng")
      (directory-files tdir))))

(expectations
  (desc "remove-schema call remove-definition")
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
      (genrnc--remove-schema "1" t))))

