(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "update-defined-typeid-list")
  (expect '("SpringFrameWork Beans" "SpringFrameWork Tool")
    (let* ((tdir (tenv-get-tmp-directory "genrnc"))
           (sfile (tenv-get-tmp-file "genrnc" genrnc--schemas-file-name))
           (rng-schema-locating-files (list sfile)))
      (tenv-update-file sfile t
                        "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">\n"
                        "<typeId id=\"SpringFrameWork Tool\" uri=\"1.rnc\"\n/>"
                        "<typeId id=\"SpringFrameWork Beans\" uri=\"1.rnc\"\n/>"
                        "<typeId id=\"SpringFrameWork Tool\" uri=\"1.rnc\"\n/>"
                        "</locatingRules>\n")
      (setq genrnc--defined-typeid-list nil)
      (genrnc--update-defined-typeid-list)
      genrnc--defined-typeid-list)))


