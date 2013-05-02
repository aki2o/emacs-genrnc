(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "read-typeid")
  (expect "Sp Fr Tool"
    (setq genrnc--defined-typeid-list nil)
    (stub genrnc--update-defined-typeid-list => nil)
    (stub read-from-minibuffer => " Sp Fr Tool ")
    (genrnc--read-typeid "http://www.springframework.org/schema/tool")))


