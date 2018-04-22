(defsystem "engine"
  :components
  ((:file "utils")
   (:file "display" :depends-on ("utils"))
   (:file "matrix" :depends-on ("utils"))
   (:file "edges" :depends-on ("matrix"))
   (:file "draw" :depends-on ("display" "edges"))
   (:file "parser" :depends-on ("draw"))
   (:file "main" :depends-on ("parser"))))
