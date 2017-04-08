;;;; raytracer.asd

(asdf:defsystem #:raytracer
  :description "Describe raytracer here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:vecto)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "raytracer")))

