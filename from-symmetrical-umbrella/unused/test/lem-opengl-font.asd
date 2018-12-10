(asdf:defsystem #:lem-opengl-font
  :depends-on (#:utility
	       #:cl-freetype2
	       #:opticl)
  :components 
  ((:file "fonts")))

