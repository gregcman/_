(asdf:defsystem #:len-opengl-font
  :depends-on (#:utility
	       #:cl-freetype2)
  :components 
  ((:file "fonts")))

