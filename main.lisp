;;;; main.lisp - Hello Triangle window and render loop
;;;; This file creates a window and should be run interactively

;;; Load setup first
(load (merge-pathnames "setup.lisp" *load-truename*))

(in-package :hello-triangle)

;;; Window parameters
(defparameter *window-width* 800)
(defparameter *window-height* 600)
(defparameter *window-title* "Hello Triangle - Common Lisp")

;;; Key callback for ESC to close
(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close window)))

;;; Framebuffer resize callback
(glfw:def-framebuffer-size-callback framebuffer-size-callback (window width height)
  (declare (ignore window))
  (gl:viewport 0 0 width height))

;;; Create vertex array object and buffer
(defun create-triangle-vao ()
  "Create and configure VAO/VBO for the triangle. Returns VAO id."
  (let ((vao (gl:gen-vertex-array))
        (vbo (gl:gen-buffer)))
    ;; Bind VAO first
    (gl:bind-vertex-array vao)

    ;; Create and fill VBO
    (gl:bind-buffer :array-buffer vbo)
    (let ((arr (gl:alloc-gl-array :float (length *triangle-vertices*))))
      (dotimes (i (length *triangle-vertices*))
        (setf (gl:glaref arr i) (aref *triangle-vertices* i)))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr))

    ;; Position attribute (location 0)
    (gl:vertex-attrib-pointer 0 3 :float nil (* 6 4) 0)
    (gl:enable-vertex-attrib-array 0)

    ;; Color attribute (location 1)
    (gl:vertex-attrib-pointer 1 3 :float nil (* 6 4) (* 3 4))
    (gl:enable-vertex-attrib-array 1)

    ;; Unbind VAO (not VBO, it's recorded in VAO)
    (gl:bind-vertex-array 0)

    vao))

;;; Main run function
(defun run ()
  "Create window and run the hello triangle render loop."
  (format t "~%Starting Hello Triangle...~%")

  ;; Initialize GLFW
  (unless (glfw:initialize)
    (error "Failed to initialize GLFW"))

  ;; Create window with OpenGL 3.3 Core profile
  (let ((window (glfw:create-window
                 :width *window-width*
                 :height *window-height*
                 :title *window-title*
                 :context-version-major 3
                 :context-version-minor 3
                 :opengl-profile :opengl-core-profile
                 :opengl-forward-compat t)))
    (unless window
      (glfw:terminate)
      (error "Failed to create GLFW window"))

    (unwind-protect
         (progn
           ;; Make OpenGL context current
           (glfw:make-context-current window)

           ;; Set callbacks
           (glfw:set-key-callback 'key-callback window)
           (glfw:set-framebuffer-size-callback 'framebuffer-size-callback window)

           ;; Enable vsync
           (glfw:swap-interval 1)

           ;; Print OpenGL info
           (format t "OpenGL Version: ~A~%" (gl:get-string :version))
           (format t "GLSL Version: ~A~%" (gl:get-string :shading-language-version))
           (format t "Renderer: ~A~%~%" (gl:get-string :renderer))

           ;; Create shader program
           (let ((shader-program (create-shader-program
                                  *vertex-shader-source*
                                  *fragment-shader-source*))
                 (vao (create-triangle-vao)))

             ;; Set clear color (dark gray)
             (gl:clear-color 0.2 0.2 0.2 1.0)

             (format t "Press ESC to exit.~%")

             ;; Render loop
             (loop until (glfw:window-should-close-p window)
                   do (progn
                        ;; Clear screen
                        (gl:clear :color-buffer-bit)

                        ;; Draw triangle
                        (gl:use-program shader-program)
                        (gl:bind-vertex-array vao)
                        (gl:draw-arrays :triangles 0 3)

                        ;; Swap buffers and poll events
                        (glfw:swap-buffers window)
                        (glfw:poll-events)))

             ;; Cleanup
             (gl:delete-vertex-arrays (list vao))
             (gl:delete-program shader-program)))

      ;; Cleanup (unwind-protect)
      (glfw:destroy-window window)
      (glfw:terminate)))

  (format t "~%Goodbye!~%")
  t)

;;; Allow running directly
(defun main ()
  "Entry point for running as a script."
  (run)
  (sb-ext:exit :code 0))
