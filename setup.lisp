;;;; setup.lisp - Dependencies and definitions for hello triangle
;;;; This file can be loaded and verified without creating a window

;;; Load Quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; Load dependencies
(ql:quickload '(:cl-glfw3 :cl-opengl) :silent t)

(defpackage :hello-triangle
  (:use :cl)
  (:export #:verify-setup
           #:run
           #:*vertex-shader-source*
           #:*fragment-shader-source*
           #:*triangle-vertices*
           #:compile-shader
           #:create-shader-program))

(in-package :hello-triangle)

;;; GLSL Shader Sources (OpenGL 3.3 Core)

(defparameter *vertex-shader-source*
  "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;

out vec3 vertexColor;

void main()
{
    gl_Position = vec4(aPos, 1.0);
    vertexColor = aColor;
}
")

(defparameter *fragment-shader-source*
  "#version 330 core
in vec3 vertexColor;
out vec4 FragColor;

void main()
{
    FragColor = vec4(vertexColor, 1.0);
}
")

;;; Triangle vertex data: position (x, y, z) and color (r, g, b)
;;; Classic RGB triangle

(defparameter *triangle-vertices*
  #(;; Position          ;; Color
    -0.5 -0.5  0.0       1.0  0.0  0.0   ; Bottom-left  (red)
     0.5 -0.5  0.0       0.0  1.0  0.0   ; Bottom-right (green)
     0.0  0.5  0.0       0.0  0.0  1.0)) ; Top          (blue)

;;; Shader compilation helpers

(defun compile-shader (source shader-type)
  "Compile a shader from source string. Returns shader ID."
  (let ((shader (gl:create-shader shader-type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    ;; Check for compilation errors
    (let ((success (gl:get-shader shader :compile-status)))
      (unless success
        (let ((log (gl:get-shader-info-log shader)))
          (error "Shader compilation failed: ~A" log))))
    shader))

(defun create-shader-program (vertex-source fragment-source)
  "Create and link a shader program from vertex and fragment shader sources."
  (let ((vertex-shader (compile-shader vertex-source :vertex-shader))
        (fragment-shader (compile-shader fragment-source :fragment-shader))
        (program (gl:create-program)))
    (gl:attach-shader program vertex-shader)
    (gl:attach-shader program fragment-shader)
    (gl:link-program program)
    ;; Check for linking errors
    (let ((success (gl:get-program program :link-status)))
      (unless success
        (let ((log (gl:get-program-info-log program)))
          (error "Shader program linking failed: ~A" log))))
    ;; Delete shaders (they're linked into the program now)
    (gl:delete-shader vertex-shader)
    (gl:delete-shader fragment-shader)
    program))

;;; Verification function (no window created)

(defun verify-setup ()
  "Verify that all dependencies are loaded and GLFW can initialize.
   Does NOT create a window - safe to run headlessly."
  (format t "~%=== Hello Triangle Setup Verification ===~%")

  ;; Check cl-opengl is loaded
  (format t "~%[1/3] Checking cl-opengl... ")
  (if (find-package :gl)
      (format t "OK (package :gl found)~%")
      (error "cl-opengl not loaded!"))

  ;; Check cl-glfw3 is loaded
  (format t "[2/3] Checking cl-glfw3... ")
  (if (find-package :glfw)
      (format t "OK (package :glfw found)~%")
      (error "cl-glfw3 not loaded!"))

  ;; Check GLFW can initialize
  (format t "[3/3] Testing GLFW initialization... ")
  (if (glfw:initialize)
      (progn
        (format t "OK~%")
        (glfw:terminate))
      (error "GLFW initialization failed!"))

  (format t "~%All checks passed! Ready to run (run) from main.lisp~%")
  t)
