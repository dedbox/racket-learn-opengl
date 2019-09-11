#lang at-exp racket/gui

(require ffi/unsafe
         ffi/vector
         opengl
         opengl/util
         racket/class
         racket/exn)

(define SCR_WIDTH 800)
(define SCR_HEIGHT 600)

(define vertexShaderSource
  (vector
   "#version 330 core\n"
   "layout (location = 0) in vec3 aPos;\n"
   "\n"
   "void main()\n"
   "{\n"
   "    gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);\n"
   "}\n"))

(define fragmentShaderSource
  (vector
   "#version 330 core\n"
   "out vec4 FragColor;\n"
   "\n"
   "void main()\n"
   "{\n"
   "    FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);\n"
   "}\n"))

(define gl-canvas%
  (class canvas%
    (inherit with-gl-context)
    (field [done? #f])
    (define/override (on-size width height)
      (with-gl-context (λ () (glViewport 0 0 width height))))
    (define/override (on-char event)
      (match* ((send event get-key-code)
               (send event get-key-release-code))
        [('escape 'press) (set! done? #t)]
        [(pcode rcode) (writeln `(KEY ,pcode ,rcode))]))
    (super-new [style '(gl no-autoclear)])))

(define-syntax-rule (with-gl-canvas canvas body ...)
  (send canvas with-gl-context (λ () body ...)))

(module+ main
  (define frame (new frame%
                     [label "LearnOpenGL"]
                     [width SCR_WIDTH]
                     [height SCR_HEIGHT]))
  (define canvas (new gl-canvas% [parent frame]))
  (send frame show #t)
  (send canvas focus)

  ;; initialize and configure
  (with-gl-canvas canvas
    (unless (gl-version-at-least? '(3 3 0))
      (error "OpenGL version 3.3 or greater not found.")))

  ;; set up vertex data (and buffer(s)) and configure vertex attributes
  ;; ------------------------------------------------------------------
  (define vertices
    (f32vector  0.5  0.5 0.0            ;top right
                0.5 -0.5 0.0            ;bottom right
               -0.5 -0.5 0.0            ;bottom left
               -0.5  0.5 0.0))          ;top left
  (define indices
    (u8vector                           ;note that we start from 0!
     0 1 3                              ;first Triangle
     1 2 3))                            ;second Triangle

  (thread
   (λ ()
     (collect-garbage)

     (with-gl-canvas canvas
       ;; build and compile our shader program
       ;; ------------------------------------
       (define program
         (create-program
          (load-shader "vshader.glsl" GL_VERTEX_SHADER)
          (load-shader "fshader.glsl" GL_FRAGMENT_SHADER)))
       (glUseProgram program)

       (define buffer (u32vector-ref (glGenBuffers 1) 0))
       (glBindBuffer GL_ARRAY_BUFFER buffer)
       (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof vertices)
                     vertices GL_STATIC_DRAW)

       (define vp (glGetAttribLocation program "vPosition"))
       (glEnableVertexAttribArray vp)
       (glVertexAttribPointer vp 3 GL_FLOAT #f 0 0)

       (glClearColor 0.2 0.3 0.3 1.0)
       ;; (glPolygonMode GL_FRONT_AND_BACK GL_LINE)
       )

     ;; render loop
     ;; -----------
     (let loop ([frames 0]
                [start-time (current-inexact-milliseconds)])
       (collect-garbage 'incremental)
       (if (>= (- (current-inexact-milliseconds) start-time) 1000.0)
           (begin (writeln frames) (loop 0 (current-inexact-milliseconds)))
           (unless (get-field done? canvas)
             (with-gl-canvas canvas
               ;; render
               ;; ------
               (glClear GL_COLOR_BUFFER_BIT)
               (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_BYTE indices)
               (glFlush))
             (send canvas swap-gl-buffers)
             (sleep)
             (loop (+ frames 1) start-time))))
     (exit))))
