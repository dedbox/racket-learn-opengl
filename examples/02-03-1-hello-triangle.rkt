#lang at-exp racket/gui

(require ffi/unsafe
         ffi/vector
         opengl
         opengl/util
         racket/class
         racket/exn)

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
  (define frame (new frame% [label "LearnOpenGL"] [width 800] [height 600]))
  (define canvas (new gl-canvas% [parent frame]))
  (send frame show #t)
  (send canvas focus)

  (with-gl-canvas canvas
    (unless (gl-version-at-least? '(3 3 0))
      (error "OpenGL version 3.3 or greater not found.")))

  (define vertices
    ;;          position      color        texture
    (f32vector  0.5  0.5 0.0  1.0 0.0 0.0  1.0 1.0   ;top right
                0.5 -0.5 0.0  0.0 1.0 0.0  1.0 0.0   ;bottom right
               -0.5 -0.5 0.0  0.0 0.0 1.0  0.0 0.0   ;bottom left
               -0.5  0.5 0.0  1.0 1.0 0.0  0.0 1.0)) ;top left

  (define indices
    (u8vector 0 1 3
              1 2 3))

  (define texCoords
    (f32vector 0.0 0.0
               1.0 0.0
               0.5 1.0))

  (thread
   (λ ()
     (collect-garbage)

     (define program #f)
     (define EBO #f)

     (with-gl-canvas canvas
       ;; build and compile our shader program
       ;; ------------------------------------
       (set! program
         (create-program
          (load-shader "examples/02-03-1-vshader.glsl" GL_VERTEX_SHADER)
          (load-shader "examples/02-03-1-fshader.glsl" GL_FRAGMENT_SHADER)))
       (glUseProgram program)
       
       (define VAO (u32vector-ref (glGenVertexArrays 1) 0))
       (glBindVertexArray VAO)

       (define buffer (u32vector-ref (glGenBuffers 1) 0))
       (glBindBuffer GL_ARRAY_BUFFER buffer)
       (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof vertices)
                     vertices GL_STATIC_DRAW)

       (set! EBO (u32vector-ref (glGenBuffers 1) 0))
       (glBindBuffer GL_ELEMENT_ARRAY_BUFFER EBO)
       (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof indices)
                     indices GL_STATIC_DRAW)

       (define (argb->rgba bs)
         (if (null? bs)
             null
             (let ([A (car bs)]
                   [R (cadr bs)]
                   [G (caddr bs)]
                   [B (cadddr bs)])
               (append (list R G B A) (argb->rgba (cddddr bs))))))

       (define (vertical-flip! data depth width height)
         (define bytes-per-row (* depth width))
         (for ([row (in-range (/ height 2))])
           (define row0 (* bytes-per-row row))
           (define row1 (* bytes-per-row (- height row 1)))
           (define temp (subbytes data row0 (+ row0 bytes-per-row)))
           (bytes-copy! data row0 data row1 (+ row1 bytes-per-row))
           (bytes-copy! data row1 temp)))

       (define (load-image path)
         (define bmp (read-bitmap path))
         (define depth (/ (send bmp get-depth) 8))
         (define width (send bmp get-width))
         (define height (send bmp get-height))
         (define data (make-bytes (* depth width height)))
         (send bmp get-argb-pixels 0 0 width height data)
         (vertical-flip! data depth width height)
         (values depth width height
                 (list->u8vector (argb->rgba (bytes->list data)))))

       (define-values (d0 w0 h0 image0) (load-image "examples/container.jpg"))
       (define-values (d1 w1 h1 image1) (load-image "examples/awesomeface.png"))

       (define texture0 (u32vector-ref (glGenTextures 1) 0))
       (glActiveTexture GL_TEXTURE0)
       (glBindTexture GL_TEXTURE_2D texture0)
       (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
       (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
       (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
       (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
       (glTexImage2D GL_TEXTURE_2D
                     0 GL_RGB w0 h0
                     0 GL_RGBA GL_UNSIGNED_BYTE image0)
       (glGenerateMipmap GL_TEXTURE_2D)

       (define texture1 (u32vector-ref (glGenTextures 1) 0))
       (glActiveTexture GL_TEXTURE1)
       (glBindTexture GL_TEXTURE_2D texture1)
       (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
       (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
       (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
       (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
       (glTexImage2D GL_TEXTURE_2D
                     0 GL_RGB w1 h1
                     0 GL_RGBA GL_UNSIGNED_BYTE image1)
       (glGenerateMipmap GL_TEXTURE_2D)

       (glUniform1i (glGetUniformLocation program "texture0") 0)
       (glUniform1i (glGetUniformLocation program "texture1") 1)

       ;; awesomeface.png

       ;; position attribute
       (glVertexAttribPointer 0 3 GL_FLOAT #f (* 8 (gl-type-sizeof GL_FLOAT)) 0)
       (glEnableVertexAttribArray 0)

       ;; color attribute
       (glVertexAttribPointer 1 3 GL_FLOAT #f
                              (* 8 (gl-type-sizeof GL_FLOAT))
                              (* 3 (gl-type-sizeof GL_FLOAT)))
       (glEnableVertexAttribArray 1)

       ;; texture attribute
       (glVertexAttribPointer 2 2 GL_FLOAT #f
                              (* 8 (gl-type-sizeof GL_FLOAT))
                              (* 6 (gl-type-sizeof GL_FLOAT)))
       (glEnableVertexAttribArray 2)

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

               ;; (define vertexColorLocation (glGetUniformLocation program "ourColor"))
               ;; (glUniform4f vertexColorLocation 1.0 0.0 0.0 1.0)

               (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_BYTE 0))
             (send canvas swap-gl-buffers)
             (sleep)
             (loop (+ frames 1) start-time))))
     (exit))))
