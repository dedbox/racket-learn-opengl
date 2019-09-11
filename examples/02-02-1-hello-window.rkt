#lang racket/gui

(require opengl
         racket/class)

(define gl-canvas%
  (class canvas%
    (inherit with-gl-context)
    (field [done? #f])
    (define/public (initialize)
      (with-gl-context (λ () (gl-version-at-least? '(3 3 0)))))
    (define/public (terminate)
      (void))
    (define/override (on-size width height)
      (with-gl-context (λ () (glViewport 0 0 width height))))
    (define/override (on-char event)
      (match* ((send event get-key-code)
               (send event get-key-release-code))
        [(pcode rcode) (writeln `(KEY ,pcode ,rcode))]))
    (super-new [style '(gl no-autoclear)])))

(module+ main
  (define frame (new frame% [label "LearnOpenGL"]))
  (define canvas
    (new gl-canvas% [parent frame] [min-width 800] [min-height 600]))
  (unless (send canvas initialize)
    (error "Failed to initialize OpenGL!"))
  (send frame show #t)
  (send canvas focus)
  (thread
   (λ ()
     (collect-garbage)
     (let loop ()
       (unless (get-field done? canvas)
         (collect-garbage 'incremental)
         (send canvas swap-gl-buffers)
         (sleep)
         (loop))
       (send canvas terminate)
       (exit)))))
