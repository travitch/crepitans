(define the-binary (load-binary "demos/Ref"))
(display (format-binary-header the-binary))
(newline)
(define discovery-info (discover-functions the-binary))
(define funcs (discovered-functions discovery-info))
(display "number of funcs: ")
(display (vector-length funcs))
(newline)
(vector-for-each
 (lambda (func) (begin
                  (display (function-name func))
                  (newline)))
 funcs)
