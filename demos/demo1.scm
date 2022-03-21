(define the-binary (load-binary "demos/Ref"))
(display (format-binary-header the-binary))
(newline)
(define discovery-info (discover-functions the-binary))
