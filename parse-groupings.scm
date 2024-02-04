(use-modules (ice-9 match))

;; TODO debug and then do with pattern matching

(define (parse-file filename)
  (let ((lines (file->lines filename)))
    (let loop ((lines lines)
               (result '())
               (current-block '())
               (current-list '()))
      (cond
       ;; End of file
       ((null? lines) (reverse (cons (reverse current-block) result)))
       ;; Empty line, possibly a block separator
       ((string=? (car lines) "")
        (if (and (null? (cdr lines)) (string=? (cadr lines) ""))
            (loop (cddr lines) ; Skip next line as well, assuming it's also empty
                  (if (null? current-list)
                      result
                      (cons (reverse current-block) result))
                  '()
                  '())
            (loop (cdr lines) result current-block current-list)))
       ;; Line with heading
       ((= (string-ref (car lines) 0) #\space)
        (loop (cdr lines) result current-block (cons (string-trim (car lines)) current-list)))
       ;; Line with bullet point
       (else
        (loop (cdr lines)
              result
              (cons (list (car lines) (reverse current-list)) current-block)
              '()))))))

(define (file->lines filename)
  (call-with-input-file filename
    (lambda (port)
      (let reading ((lines '()))
        (let ((line (read-line port 'eof)))
          (if (eq? line 'eof)
              (reverse lines)
              (reading (cons line lines))))))))

(define (string-trim str)
  (let ((s (regexp-replace #px"^[ ]+" str "")))
    (regexp-replace #px"[ ]+$" s "")))
