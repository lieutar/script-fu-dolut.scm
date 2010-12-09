;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define $CHAR-NULL (integer->char 0))
(define $GIMP?     (symbol-bound? `gimp-image-get-layers))


(define ($substring-before-c str item)
  (list->string
   (let ((buf (let iter ((rest (string->list str))
                         (buf  '()))
                (if (null? rest)
                    buf
                    (let ((c (car rest)))
                      (if (char=? c item)
                          buf
                          (iter (cdr rest) (cons c buf))))))))
    (if (null? buf) '() (reverse buf)))))


(define ($any->str obj)
  (let* ((str (make-string 4096 $CHAR-NULL))
         (out (open-output-string str)))
    (write obj out)
    ($substring-before-c str $CHAR-NULL)))


(define ($debug obj)
  (gimp-message ($any->str obj))
  obj)

(define ($msg . objs)
  (gimp-message (apply string-append
                       (map (lambda (o)
                              (string-append (if (string? o)
                                                 o
                                                 ($any->str o)) "\n"))
                            objs))))

;;
(define ($alist? o)
  (or (null? o)
      (and (pair? o)
           (pair? (car o))
           ($alist? (cdr o)))))

(define ($copy-list seq)
  (if (null? seq)
      '()
      (cons (car seq) ($copy-list (cdr seq)))))

(define ($append! seq . lists)
  (let iter ((seq   seq)
             (lists lists))
    (if (not (null? lists))
        (iter (set-cdr! (last-pair seq) (car lists))
              (cdr lists))))
  seq)

;;;
;;; functional utilities
;;;

(define ($curry proc . params)
  (lambda args (apply proc (append params args))))



;;;
;;; $db object
;;;

(define ($db-new)  (list 'db))

(define ($db? o)  (and (pair? o)(eq? (car o) 'db)))

(define ($$build-alist-r path val)
  (if (null? path)
      val
      (list (cons (car path) ($$build-alist-r (cdr path))))))


(define ($db-set! o path val)
  (let iter ((cur  o)
             (path (if (list? path) path (list path))))
    ($msg cur path)
    (let ((alist (cdr cur)))
      (letrec ((build (lambda (path)
                        (if (null? path)
                            val
                            (cons (cons (car path) (build (cdr path)))
                                  (if ($alist? alist)
                                      alist '()))))))
        (if (or (not ($alist? alist))
                (null? alist))
            ;; alist is empty
            (set-cdr! cur (build path))
            ;; alist is not empty
            (let* ((field (car path))
                   (next (assoc field alist)))
              (if (and next
                       (not (null? (cdr path))))
                  (iter next (cdr path))
                  (set-cdr! cur (build path))))))))
  val)

(define ($db-get o path . default)
  (let ((retval
         (let iter ((cur  (list o))
                    (path (cons 'db (if (list? path) path (list path)))))
           (if (null? path)
               cur
               (let* ((slot (car path))
                      (next (assoc slot cur)))
                 (if next
                     (iter (cdr next) (cdr path))
                     '()))))))
    (if (null? retval)
        (if (null? default) '() (car default))
        retval)))

;;;
;;; list utilities
;;;

(define ($drop-member item seq . opt-pred)
  (let iter ((seq seq)
             (p   (if (null? opt-pred equal? (car opt-pred)))))
    (if (null? seq)
        '()
        (let ((first (car seq))
              (next  (iter (cdr seq) p)))
          (if (p first item) next (cons first next))))))



;;;
;;; script-fu utilities
;;;

(define ($freeze-undo-with img proc)
  (gimp-image-undo-freeze img)
  (proc)
  (gimp-image-undo-thaw   img))

(define ($defcmd name description body menu-path author copyright modes
                 . argspec)
  (eval `(define ,(string->symbol name) ,body) (interaction-environment))
  (apply script-fu-register
         name
         menu-path
         description
         author
         author
         copyright
         modes
         argspec))

(define ($define symbol value)
  (eval `(define ,symbol ',value) (interaction-environment)))

(if
 $GIMP?
 (let ((get-by-name
        (lambda (get-collection get-name)
          (lambda (img name)
            (call/cc
             (lambda (return)
               (let ((collection (vector->list (cadr (get-collection img)))))
                 (for-each
                  (lambda (item)
                    (if (equal? (car (get-name item)) name)(return item)))
                  collection))
               #f))))))
   ($define '$get-layer-by-name (get-by-name gimp-image-get-layers
                                             gimp-layer-get-name))
   ($define '$get-channel-by-name (get-by-name gimp-image-get-channels
                                               gimp-channel-get-name))
   ;;($define '$get-vector-by-name (get-by-name gimp-image-get-vectors
   ;;                                           gimp-vector-get-name))
   ))



(define ($touple<? a b pred<? pred=? order-spec)
  (call/cc
   (lambda (cont)
     (for-each
      (lambda (p)
        (let* ((spec   (if (list? p) p (list p #f)))
               (pos    (car  spec))
               (rev?   (cadr spec))
               (last   (cddr spec))
               (round  (or (if (null? last)
                               #f
                               (let ((proc (car last)))
                                 (if (procedure? proc) proc #f)))
                           (lambda (a) a)))
               (children  (if (null? last)
                              '()
                              (cadr last)))
               (return (if rev?
                           (lambda (v) (cont (not v)))
                           cont))
               (a-item (round (nth pos a)))
               (b-item (round (nth pos b))))
          (if
           (pred=? a-item b-item)
           (if (null? children)
               #f
               (return ($touple<? a b pred<? pred=? children)))
           (return (pred<? a-item b-item)))))
      order-spec)
     #f)))

(define ($sort lst pred)
  (case (length lst)
    ((0) '())
    ((1) lst)
    ((2) (let ((a (car lst))
               (b (cadr lst)))
           (if (pred a b) (list a b) (list b a))))
    (else
     (let iter ((s    (car lst))
                (rest (cdr lst))
                (lt   '())
                (gt   '()))
       (if (null? rest)
           (append ($sort lt pred) (cons s ($sort gt pred)))
           (apply iter
                  s
                  (cdr rest)
                  (let ((i (car rest)))
                    (if (pred i s)
                        (list (cons i lt) gt)
                        (list lt          (cons i gt))))))))))

;;;
;;; color utilities
;;;

(define $rgb->hsv
  (let* ((VALUE-R 0.29891)
         (VALUE-G 0.58661)
         (VALUE-B 0.11448))         
    (lambda (rgb)
      (let* ((r (/ (car   rgb) 255))
             (g (/ (cadr  rgb) 255))
             (b (/ (caddr rgb) 255))
             (max-item (max r g b))
             (min-item (min r g b))
             (d  (- max-item min-item))
             (/d (if (zero? d)
                     (lambda (n) 0)
                     (lambda (n) (/ n d))))
             (max-sym  (if (> r g)
                           (if (> r b) 'r 'b)
                           (if (> g b) 'g (if (= r b) 'r 'b))))
             (h (case max-sym
                  ((r)
                   (+ 0
                      (* (/d (- g b)) (/ 1 6))))
                  ((g)
                   (+ (/ 1 3)
                      (* (/d (- b r)) (/ 1 6))))
                  ((b)
                   (+ (* 2 (/ 1 3))
                      (* (/d (- r g)) (/ 1 6))))))

             (s (if (zero? max-item)
                    0
                    (/ (- max-item min-item) max-item)))

             (v (+ (* VALUE-R r)
                   (* VALUE-G g)
                   (* VALUE-B b))))
        (list (if (< h 0) (+ 1 h) h) s v)))))

