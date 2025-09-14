(import
 (owl toplevel)
 (owl args)
 (prefix (ext sqlite io) s3/))

(define *port* 7762)
(define *db-name* "lightp.db")

(define timeout 1)             ; timeout for checking light levels
(define reconnect 30)          ; ask client to reconnect after
(define db-auto-save-every 60) ; auto save database ever _ seconds

(define prim!
  (case-lambda
   ((n a b c)  (sys-prim n a b c))
   ((n a b)    (sys-prim n a b #f))
   ((n a)      (sys-prim n a #f #f))
   ((n)        (sys-prim n #f #f #f))))

(define (init-fast! dev)
  (prim! 1000 (c-string dev)))

(define (query-average)
  (prim! 1001))

(define command-line-rules
  (cl-rules
   `((help  "-h" "--help")
     (level "-l" "--level"  has-arg comment "Light tuning level (0-255)" default "128")
     (dev   "-d" "--device" has-arg comment "Camera device"              default "/dev/video1")
     )))

(define (state->bytes state)
  (list
   (case state
     ('dark  #\a)
     ('light #\b)
     ('recon #\R)
     (else   0))))

(define (make-client fd ip)
  (print "Making client with fd " fd)
  (let ((thrname (string->symbol (str "client@" ip "-" (time-ns)))))
    (thread
     (begin
       (sleep (* 1000 reconnect))
       (print "will ask to recon")
       (mail thrname (tuple 'kys! 'recon))))
    (thread
     thrname
     (let loop ((state #f))
       (lets ((who m (next-mail)))
         (tuple-case m
           ((kys! st)
            (write-bytes fd (state->bytes st))
            (close-port fd)
            (mail 'master (tuple 'drop-client! thrname)))
           ((state new _)
            (if (not (eq? state new))
                (begin
                  (write-bytes fd (state->bytes new))
                  (loop new))
                (loop state)))
           (else
            (print "unknown message: " m)
            (loop state))))))))

(define (start-master!)
  (thread
   'master
   (let loop ((clients #n))
     (lets ((who m (next-mail)))
       (tuple-case m
         ((add-client! c)
          (print "[master] new client " c)
          (loop (cons c clients)))
         ((drop-client! c)
          (print "[master] dropping client " c)
          (loop (filter (λ (x) (not (equal? x c))) clients)))
         ((announce state value)
          (print "[master] announcing " state)
          (for-each (λ (c) (mail c (tuple 'state state value))) (cons 'db clients))
          (loop clients))
         (else
          (loop clients)))))))

(define (start-server!)
  (start-master!)
  (let ((sock (open-socket *port*)))
    (thread
     'server
     (let loop ()
       (lets ((ip fd (tcp-client sock)))
         (mail 'master (tuple 'add-client! (make-client fd ip)))
         (loop))))))

(define (init-db! ptr)
  (s3/execute ptr "create table if not exists avgs (id integer primary key, timestamp text, bavg text)"))

(define (get-db)
  (let ((db (s3/open *db-name*)))
    (init-db! db)
    (print "[db] got db at " db)
    db))

(define (start-db-process!)
  (thread
   'db-auto-save
   (let loop ()
     (sleep (* 1000 db-auto-save-every))
     (mail 'db (tuple 'save!))
     (loop)))
  (thread
   'db
   (let loop ((ptr (get-db)))
     (lets ((who m (next-mail)))
       (tuple-case m
         ((state _ s)
          (print "[db] saving " s)
          (print "values: " (list (time-ms) (format #f "~,4f" s)))
          (if (s3/execute ptr "insert into avgs (timestamp, bavg) values (?,?)" (list (str (time-ms)) (format #f "~,4f" s)))
              (loop ptr)
              (loop (get-db))))
         ((save!)
          (print "[db] closing & re-opening database as per 'save!")
          (s3/close ptr)
          (loop (get-db)))
         (else
          (print "[db] unknown message " m)
          (loop ptr)))))))

(λ (args)
  (process-arguments
   (cdr args) command-line-rules "you lose"
   (λ (opt extra)
     (when (get opt 'help #f)
       (print "Usage: " (car args) " [args]")
       (print-rules command-line-rules)
       (halt 0))

     (print "starting w/ fast, ret=" (init-fast! (get opt 'dev "bug, beware")))

     (start-db-process!)
     (start-server!)

     (let* ((level (string->number (get opt 'level 'bug))))
       (let loop ()
         (let ((avg (query-average)))
           (mail 'master (tuple 'announce (if (> avg level) 'light 'dark) avg))
           (sleep (* 1000 timeout))
           (loop)))))))
