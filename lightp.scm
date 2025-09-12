(import
 (owl toplevel)
 (owl args)
 (prefix (ext sqlite io) s3/))

(define *port* 7762)
(define *db-name* "lightp.db")

(define timeout 1)    ; timeout for checking light levels
(define reconnect 30) ; ask client to reconnect after

(define W 160)
(define H 120)
(define *bufsiz* (* W H 2))
(define *cmd* "video -e yuy2 -r 1 -s ~ax~a -f ~a -o -")

(define command-line-rules
  (cl-rules
   `((help  "-h" "--help")
     (level "-l" "--level"  has-arg comment "Light tuning level (0-255)" default "128")
     (dev   "-d" "--device" has-arg comment "Camera device"              default "/dev/video1")
     )))

(define (bavg b)
  (let* ((len (bytevector-length b))
         (sum
          (let loop ((sum 0) (n 0))
            (if (>= n len)
                sum
                (loop
                 (+ sum (band #xf0 (bytevector-u8-ref b n)))
                 (+ n 2))))))
    (/ sum (* 160 120))))

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

(define (start-db-process!)
  (let ((ptr (s3/open *db-name*)))
    (init-db! ptr)
    (thread
     'db
     (let loop ()
       (lets ((who m (next-mail)))
         (tuple-case m
           ((state _ s)
            (print "[db] saving " s)
            (print "values: " (list (time-ms) (format #f "~,4f" s)))
            (s3/execute ptr "insert into avgs (timestamp, bavg) values (?,?)" (list (str (time-ms)) (format #f "~,4f" s))))
           (else
            (print "[db] unknown message " m)))
         (loop))))))

(λ (args)
  (process-arguments
   (cdr args) command-line-rules "you lose"
   (λ (opt extra)
     (when (get opt 'help #f)
       (print "Usage: " (car args) " [args]")
       (print-rules command-line-rules)
       (halt 0))

     (start-db-process!)
     (start-server!)

     (let* ((level (string->number (get opt 'level 'bug)))
            (cmd (format #f *cmd* W H (get opt 'dev 'bug))))
       (lets ((r _ (popen cmd)))
         (let loop ((last (time-ms)))
           (let ((b (try-get-block r *bufsiz* #t))
                 (time (time-ms)))
             (if (> time (+ last (* 1000 timeout)))
                 (begin
                   (if-lets ((_ (bytevector? b))
                             (avg (bavg b)))
                     (mail 'master (tuple 'announce (if (> avg level) 'light 'dark) avg)))
                   (loop time))
                 (loop last)))))))))
