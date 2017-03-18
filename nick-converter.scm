;; -*- geiser-scheme-implementation: 'guile -*-

(use-modules ((srfi srfi-1)
              #:select (any)))
(use-modules ((srfi srfi-26)
              #:select (cut)))
(use-modules (ice-9 regex))
(use-modules (ice-9 hash-table))
(use-modules (ice-9 match))

(if (defined? 'weechat:register)
    (weechat:register "gateway_nickconverter"
                      "zv <zv@nxvr.org>"
                      "1.0"
                      "GPL3"
                      "Convert usernames of gateway connections their real names"
                      ""
                      ""))

;; `user-prefix' is a distinguishing username prefix for 'fake' users
(define *user-prefix* "^")
(define *gateway-config* "gateways")

(define (print . msgs)
  (if (defined? 'weechat:print)
      (weechat:print "" (apply format (cons #f msgs)))))

;; A regular expression must have the gateway username in the first matchgroup,
;; the "real" username in the 3rd, and the real-username along with it's enclosing
;; brackets in the 2nd
(define *gateway-regexps* (make-hash-table))

(define (process-network-infolist)
  "Convert the internal user-defined servername to the 'true' servername
returned during /version"
  (define il (weechat:infolist_get "irc_server" "" ""))

  ;; pull the network field out of the list of /VERSION results
  (define (extract-network result)
    (if (null? result) #f
        (match (string-split (car result) #\=)
          [("NETWORK" network) network]
          [_ (extract-network (cdr result))])))

  ;; pull out a '(name network-name) pair from an infolist str
  (define (process return-code)
    (if (= return-code 0) '()
        (let* ((name      (weechat:infolist_string il "name"))
               (isupport  (weechat:infolist_string il "isupport"))
               (reply     (string-split isupport #\space))
               (network   (or (extract-network reply)
                              ;; if no network, use local name
                              name)))
          (cons
           (cons name network)
           (process (weechat:infolist_next il))))))

  (process (weechat:infolist_next il)))

(define *hostname-table* (alist->hash-table (process-network-infolist)))

(define (replace-privmsg msg gateways)
  "A function to replace the privmsg sent by by a gateway "
  (let* ((match? (cut regexp-exec <> msg))
         (result (any match? gateways)))
    (if result
        (let* ([nth-match (cut match:substring result <>)]
               ;; take everything after username before message
               [username (nth-match 1)]
               [real-username (nth-match 3)]
               ;; extract everything after the fake r2tg username
               [message (string-copy msg
                                     ;; skip the inserted space
                                     (+ 1 (match:end result 2))
                                     (string-length msg))]
               ;; extract everything before the message but after the username
               [hostmask (string-copy msg
                                      (match:end result 1)
                                      (match:start result 2))])
          (string-append ":" *user-prefix* real-username hostmask message))
        msg)))

(define (server->gateways server)
  (hash-ref *gateway-regexps* (hash-ref *hostname-table* server)))

(define (privmsg-modifier data modifier-type server msg)
  ;; fetch the appropriate gateway by server
  (let ((gateways (server->gateways server)))
    (if gateways
        (replace-privmsg msg gateways)
        msg)))

(define (process-weechat-option opt)
  "Takes in the application-define weechat-options and emits a server and
matching regular expression.

# Example

scheme@(guile-user)> (process-weechat-option \"(freenode #radare r2tg <NICK>)\")
$1 = '(\"freenode\" . (make-regexp \":(r2tg)!\\S* PRIVMSG #radare :(<(\\S*?)>) .*\")))"

  (define (make-gateway-regexp gateway-nick channel mask)
    "Build a regular expression that will match the nick, channel and \"<NICK>\"-style mask"
    (let ((mask-regexp ;; replace <NICK> with <(\\S*?)>
           (regexp-substitute/global #f "NICK" mask 'pre "(\\S*?)" 'post "")))
      (make-regexp (format #f ":(~a)!\\S* PRIVMSG ~a :(~a)" gateway-nick channel mask-regexp))))

  (define (extract-fields str)
    "Guile's regex engine doesn't support non-greedy matchers (wtf?), so I had
to write this.

# Example
scheme@(guile-user)> (extract-fields \"(freenode #radare r2tg <NICK>)\")
$1 = (\"freenode\" \"#radare\" \"r2tg\" \"<NICK>\")
"
    (let* ((range-end  (λ (range) (+ 1 (cdr range))))
           (find-space (λ (end) (string-index str #\space end)))
           ;; opening (first) and closing (last) parenthesis
           (opening-par   (string-index str #\())
           (closing-par   (string-index str #\)))
           ;; extract the range of each
           (server        (cons (+ 1 opening-par) (find-space 0)))
           (channel       (cons (range-end server) (find-space (range-end server))))
           (gateway-nick  (cons (range-end channel) (find-space (range-end channel))))
           (mask          (cons (range-end gateway-nick) closing-par)))

      ;; and then get the strings
      (map (λ (window) (substring str (car window) (cdr window)))
           (list server channel gateway-nick mask))))


  (let* ((fields (extract-fields opt))
         (server  (list-ref fields 0))
         (channel (list-ref fields 1))
         (gateway-nick (list-ref fields 2))
         (mask    (list-ref fields 3)))
    (cons server (make-gateway-regexp gateway-nick channel mask))))

(define (split-gateways config)
  "Push our elts onto the stack to extract our configs

# Example
scheme@(guile-user)> (gateways-config->regex \"(freenode #radare r2tg <NICK>)(* * slack-irc-bot NICK:)\")
$1 = (\"(freenode #radare r2tg <NICK>)\" \"(* * slack-irc-bot NICK:)\")
"
  (define (loop stk current rest)
    (if (string-null? rest) (cons current '())
        (let* ((head (string-ref rest 0))
               (nrest (string-drop rest 1))
               (ncurrent (string-append current (string head))))
          (cond
           [(and (null? stk) (not (string-null? current)))
            (cons current (loop stk "" rest))]
           [(eq? head #\() (loop (cons #\( stk) ncurrent nrest)]
           [(eq? head #\)) (loop (cdr stk) ncurrent nrest)]
           ;; skip characters if our stk is empty
           [(null? stk) (loop stk current nrest)]
           [else (loop stk ncurrent nrest)]))))

  (loop '() "" config))

(define (fetch-weechat-gateway-config)
  (if (defined? 'weechat:config_get_plugin)
      (weechat:config_get_plugin *gateway-config*)
      ;; a test string
      "(freenode #radare r2tg <NICK>)(freenode #test-channel zv-test <NICK>)"))

(define (assign-gateways-regex)
  "Fetch our weechat gateway configuration and assign it to our local regexps"
  (let* ((config_str (fetch-weechat-gateway-config))
         (config_lst (split-gateways config_str))
         (gateways   (map process-weechat-option config_lst)))
    ;; for each gateway, add it to our `*gateway-regexps*' ht
    (for-each
     (λ (gt)
       (let* ((server    (car gt))
              (new-regex (cdr gt))
              (server-regexps (hash-ref *gateway-regexps* server '())))
         (hash-set! *gateway-regexps* server
                     (cons new-regex server-regexps))))
     gateways)))

;; Initialize our settings
(if (not (= 1 (weechat:config_is_set_plugin *gateway-config*)))
    (weechat:config_set_plugin
     *gateway-config*
     "(freenode #radare r2tg <NICK>)(freenode #test-channel zv-test <NICK>)"))

(assign-gateways-regex)

(weechat:hook_modifier "irc_in_privmsg" "privmsg-modifier" "")
(print "Gateway Nickconverter by zv <zv@nxvr.org>")

