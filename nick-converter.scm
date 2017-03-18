;; -*- geiser-scheme-implementation: 'guile -*-

(use-modules ((srfi srfi-1)
              #:select (any)))
(use-modules ((srfi srfi-26)
              #:select (cut)))
(use-modules (ice-9 regex))
(use-modules (ice-9 hash-table))
(use-modules (ice-9 match))

;; A test-harness for checking if we are inside weechat
(define-syntax if-weechat
  (syntax-rules ()
    ((_ conseq alt) (if (defined? 'weechat:register) conseq alt))
    ((_ conseq) (if (defined? 'weechat:register) conseq))))

(if-weechat
 (weechat:register "gateway_nickconverter" "zv <zv@nxvr.org>" "1.0" "GPL3"
                   "Convert usernames of gateway connections their real names" "" ""))

;; `user-prefix' is a distinguishing username prefix for 'fake' users
(define *user-prefix* "^")
(define *gateway-config* "gateways")
(define *default-irc-gateways* "(freenode #radare r2tg <NICK>)(freenode #test-channel zv-test <NICK>)")

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

;; This is a table that maps a weechat network 'name' to it's IRC-style hostname
(define *hostname-table* (alist->hash-table '(("freenode" . "freenode"))))
(if-weechat
 (set! *hostname-table* (alist->hash-table (process-network-infolist))))

(define (replace-privmsg msg gateways)
  "A function to replace the PRIVMSG sent by by a gateway "
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
  "The hook for all PRIVMSGs in Weechat"
  (let ((gateways (server->gateways server)))
    (if gateways
        (replace-privmsg msg gateways)
        msg)))

(define* (make-gateway-regexp gateway-nick channel mask #:optional emit-string)
  "Build a regular expression that will match the nick, channel and \"<NICK>\"-style mask"
  (let* ([mask-regexp ;; replace <NICK> with <(\\S*?)>
          (regexp-substitute/global #f "NICK" mask 'pre "(\\S*?)" 'post "")]
         [composed-str (format #f ":(~a)!\\S* PRIVMSG ~a :(~a)" gateway-nick channel mask-regexp)])
    (if emit-string composed-str (make-regexp composed-str))))

(define (extract-gateway-fields str)
  "This is a hack around Guile's non-greedy matchers.

  # Example
  scheme@(guile-user)> (extract-gateway-fields \"(freenode #radare r2tg <NICK>)\")
  $1 = (\"freenode\" \"#radare\" \"r2tg\" \"<NICK>\")"
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

(define* (process-weechat-option opt #:optional emit-string)
  "Takes in the application-define weechat-options and emits a server and
matching regular expression.

The optional parameter `emit-string' controls if a string or a compiled regular
expression is returned.

# Example

scheme@(guile-user)> (process-weechat-option \"(freenode #radare r2tg <NICK>)\")
$1 = '(\"freenode\" . (make-regexp \":(r2tg)!\\S* PRIVMSG #radare :(<(\\S*?)>) .*\")))"
  (let* ((fields (extract-gateway-fields opt))
         (server  (list-ref fields 0))
         (channel (list-ref fields 1))
         (gateway-nick (list-ref fields 2))
         (mask    (list-ref fields 3)))
    (cons server (make-gateway-regexp gateway-nick channel mask emit-string))))


(define (split-gateways config)
  "Push our elts onto the stack to extract our configs

# Example
scheme@(guile-user)> (split-gateways \"(freenode #radare r2tg <NICK>)(* * slack-irc-bot NICK:)\")
$1 = (\"(freenode #radare r2tg <NICK>)\" \"(* * slack-irc-bot NICK:)\")
"
  (define (process stk current rest)
    (if (string-null? rest) (cons current '())
        (let* ((head (string-ref rest 0))
               (nrest (string-drop rest 1))
               (ncurrent (string-append current (string head))))
          (cond
           [(and (null? stk) (not (string-null? current)))
            (cons current (process stk "" rest))]
           [(eq? head #\() (process (cons #\( stk) ncurrent nrest)]
           [(eq? head #\)) (process (cdr stk) ncurrent nrest)]
           ;; skip characters if our stk is empty
           [(null? stk) (process stk current nrest)]
           [else (process stk ncurrent nrest)]))))

  (process '() "" config))

(define (fetch-weechat-gateway-config)
  "Extract the gateway configuration string"
  (if-weechat (weechat:config_get_plugin *gateway-config*)
              *default-irc-gateways*))

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

;; Initialize our weechat settings & privmsg hook
(if-weechat
 (begin
   (if (not (= 1 (weechat:config_is_set_plugin *gateway-config*)))
       (weechat:config_set_plugin
        *gateway-config*
        *default-irc-gateways*))

   (weechat:hook_modifier "irc_in_privmsg" "privmsg-modifier" "")))

;; Setup our gateways->regex map
(assign-gateways-regex)

(print "Gateway Nickconverter by zv <zv@nxvr.org>")
