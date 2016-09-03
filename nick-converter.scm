(use-modules ((srfi srfi-1)
              #:select (any)))
(use-modules (srfi srfi-26))
(use-modules (ice-9 regex))

(if (defined? 'weechat:register)
    (begin
      (weechat:register "gateway-nickconverter"
                      "zv <zv@nxvr.org>"
                      "1.0"
                      "GPL3"
                      "Convert usernames of gateway connections their real names"
                      ""
                      "")
      (weechat:print "" "Initialize Gateway Nickconverter")))

;; A regular expression must have the gateway username in the first matchgroup,
;; the "real" username in the 3rd, and the real-username along with it's enclosing
;; brackets in the 2nd
(define *gateway-regexps*
  (alist->hash-table
   `(("freenode" . (;; r2tg
                    ,(make-regexp ":(r2tg)!\\S* PRIVMSG #radare :(<(.*?)>) .*")
                    ;; slack-irc-bot
                    ,(make-regexp ":(slack-irc-bot)!\\S* PRIVMSG #\\S* :(<(.*?)>) .*")
                    ;; test
                    ,(make-regexp ":(zv-test)!\\S* PRIVMSG #test-channel :(<(.*?)>) .*"))))))

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
          (string-append ":" real-username hostmask message))
        msg)))

(define (server->gateways server)
  (hash-ref *gateway-regexps* server))

(define (privmsg-modifier data modifier-type server msg)
  ;; fetch the appropriate gateway by server
  (let ((gateways (server->gateways server)))
    (if gateways
        (replace-privmsg msg gateways)
        msg)))

(if (defined? 'weechat:hook_modifier)
    (weechat:hook_modifier "irc_in_privmsg" "privmsg-modifier" ""))

(define test-msg ":r2tg!~user@static.213-239-215-115.clients.your-server.de PRIVMSG #radare :<Maijin> Just build using ./sys/asan.sh and paste log caused by your issue")
(define test-nonmsg ":aiju!~aiju@unaffiliated/aiju PRIVMSG #cat-v :branch_: a large part of modern human intelligence is learned through culture :)")
(define test-zv ":zv-test!43a46046@gateway/web/freenode/ip.67.164.96.70 PRIVMSG #test-channel :<Maijin> adfasfaf")
