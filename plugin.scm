(use-modules (srfi srfi-26))
(use-modules (ice-9 regex))

(weechat:register "r2tg_nickconvert"
                  "zv <zv@nxvr.org>"
                  "1.0"
                  "GPL3"
                  "A small plugin to convert the names of users of r2tg to their real names"
                  ""
                  "")

(weechat:print "" "Initialize r2tg nickconvert")

(define *gateways*
  '(("#radare" . "r2tg")))

(define *r2tg-msg-regex* ":(r2tg)!~.*?@\\S* PRIVMSG #radare :(<(.*?)>) .*")


(define (extract-real-message msg)
  "Converts a string like '<pancake> adsfadfadfasdf' to a tuple"
  (let* ([nick-ending (string-index msg #\>)]
         [nick (substring msg 1 nick-ending)]
         [msg (substring msg nick-ending (string-length msg))])
    (cons nick msg)))

(define (replace-privmsg msg)
  "A function to replace the privmsg sent by r2tg"
  (let ((result (string-match *r2tg-msg-regex* msg)))
    (if result
        (let* ([nth-match (cut match:substring result <>)]
               ;; take everything after username before message
               [username (nth-match 1)]
               [real-username (nth-match 3)]
               ;; extract everything after the fake r2tg username
               [message (string-copy msg
                                     (+ 1 ; skip the inserted space
                                        (match:end result 2))
                                     (string-length msg))]
               ;; extract everything before the message but after the username
               [hostmask (string-copy msg (match:end result 1) (match:start result 2))]
               ;; actually join our string
               [new-msg (string-join (list real-username hostmask message) "")])
          new-msg)
        msg)))

(define (privmsg-modifier data modifier-data server msg)
  (if (equal? server "freenode") (replace-privmsg msg)
      msg))

;; (weechat:hook_modifier "irc_in_privmsg" "privmsg-modifier" "")
(define test-msg ":r2tg!~user@static.213-239-215-115.clients.your-server.de PRIVMSG #radare :<Maijin> Just build using ./sys/asan.sh and paste log caused by your issue")
(define test-nonmsg ":aiju!~aiju@unaffiliated/aiju PRIVMSG #cat-v :branch_: a large part of modern human intelligence is learned through culture :)")
