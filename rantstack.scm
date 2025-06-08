#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/http)

;; Constants
(define POST-DEATH 1800)
(define NAME "RantStack")
(define DEFAULT-TITLE "I GOT SOMETHING TO SAY!!!")
(define DEFAULT-AUTHOR "Some Rando")
(define MAX-POSTS 20)
(define MAX-LENGTH 1500)
(define ABOUT (string-append NAME " is a place for temporary rants." 
                             " All posts are stored only in memory " 
                             " and deleted after 30 minutes. "))
(define GH-URL "https://glitch.com/edit/#!/rantstack")
(define GLYPH "glyphicon glyphicon-fire")

;; Structs
; The stack is a list of posts
(struct stack (posts) #:mutable)

; Each post has a title, author, body text, and the current time in seconds (created)
(struct post (title author body created) #:mutable)

;; Establish the RANTS
; this is the initial stack state.
(define RANTS (stack (list (post (string-append "Welcome to " NAME)
                                DEFAULT-AUTHOR
                                "This is a place for rants. All is forgotten."
                                (current-seconds)))))

;; Functions
; checks if the current time is past the expiration date of the post
(define (old-post? p)
  (> (current-seconds) (+ (post-created p) POST-DEATH)))

; Looks over the stack, removing old posts and dropping any over the maximum
(define (check-posts s)
  (set-stack-posts! RANTS (for/list ((i (stack-posts s))
                                    (j (in-range MAX-POSTS))
                                    #:unless (old-post? i))
                           i)))

; adds a post to the stack given title, name, and body
(define (add-post title name body)
  (set-stack-posts! RANTS (cons (post (if (string=? title "") DEFAULT-TITLE title)
                                     (if (string=? name "") DEFAULT-AUTHOR name)
                                     body
                                     (current-seconds))
                               (stack-posts RANTS))))

; Renders an error bar at the top of the page with the given message, unless no error is present
(define (render-error err)
  (if err 
      `(div ((class "alert alert-dismissable alert-danger"))
            (button ((type "button") 
                     (class "close")
                     (data-dismiss "alert")))
            (strong "Hey! Listen! ")
            ,err)
      ""))

; renders a post. Posts are rendered as Bootstrap panels with title, body, and author
(define (render-post p)
  `(div ((class "panel panel-default"))
        (div ((class "panel-heading")) (h4 ,(post-title p)))
        (div ((class "panel-body")) (p ,(post-body p)))
        (div ((class "panel-footer")) (em ,(post-author p)))))

; Creates a template for a container containing all of the current stack posts,
; after clearing old or excess posts
(define (render-posts s)
  (check-posts RANTS)
  `(div ((class "container"))
        ,@(map render-post (stack-posts s))))

; Template for the navbar at the top of the page
(define (render-bar)
  `(nav ((class "navbar navbar-default")
         (role "navigation"))
        (div ((class "container-fluid"))
             (div ((class "navbar-header"))
                  (a ((class "navbar-brand")
                      (href "#"))
                     (span ((class ,GLYPH))) ,NAME)))))

; Template for the entry box, rendered as a Bootstrap jumbotron
(define (render-jumbotron)
  `(div ((class "jumbotron"))
        (fieldset 
         (legend "Add Your Rant")
         (form 
          (div ((class "row"))
               (div ((class "col-md-8"))
                    (div ((class "form-group"))
                         (label ((for "Title") (class "control-label")) "Title")
                         (input ((type "text") 
                                 (class "form-control") 
                                 (name "Title") 
                                 (placeholder ,DEFAULT-TITLE)))))
               (div ((class "col-md-4"))
                    (div ((class "form-group"))
                         (label ((for "Title") (class "control-label")) "Name")
                         (input ((type "text") 
                                 (class "form-control") 
                                 (name "Name") 
                                 (placeholder ,DEFAULT-AUTHOR))))))
          (div ((class "row"))
               (div ((class "form-group"))
                    (label ((for "Post") (class "col-lg-2 control-label")) "Your Rant")
                    (div ((class "col-lg-10"))
                         (textarea ((class "form-control")
                                    (rows "5") 
                                    (name "Post")
                                    (placeholder 
                                     ,(string-append "Your rant may not exceed " 
                                                     (number->string MAX-LENGTH)
                                                     " characters.")))))))
          (div ((class "row"))
               (div ((class "col-lg-10 col-lg-offset-2"))
                    (div ((class "form-group"))
                         (button ((type "submit") (class "btn btn-primary")) "Submit")
                         (button ((type "reset") (class "btn btn-default")) "Cancel"))))))))

; Generates a response containing the main page
(define (render-page [err #f])
  (response/xexpr
   #:preamble #"<!DOCTYPE html>"
   `(html (head (title ,NAME)
                (link ((rel "stylesheet")
                       (href "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css")))
                (link ((rel "stylesheet")
                       (href "//maxcdn.bootstrapcdn.com/bootswatch/3.2.0/journal/bootstrap.min.css")))
                (script ((src "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"))))
          (body (div ((class "col-lg-10"))
                     ,(render-bar)
                     ,(render-error err)
                     (div ((class "col-lg 8"))
                          (div ((class "container")) ,(render-jumbotron))
                          ,(render-posts RANTS))))
          (footer (div ((class "container well well-sm")) 
                          (p ((class "text-center"))
                             ,ABOUT (a ((href ,GH-URL)) "Source on Glitch.")))))))

; The main request handler, parsing out posts as needed or calling the response generator
(define (show-page req [err #f])
  (let ((bindings (request-bindings req))
        (responder (lambda () (render-page err))))
    (if (for/and ((i '(title name post)))
          (exists-binding? i bindings))
        (cond ((string=? (extract-binding/single 'post bindings) "")
               (show-page (redirect/get) "Post body cannot be empty"))
              ((> (string-length (extract-binding/single 'post bindings)) MAX-LENGTH)
               (show-page (redirect/get) "Post exceeded maximum length"))
              (else (begin 
                      (add-post (extract-binding/single 'title bindings)
                                (extract-binding/single 'name bindings)
                                (extract-binding/single 'post bindings))
                      (show-page (redirect/get)))))
        (send/back (responder)))))

; Initial request reciever
(define (start req)
  (show-page req))

;; Main Server startup
(serve/servlet start
               #:servlet-regexp #rx""
               #:servlet-path "/"
               #:port (string->number (getenv "PORT"))
               #:launch-browser? #f)