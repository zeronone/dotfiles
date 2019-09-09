;;; init.el -*- lexical-binding: t; -*-

;; I've swapped these keys on my keyboard
;;(setq x-super-keysym 'meta
;;      x-alt-keysym   'alt)

(doom! :input
       ;japanese

       :completion
       (company           ; the ultimate code completion backend
         ;+auto
         +childframe
         ;+tng
       )
       ;;helm              ; the *other* search engine for love and life
       ;;ido              ; the other *other* search engine...
       (ivy              ; a search engine for love and life
         +fuzzy
         ;+childframe
       )

       :ui
       workspaces        ; tab emulation, persistence & separate workspaces
       deft
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       modeline     ; a snazzy Atom-inspired mode-line
       ;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;; evil-goggles      ; display visual hints when editing in evil
       fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       nav-flash         ; blink the current line after jumping
       ;neotree           ; a project drawer, like NERDTree for vim
       ophints
       treemacs          ; a project drawer, like neotree but cooler
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       (pretty-code       ; replace bits of code with pretty symbols
        +iosevka
        )
       ;tabbar            ; FIXME an (incomplete) tab bar for Emacs
       unicode           ; extended unicode support for various languages
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       vc-gutter
       window-select     ; visually switch windows
       indent-guides

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       fold
       format
       ;;(format +onsave)  ; automated prettiness
       multiple-cursors  ; editing in many places at once
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;file-templates    ; auto-snippets for empty files
       ; word-wrap     ; slows down

       :emacs
       (dired            ; making dired pretty [functional]
         +ranger         ; bringing the goodness of ranger to dired
         +icons          ; colorful icons for dired-mode
        )
       electric          ; smarter, keyword-based electric-indent
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;term              ; terminals in Emacs
       vterm
       ;;eshell            ; a consistent, cross-platform shell (WIP)

       :tools
       ansible
       debugger
       direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       eval              ; run code, run (also, repls)
       flycheck
       flyspell
       gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       lsp
       macos             ; MacOS-specific commands
       make              ; run make tasks from Emacs
       magit             ;
      ;password-store    ; password manager for nerds
       pdf               ; pdf enhancements
      ;prodigy           ; FIXME managing external services & code builders
      ;rgb               ; creating color strings
       terraform
       tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp
       ;wakatime


       :lang
       agda
       ;;assembly          ; assembly for fun or debugging
       (cc +lsp +rtags)    ; C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       common-lisp
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;erlang            ; an elegant language for a more civilized age
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;ess               ; emacs speaks statistics
       fsharp
       go                ; the hipster dialect
       (haskell +dante) ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       idris             ;
       ;(java +meghanada) ; the poster child for carpal tunnel syndrome
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       kotlin
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;ledger            ; an accounting system in Emacs
       lua               ; one-based indices? one-based indices
       (markdown +pandoc)          ; writing docs for people to ignore
       ;nim               ; python + lisp at the speed of c
       nix               ; I hereby declare "nix geht mehr!"
       ;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +habit
        +present         ; Emacs for presentations
        +protocol)
       perl              ; write code no one else can comprehend
       ;php               ; perl's insecure younger brother
       ;plantuml          ; diagrams for confusing people more
       purescript        ; javascript, but functional
       (python            ; beautiful is better than ugly
         +pyenv
         +lsp)
        qt
        racket
        rest              ; Emacs as a REST client
        ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
        (rust +lsp)             ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
        (scala +lsp)             ; java, but good
        (sh +fish)                ; she sells (ba|z)sh shells on the C xor
        ;solidity          ; do you need a blockchain? No.
        swift             ; who asked for emoji variables?
        ;tera
        web               ; the tubes

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       calendar
      ;(email +gmail)    ; emacs as an email client
      ;irc               ; how neckbeards socialize
      ;(rss +org)        ; emacs as an RSS reader
      ;twitter           ; twitter client https://twitter.com/vnought
      ;(write            ; emacs as a word processor (latex + org + markdown)
      ; +wordnut         ; wordnet (wn) search
      ; +langtool)       ; a proofreader (grammar/style check) for Emacs

       :collab
       ;;floobits          ; peer programming for a price
       ;impatient-mode    ; show off code over HTTP

       :config
       ;; The default module set reasonable defaults for Emacs. It also provides
       ;; a Spacemacs-inspired keybinding scheme, a custom yasnippet library,
       ;; and additional ex commands for evil-mode. Use it as a reference for
       ;; your own modules.
       (default +bindings +smartparens +snippets +evil-commands))


;; Fancy look
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Disable title bars
;; (setq default-frame-alist '((undecorated . t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#1E2029" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(custom-enabled-themes (quote (doom-dracula)))
 '(custom-safe-themes
   (quote
    ("06e4b3fdcbadc29ff95a7146dee846cd027cfefca871b2e9142b54ad5de4832f" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "427fa665823299f8258d8e27c80a1481edbb8f5463a6fb2665261e9076626710" "4ea0aa360264ff861fb0212abe4161b83ad1d8c8b74d8a04bcd1baf0ebdceeae" "8e04ea7bf8a736b0bfacd363f4810ffce774ff9ba24f356172ae2b83307aebb2" "614e5089876ea69b515c50b6d7fa0a37eb7ed50fda224623ec49e1c91a0af6a1" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "cb477d192ee6456dc2eb5ca5a0b7bd16bdb26514be8f8512b937291317c7b166" "cdb3e7a8864cede434b168c9a060bf853eeb5b3f9f758310d2a2e23be41a24ae" "2f0cbe053485bccbbbb582acdba7c7c9585ad808ee8ab32f0d727c3d39b42275" "8047ac280914cbe8dcdc489703c398f0941339cfca77dfc09f3641f1f040267c" "a16e816774b437acb78beb9916a60ea236cfcd05784227a7d829623f8468c5a2" "8c847a5675ece40017de93045a28ebd9ede7b843469c5dec78988717f943952a" "f8c30fa07ba7e8fe884f22b428dae6724955fa61ad84a658c3b0164ae391fb52" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "ab9456aaeab81ba46a815c00930345ada223e1e7c7ab839659b382b52437b9ea" "ef4edbfc3ec509612f3cf82476beddd2aeb3da7bdc3a35726337a0cc838a4ef4" "e3c87e869f94af65d358aa279945a3daf46f8185f1a5756ca1c90759024593dd" "e838d6375a73fda607820c65eb3ea1f9336be7bd9a5528c9161e10c4aa663b5b" "1a6d627434899f6d21e35b85fee62079db55ef04ecd9b70b82e5d475406d9c69" "4e132458143b6bab453e812f03208075189deca7ad5954a4abb27d5afce10a9a" "f5568ed375abea716d1bdfae0316d1d179f69972eaccd1f331b3e9863d7e174a" "e95ad48fd7cb77322e89fa7df2e66282ade015866b0c675b1d5b9e6ed88649b4" "b0fd04a1b4b614840073a82a53e88fe2abc3d731462d6fde4e541807825af342" "9c27124b3a653d43b3ffa088cd092c34f3f82296cf0d5d4f719c0c0817e1afa6" "868abc288f3afe212a70d24de2e156180e97c67ca2e86ba0f2bf9a18c9672f07" "155a5de9192c2f6d53efcc9c554892a0d87d87f99ad8cc14b330f4f4be204445" "256bd513a9875cd855077162cdfee8d75b0ad7e18fe8b8cbc10412561fbef892" default)))
 '(fci-rule-color "#6272a4")
 '(jdee-db-active-breakpoint-face-colors (cons "#1E2029" "#bd93f9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1E2029" "#50fa7b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1E2029" "#565761"))
 '(objed-cursor-color "#ff5555")
 '(safe-local-variable-values
   (quote
    ((eval setq flycheck-checker
           (quote python-pycheckers)))))
 '(vc-annotate-background "#282a36")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50fa7b")
    (cons 40 "#85fa80")
    (cons 60 "#bbf986")
    (cons 80 "#f1fa8c")
    (cons 100 "#f5e381")
    (cons 120 "#face76")
    (cons 140 "#ffb86c")
    (cons 160 "#ffa38a")
    (cons 180 "#ff8ea8")
    (cons 200 "#ff79c6")
    (cons 220 "#ff6da0")
    (cons 240 "#ff617a")
    (cons 260 "#ff5555")
    (cons 280 "#d45558")
    (cons 300 "#aa565a")
    (cons 320 "#80565d")
    (cons 340 "#6272a4")
    (cons 360 "#6272a4")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:inherit doom-modeline-bar))))
 '(lsp-ui-peek-highlight ((t (:inherit doom-modeline-urgent :box 1))))
 '(lsp-ui-peek-peek ((t (:inherit mode-line))))
 '(lsp-ui-peek-selection ((t (:inherit doom-modeline-bar :weight bold)))))
