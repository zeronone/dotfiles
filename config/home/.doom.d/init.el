;;; init.el -*- lexical-binding: t; -*-

;; I've swapped these keys on my keyboard
(setq x-super-keysym 'meta
      x-alt-keysym   'alt)

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
       ;(pretty-code       ; replace bits of code with pretty symbols
       ; +iosevka
       ; )
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242730" "#ff665c" "#7bc275" "#FCCE7B" "#51afef" "#C57BDB" "#5cEfFF" "#bbc2cf"])
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("001c2ff8afde9c3e707a2eb3e810a0a36fb2b466e96377ac95968e7f8930a7c5" "7f74a3b9a1f5e3d31358b48b8f8a1154aab2534fae82c9e918fb389fca776788" "423435c7b0e6c0942f16519fa9e17793da940184a50201a4d932eafe4c94c92d" "0fe9f7a04e7a00ad99ecacc875c8ccb4153204e29d3e57e9669691e6ed8340ce" "fe76f3d5094967034192f6a505085db8db6deb0e135749d9a54dc488d6d3ee2f" "d6f04b6c269500d8a38f3fabadc1caa3c8fdf46e7e63ee15605af75a09d5441e" "5e0b63e0373472b2e1cf1ebcc27058a683166ab544ef701a6e7f2a9f33a23726" "7d56fb712ad356e2dacb43af7ec255c761a590e1182fe0537e1ec824b7897357" "428754d8f3ed6449c1078ed5b4335f4949dc2ad54ed9de43c56ea9b803375c23" "cdb3e7a8864cede434b168c9a060bf853eeb5b3f9f758310d2a2e23be41a24ae" "2878517f049b28342d7a360fd3f4b227086c4be8f8409f32e0f234d129cee925" "0d087b2853473609d9efd2e9fbeac088e89f36718c4a4c89c568dd1b628eae41" "e7666261f46e2f4f42fd1f9aa1875bdb81d17cc7a121533cad3e0d724f12faf2" "2d392972cbe692ee4ac61dc79907af65051450caf690a8c4d36eb40c1857ba7d" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "728eda145ad16686d4bbb8e50d540563573592013b10c3e2defc493f390f7d83" "c95043bcca81b664f7b394e88f888065aa80ba48b4f3a02ede30590399035a49" "332e009a832c4d18d92b3a9440671873187ca5b73c2a42fbd4fc67ecf0379b8c" "70ed3a0f434c63206a23012d9cdfbe6c6d4bb4685ad64154f37f3c15c10f3b90" "c8f959fb1ea32ddfc0f50db85fea2e7d86b72bb4d106803018be1c3566fd6c72" "2a3ffb7775b2fe3643b179f2046493891b0d1153e57ec74bbe69580b951699ca" "f951343d4bbe5a90dba0f058de8317ca58a6822faa65d8463b0e751a07ec887c" "071f5702a5445970105be9456a48423a87b8b9cfa4b1f76d15699b29123fb7d8" "b462d00de785490a0b6861807a360f5c1e05b48a159a99786145de7e3cce3afe" "f2b83b9388b1a57f6286153130ee704243870d40ae9ec931d0a1798a5a916e76" "1728dfd9560bff76a7dc6c3f61e9f4d3e6ef9d017a83a841c117bd9bebe18613" "f589e634c9ff738341823a5a58fc200341b440611aaa8e0189df85b44533692b" "0713580a6845e8075113a70275b3421333cfe7079e48228c52300606fa5ce73b" "f30aded97e67a487d30f38a1ac48eddb49fdb06ac01ebeaff39439997cbdd869" "a2286409934b11f2f3b7d89b1eaebb965fd63bc1e0be1c159c02e396afb893c8" default)))
 '(fci-rule-color "#62686E")
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854"))
 '(objed-cursor-color "#ff665c")
 '(vc-annotate-background "#242730")
 '(vc-annotate-color-map
   (list
    (cons 20 "#7bc275")
    (cons 40 "#a6c677")
    (cons 60 "#d1ca79")
    (cons 80 "#FCCE7B")
    (cons 100 "#f4b96e")
    (cons 120 "#eda461")
    (cons 140 "#e69055")
    (cons 160 "#db8981")
    (cons 180 "#d082ae")
    (cons 200 "#C57BDB")
    (cons 220 "#d874b0")
    (cons 240 "#eb6d86")
    (cons 260 "#ff665c")
    (cons 280 "#d15e59")
    (cons 300 "#a35758")
    (cons 320 "#754f56")
    (cons 340 "#62686E")
    (cons 360 "#62686E")))
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
