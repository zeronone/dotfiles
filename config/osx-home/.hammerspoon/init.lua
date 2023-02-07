hs.hotkey.bind({"cmd","alt","ctrl"}, "i",
  function ()
    task = hs.task.new("/Users/arezai/.emacs.d/bin/doom", nil, { "everywhere" })
    task:setEnvironment({PATH="/Users/arezai/Library/Application Support/Coursier/bin:/Applications/Emacs.app/Contents/MacOS/bin:/Applications/Coq_Platform_2021.02.1.app/Contents/Resources/bin:/Users/arezai/.krew/bin:/Users/arezai/sdks/flutter/bin:/Users/arezai/bin:/Users/arezai/.gem/bin:/Users/arezai/scripts:/Users/arezai/.emacs.d/bin:/Users/arezai/.cargo/bin:/Users/arezai/.cabal/bin:/Users/arezai/.ghcup/bin:/Users/arezai/.local/bin:/usr/local/sbin:/Users/arezai/Library/Application Support/Coursier/bin:/Users/arezai/.pyenv/bin:/Users/arezai/.nvm/versions/node/v12.22.12/bin:/Users/arezai/env/bin:/Users/arezai/indeed/hobo/bin:/Users/arezai/.tfenv/bin:/Users/arezai/indeed/system-setup/bin:/Users/arezai/.indeed-dev-tools/bin:/Users/arezai/indeed/javadev/bin/po:/Users/arezai/indeed/javadev/bin:/usr/local/opt/coreutils/libexec/gnubin:/usr/local/opt/ed/libexec/gnubin:/usr/local/opt/findutils/libexec/gnubin:/usr/local/opt/gnu-indent/libexec/gnubin:/usr/local/opt/gnu-sed/libexec/gnubin:/usr/local/opt/gnu-tar/libexec/gnubin:/usr/local/opt/gettext@0.20.2/bin:/usr/local/opt/gnu-which/libexec/gnubin:/usr/local/opt/gnutls/bin:/usr/local/opt/grep/libexec/gnubin:/usr/local/opt/openssl@1.1/bin:/usr/local/opt/python@3.6.8/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/munki:/opt/X11/bin:/Library/Apple/usr/bin:/Applications/Wireshark.app/Contents/MacOS:/Users/arezai/go/bin:/Users/arezai/myfiles/dotfiles/bin:/Users/arezai/bin/kotlin-language-server/bin:/usr/local/go/bin:/usr/local/opt/mongodb-community@3.6/bin:/Users/arezai/n/bin"})
    task:start()
  end
)


hs.hotkey.bind({"cmd","alt","ctrl"}, "E",
  function ()
    task = hs.task.new("/Applications/Emacs.app/Contents/MacOS/bin/emacs", nil, nil)
    task:setEnvironment({PATH="/Users/arezai/Library/Application Support/Coursier/bin:/Applications/Emacs.app/Contents/MacOS/bin:/Applications/Coq_Platform_2021.02.1.app/Contents/Resources/bin:/Users/arezai/.krew/bin:/Users/arezai/sdks/flutter/bin:/Users/arezai/bin:/Users/arezai/.gem/bin:/Users/arezai/scripts:/Users/arezai/.emacs.d/bin:/Users/arezai/.cargo/bin:/Users/arezai/.cabal/bin:/Users/arezai/.ghcup/bin:/Users/arezai/.local/bin:/usr/local/sbin:/Users/arezai/Library/Application Support/Coursier/bin:/Users/arezai/.pyenv/bin:/Users/arezai/.nvm/versions/node/v12.22.12/bin:/Users/arezai/env/bin:/Users/arezai/indeed/hobo/bin:/Users/arezai/.tfenv/bin:/Users/arezai/indeed/system-setup/bin:/Users/arezai/.indeed-dev-tools/bin:/Users/arezai/indeed/javadev/bin/po:/Users/arezai/indeed/javadev/bin:/usr/local/opt/coreutils/libexec/gnubin:/usr/local/opt/ed/libexec/gnubin:/usr/local/opt/findutils/libexec/gnubin:/usr/local/opt/gnu-indent/libexec/gnubin:/usr/local/opt/gnu-sed/libexec/gnubin:/usr/local/opt/gnu-tar/libexec/gnubin:/usr/local/opt/gettext@0.20.2/bin:/usr/local/opt/gnu-which/libexec/gnubin:/usr/local/opt/gnutls/bin:/usr/local/opt/grep/libexec/gnubin:/usr/local/opt/openssl@1.1/bin:/usr/local/opt/python@3.6.8/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/munki:/opt/X11/bin:/Library/Apple/usr/bin:/Applications/Wireshark.app/Contents/MacOS:/Users/arezai/go/bin:/Users/arezai/myfiles/dotfiles/bin:/Users/arezai/bin/kotlin-language-server/bin:/usr/local/go/bin:/usr/local/opt/mongodb-community@3.6/bin:/Users/arezai/n/bin"})
    task:start()
  end
)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "W", function()
  hs.alert.show("Hello World!")
end)
