rm -rf racket/
curl -L https://download.racket-lang.org/releases/7.2/installers/racket-minimal-7.2-x86_64-linux.sh -o install-racket.sh
sh install-racket.sh --in-place --create-links . --dest racket/
racket/bin/raco pkg install --batch --auto web-server-lib
