# Lisp Web Writer
html, css, javascript file writing utility for common lisp
## How to Install
* best used with [soc](https://github.com/wtleeiv/soc)
* clone this repo and soc repo into a location within ASDF source registry (eg. `~/.roswell/local-projects/`)
  * if you have no idea what the ASDF source registry is, check [this](https://common-lisp.net/project/asdf/asdf/Configuring-ASDF-to-find-your-systems.html) out 
* now that both projects are found by quicklisp you are ready to start writing web apps in lisp!!
## How to Use
lww recursively searches within your projects root directory for files named like: filename_{html,css,js}.lisp and writes out the associated web file
ex. index_html.lisp -> index.html
more specifically, it sets the stream soc:*soc* to the matching web filename, then loads the lisp file
its up to you to write code that makes use of this filestream
* soc ex. `(soc:soc <body of file>)`
* parenscript ex. `(ps:ps-to-stream soc:*soc* <body of file>)`
* `(lww:write-app "/path/to/project")` write all web files whose lisp files have changed (more recent timestamps)
* `(lww:write-app "/path/to/project" 't)` force lww to overwrite all matching lisp -> web file pairs
