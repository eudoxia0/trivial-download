trivial-download allows you to download files from the Internet from Common
Lisp. It even provides a progress bar.

# Usage

```lisp
cl-user> (download "http://www.google.com/robots.txt" "/tmp/robots.txt")
Downloading "http://www.google.com/robots.txt" (Unknown size)
t
cl-user> (download "https://github.com/favicon.ico" "/tmp/favicon.ico")
Downloading "https://github.com/favicon.ico" (6.518 kB)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%t
```

# License

Copyright (c) 2014 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.

