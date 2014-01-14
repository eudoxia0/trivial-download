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

# Documentation

* [Macro] `with-download` *(url &rest body)*

Executes `body` in a loop. Inside `body`, the variables `stream` and `it` are
defined to the file stream and the current byte, respectively.

Example:

```lisp
(with-download "https://github.com/favicon.ico"
  (format t "I'm a byte! ~A" it))
```

* [Macro] `with-download-progress` *(url &rest body)*

The same as `with-download`, only this prints progress information while
downloading.

* [Function] `download` *(url output)*

An specialization of `with-download-progress` for the most common case: Saving
the file in `url` to the pathname in `output`.

# License

Copyright (c) 2014 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.

