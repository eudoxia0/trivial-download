# trivial-download

[![Build Status](https://travis-ci.org/eudoxia0/trivial-download.svg?branch=master)](https://travis-ci.org/eudoxia0/trivial-download)
[![Coverage Status](https://coveralls.io/repos/eudoxia0/trivial-download/badge.svg?branch=master)](https://coveralls.io/r/eudoxia0/trivial-download?branch=master)
[![Quicklisp](http://quickdocs.org/badge/trivial-download.svg)](http://quickdocs.org/trivial-download/)

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

You probably want the `download` function, which downloads files from the
network to the local disk. If you want more control over that -- like, say,
writing the downloaded bytes to a database instead -- you want the
`with-download` macro.

`trivial-download` downloads everything in chunks that are `*chunk-size*` bytes
long. `*chunk-size*`, by default, is 256.

* [Function] `download` *(url output)*

Downloads the content of `url` and writes it to `output`. The file is written as
it is downloaded, chunk-by-chunk, not downloaded into memory and written at
once.

* [Macro] `with-download` *(url (file-size total-bytes-read array stream) &rest body)*

Downloads the contents of `url`, executing `body` in every chunk.

The extra arguments are:

- `stream`: A `flexi-io-stream` bivalent stream.
- `file-size`: The size, in bytes, of the file to download.
- `bytes-read`: The number of bytes read.
- `array`: As every chunk is downloaded, its contents are written to `array`.

Example:

```lisp
(with-download "https://github.com/favicon.ico"
  ;; Do something
  )
```

* [Macro] `with-download-progress` *(url (file-size total-bytes-read array stream) &rest body)*

The same as `with-download`, only this prints progress information while
downloading.

# License

Copyright (c) 2014-2015 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.
