# Source Code of http://daniel.kxpq.de

## About

This is the source code of http://daniel.kxpq.de, written in
[Weblocks](http://github.com/skypher/weblocks).

## Running

You need

- A Common Lisp (tested with SBCL and Clozure CL (≥ 1.7))
- [Quicklisp](http://www.quicklisp.org)

Then after cloning this repository `/to/some/path`, you need to make sure that
`/to/some/path/website.asd` can be found by Quicklisp (or ASDF, for that matter).  I have
ASDF configured to search for system files in `~/.config/common-lisp/systems`, i.e., my
`~/.config/common-lisp/source-registry.conf` looks like this

```
(:source-registry
  (:tree (:home ".config/common-lisp/systems/"))
  :inherit-configuration)
```

Then putting a symlink to `website.asd` to `~/.config/common-lisp/systems` should be
enough.

Now start your common lisp, and do

```
CL-USER> (ql:quickload :website)
... some output ...
CL-USER> (website:start-website)
```

Then point your browser to port 52341 on your localhost.

## License

ⓒ Daniel Borchmann 2015

Licensed under the MIT License.
