# LODDS (Local Open Distributed Data Sharing)

LODDS is a decentral (local) network file-sharing protocol. This
Repository contains the Protocol and its Implementations.
  - [Protocol](first-spec.txt)
  - [Java Implementation](java-code)
  - [Common Lisp Implementation](cl-code)

## TODO

- Advertised name Specification (whitespace allowed?)
- Regex for relative Filename (/, \, :, *, ?, ", <, >, |, %)

## Commit Message Format

For our commit messages, we use a precise format. This should include
a headline before information about specific changes.  You can read
more about it
[here](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).
We also tag our commit message headlines to distinguish between
certain commits. For example every change to the common lisp code is
tagged with a `lisp` tag, java code is tagged with `java` etc.

### Commit Tags and Format

#### Format

Headlines begin with a tag. Tags end with a `:`, multiple tags can be
combined (concatenated with space in between). For example:

```
lisp: doc: updated function documentation

...
```

#### List of Tags

 tag   | description
-------|------------
java   | commit concerning java code
lisp   | commit concerning lisp code
doc    | documentation change (no code changed)
spec   | specification change
bugfix | a bug was fixed!
style  | code style changes (no code changed)
layout | code layout changes (no code change)
readme | README was updated
