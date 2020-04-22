# OBorg Documentation

"[BorgBackup](https://borgbackup.readthedocs.io/en/stable/)
(short: Borg) is a deduplicating backup program. Optionally, it
supports compression and authenticated encryption."

OBorg is a partial alternative implementation of Borg.  It is unlikely
it will ever be feature complete.  It has some of the following goals:

- Understanding/documenting the file formats used by Borg.  Because
  of the security implications of backups, my primary purpose behind
  OBorg is as an analysis of the use of cryptography within Borg.
- An alternate restore implementation.  Because ocaml is able to
  create a single exectuable of a program, if OBorg is able to restore
  from Borg backups, it can be useful for disaster recovery.  It is
  also just useful in general to have alternate implementations.
- Something significant to implement in Ocaml.  I don't get a lot of
  opportunities to write Ocaml code.  Ocaml is the most pleasant
  programming language I've used.

## Borg internals

Much of [Borg
Internals](https://borgbackup.readthedocs.io/en/stable/internals.html)
are documented, but there is a tendency to just use existing Python
libraries for many of these formats.  Fortunately, Message Pack is
supported by Ocaml.

Because borg repositories are encrypted, it is necessary to begin with
supporting the encryption of the repo.  The plan is to start with the
outermost layer and work inward.

### Keyfiles (read done)

The first files encountered in a Borg repo don't always live in the
repository itself.  I'm starting with supporting external keyfiles.
These files are
[described](https://borgbackup.readthedocs.io/en/stable/internals/data-structures.html#key-files)
fairly completely in the Borg documentation, and an implementation of
a reader for this was fairly straightforward.

After using the mirage-crypto library, I feel this is a good crypto
library to use as well for the rest of the OBorg project.

### HashIndex (TODO)

Before attempting to read from the repository itself, it may be useful
to implement the
[HashIndex](https://borgbackup.readthedocs.io/en/stable/internals/data-structures.html#hashindex)
file support.  These files are a simple hash table, using the SHA-256
hash function used within Borg as the key.  The Borg implementation is
in C, and although benchmarking may say otherwise, a plain
implementation in OCaml is probably adequate for our needs.

### The Object Store (TODO)

Reading the object store will be the first use of the keys.  This
format is also fairly well
[documented](https://borgbackup.readthedocs.io/en/stable/internals/data-structures.html#segments).
The outer segment layer is not encrypted, but the contents of the
objects in the segment files will be.

## Why OCaml

Because I like OCaml.  I recently undertook an implementation of
[rsure](https://github.com/d3zd3z/rsure) in OCaml
([osure](https://github.com/d3zd3z/osure)).  The full implementation,
including the complicated weave implementation, and all of the tree
comparison operations took only a few weeks of mostly weekends and
evenings.  Because a CPS transformation of some of the weave code was
fairly easy to implement in OCaml (compared to Rust), I was able to
turn a push parser into a pull parser.  As a consequence, reading from
weave files is quite a bit faster in the OCaml version than the rust
version.  The Rust implementation uses communications with a thread to
change the parser.
