# zt

zt is a simple but highly opinionated Emacs package for building a
Zettelkasten. To get started, continue reading from
[Installation](#installation) below.

**Table of contents**

* [Motivation](#motivation)
* [Installation](#installation)
* [Configuration](#configuration)
* [Tutorial](#tutorial)
* [Best practices and philosophy](#best-practices-and-philosophy)
* [Comparison to other software](#comparison-to-other-software)
* [License](#license)

## Motivation

zt is [yet another](#comparison-to-other-software) Emacs package for building a
Zettelkasten. Why another one? From a mechanistic viewpoint, zt is different
from other solutions on a few parameters:

1. zt does not (necessarily) store the title of a file in its filename; by
   default files are named like `20220921T182341.txt`. You may optionally add
   more stuff after the ID prefix if you want, but the ID prefix is required.
2. However, zt *can* use the titles of files when searching for notes. For plain
   text files, the title is the first line, for Markdown, it's the first heading
   (or `title:` property in the YAML frontmatter), and for Org-mode it's the
   `#+TITLE:` or first heading.
3. This "logical title" is used to interactively (through `completing-read`)
   find notes for navigation or linking by their titles. It's also possible to
   navigate via backlinks.
4. Links are simply the ID of a note. Delimiters are not required. By default,
   commands that insert links also insert the title of the linked file and
   optionally a link back to the originating context. In Org-mode, the special
   `zt:` link type may be used, e.g. `zt:20220921T182341`.

The result of this is that a note can be renamed without changing its filename
and without updating existing links to that file. You can also use special
characters in titles without any issues. (Note that [The
Archive](https://zettelkasten.de/the-archive/) and
[Zettlr](https://www.zettlr.com/) work in the same way, but The Archive is
proprietary macOS-only software, and Zettlr supports only Markdown.)

As for *why this actually matters*, well, it's mostly a matter of aesthetics. A
Zettelkasten is supposed to be a simple system for building knowledge out of
small notes in an organic way. By assigning special meaning to the filename, the
author must make an important decision before they've even written the note, or
else they are required to go through some special ceremony to change their
decision later. From a technical standpoint, this doesn't matter that much, but
it can subtly guide you towards taking notes in a more "top-down" manner, which
goes against the spirit of Zettelkasten (see [*Best practices and
philosophy*](#best-practices-and-philosophy) below).

In my opinion, a plain text-based approach where the individual notes are
represented as files on disk named after their IDs, and where links between
notes are created by simply writing the ID of the relevant note, is the best
replication of an analog Zettelkasten. Such a system can then be extended with
tools for finding notes by their title, automatically finding backlinks of
notes, automatically following note links, interoperation with other file
formats like Markdown and Org-mode, and other digital-only features. This is the
design philosophy that gave rise to zt.

## Installation

First, install the zt package itself. You can use a package manager like
[straight.el](https://github.com/radian-software/straight.el) to do this for
you, or you can do it manually with:

```emacs-lisp
;; Download files
(make-directory "~/.emacs.d/load" t)
(require 'url)
(url-copy-file "https://raw.githubusercontent.com/c2d7fa/zt/main/zt.el" "~/.emacs.d/load/zt.el" t)

;; Load package (add this to init.el)
(add-to-list 'load-path "~/.emacs.d/load")
(require 'zt)
```

Unfortunately, zt also requires a companion program called `ztf`. This is
necessary because Emacs' built-in file loading just isn't fast enough to support
backlinking and search, so `ztf` is used for title resolution and backlinking.

If you're running x86-64 Linux, a prebuilt executable will be downloaded from
GitHub the first time it's needed. Otherwise, you'll have to build it yourself
by running `nim c -d:release ./ztf.nim` and copying the resulting executable to
`~/.local/share/zt/ztf` or whatever `zt-zft-executable-path` is set to.

## Tutorial

**TODO: This section hasn't been written yet!**

Outline:

- Recommended keybinding setup.
- Reference to (currently unwritten) manual.
- Briefly describe file layout, title parsing.
- Describe workflow for inserting link to new file.

## Best practices and philosophy

**TODO: This section hasn't been written yet!**

Outline:

- Understand what a Zettelkasten is and isn't. Consider using a different
  system, which can still be integrated with zt by sharing file naming
  scheme. Org-mode is suited to journal, calendar, project/task management, etc.
- Keep filenames and titles separate; prefer plain text, only use Org when you
  actually need it (mention `zt-change-file-extension`).
- Don't rely on automatic backlinks (although zt does support them)
- Do use Folgezettel to avoid overhead of adding everything to indexes and allow
  for serendipitous discovery later. Describe Folgezettel in Zt and how they
  relate to Luhmanns numbering system. Add screenshot to demonstrate idea.
- Use links sparingly; suprisingly, fewer links are better!
- Use structure notes to enable creative connections between distant notes. You
  should specifically avoid making indexes of obviously related notes.

## Comparison to other software

- [The Archive](https://zettelkasten.de/the-archive/) is a proprietary
  application for macOS. It's designed for Zettelkasten and values simple
  design. If you use macOS and don't mind using proprietary software, this is
  what I recommend.
- [Zettlr](https://www.zettlr.com/) has a similar design philosophy to The
  Archive, but it's free and open-source software and available on multiple
  platforms. Zettlr, like zt, does not care about the filenames of notes and
  supports finding notes by title, even if the title is different from the
  filename. It supports only Markdown. For most people, this would be my second
  recommendation. If you're looking for something that's compatible with plain
  text or Org-mode, keep reading.
- **zt** is an Emacs package designed for Zettelkasten with a simple
  design. It's compatible with multiple file-formats and handles links to other
  files by their IDs, such that renaming a note does not require any updates to
  filenames or existing links. It lets you interactively find files by their
  "logical" title (in plain text, Markdown and Org), and makes including the
  title in the filename optional.
- [zk.el](https://github.com/localauthor/zk) is another Emacs package with a
  very similar design philosophy to zt. It's also inspired by The Archive, is
  also designed for plain text Zettelkasten and also values simplicity in its
  design. zk stores the titles of files in their filenames for the purpose of
  selecting files for linking with completion, so if you forget to update the
  filename, you won't be able to easily find the file you want to link
  to. However, links are stored by ID, so it's possible to rename files without
  needing to update existing links. If you basically like zt but don't want to
  use it for some reason, this is what I recommend.
- [Denote](https://protesilaos.com/emacs/denote) is another simple note-taking
  package for Emacs. It's not designed specifically for Zettelkasten but it can
  certainly be used for that purpose. Denote expects you to assign tags to your
  files. Like zk, and unlike zt, Denote requires you to keep filenames in sync
  with thier titles; like both zk and zt, you don't need to update existing
  links whenever you change the title of a note. If you're trying to build a
  Zettelkasten, I recommend using zk or zt over Denote. Note that zt is
  specifically designed to be interoperable with Denote by using the same ID
  format, so if you're using Org-mode, transitioning between these two is just a
  matter of `s/zt:/denote:/g`.
- [Logseq](https://logseq.com/), [Obsidian](https://obsidian.md/), [Roam
  Research](https://roamresearch.com/) and others *can* be used to implement a
  Zettelkasten system, but they are not really designed for it. You must
  manually assign unique titles to all files. Obsidian has a Zettelkasten
  plugin, but it still requires you to link to files with verbose links that
  include the entire title. Whenever you change the title of a note, you must
  also update all links to it (which can be done automatically, of course). If
  you don't mind this -- or especially if you want to build something more like
  a personal wiki than a Zettelkasten in a strict sense -- these are all good
  solutions.
- [Org-mode](https://orgmode.org/) can be used on its own to build a
  Zettelkasten by simply linking between files by their IDs. A lot of people
  characterize Org-mode as "plain text", but if you rely on its ID-based linking
  mechanisms, you're essentially locked in to using Org-mode forever. You can
  use Org-mode with zt, zk or Denote and get the best of both worlds.
- [Org-roam](https://github.com/org-roam/org-roam) is essentially a port of
  Obsidian to Emacs, designed for use with Org-mode. It has more overhead than
  plain Org-mode (notably, requiring a cache for backlinking), but provides a
  more familiar experience for Obsidian users. It has quite a few features. For
  my purposes (building a Zettelkasten), I found it somewhat clumsy, but it
  may be well-suited if you want something more like a personal wiki.
- [emacs-zettelkasten](https://sr.ht/~ymherklotz/emacs-zettelkasten/) seems to
  have similar goals both zt and zk. I haven't tried it.
- [Zetteldeft](https://github.com/EFLS/zetteldeft) is the package that zk is
  based on. I haven't tried it either.
- [Hyperbole](https://www.gnu.org/software/hyperbole/) is infamous for being
  rather hard to grasp, but it seems very cool from a distance. It does a lot
  more than zt, but I can't really give a useful comparison beyond that.

## License

zt is licensed under the terms of the GNU AGPLv3 or any later version as
described in `LICENSE.md`.
