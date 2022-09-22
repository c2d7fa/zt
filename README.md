# zt

zt is a simple but highly opinionated Emacs package for building a
[Zettelkasten](https://zettelkasten.de/posts/overview/). To get started,
continue reading from [Installation](#installation) below.

#### Table of contents

* [Motivation](#motivation)
* [Installation](#installation)
  * [Non-Linux platforms](#non-linux-platforms-installation-of-ztf-companion)
* [Tutorial](#tutorial)
  * [Configure keybindings](#configure-keybindings)
  * [Create your first note](#create-your-first-note)
  * [Branch off into a follower-note](#branch-off-into-a-follower-note)
  * [Add a structure note](#add-a-structure-note)
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
[straight.el](https://github.com/radian-software/straight.el) (as [shown
below](#configure-keybindings)) to do this for you, or you can do it manually
with:

```emacs-lisp
;; Download files (run once, and again when updating zt)
(make-directory "~/.emacs.d/load" t)
(require 'url)
(url-copy-file "https://raw.githubusercontent.com/c2d7fa/zt/main/zt.el" "~/.emacs.d/load/zt.el" t)

;; Load package (add this to init.el)
(add-to-list 'load-path "~/.emacs.d/load")
(require 'zt)
```

If you're using x86-64 Linux, you can skip ahead to the [Tutorial](#tutorial)
now; otherwise, read the next section.

#### Non-Linux platforms: Installation of `ztf` companion

Unfortunately, zt also requires a companion program called `ztf`. This is
necessary because Emacs' built-in file loading just isn't fast enough to support
backlinking and search, so `ztf` is used for title resolution and backlinking.

If you're running x86-64 Linux, zt will ask to download a prebuilt executable
from the internet the first time it's needed. Its checksum is verified against a
hardcoded value, but obviously you must trust me to not be actively distributing
malware.

Other platforms are not explicitly supported, but you should be able to build it
yourself by running `nim c -d:release ./ztf.nim` and copying the resulting
executable to `~/.local/share/zt/ztf` or whatever `zt-zft-executable-path` is
set to.

Otherwise, consider just porting [ztf.nim](./ztf.nim) yourself. It's a very
short program. You can also replace this program to change how title extraction
works, for example to support a custom file format.

## Tutorial

In this section, we'll see how to configure zt, begin a new Zettelkasten, and
use follower and structure notes to start building up a library.

#### Configure keybindings

By default, zt exports a keymap `zt-minor-mode-prefix-map`, which is bound
(inside `zt-minor-mode-map`) to the prefix `C-c #`. It's *strongly* recommended
that you change this to something more convenient, and also that you remap some
of the more common commands. For example, this is my configuration with
[use-package](https://github.com/jwiegley/use-package) and
[straight.el](https://github.com/radian-software/straight.el):

```emacs-lisp
(use-package zt
  :straight (:host github :repo "c2d7fa/zt")
  :bind-keymap ("C-z" . zt-minor-mode-prefix-map)
  :bind (:map zt-minor-mode-map
         ("C-a" . zt-insert-link)
         ("M-a" . zt-insert-linking-file)
         ("C-f" . zt-find-file)
         ("M-f" . zt-find-linking-file)
         ("C-o" . zt-insert-new-id)
         ("M-o" . zt-open-follower-at-point)))
```

Note that this remaps some built-in keys. In the following, the default mappings
(prefixed with `C-c #`) are given, but you really should change them!  You can
use `describe-keymap` with `zt-minor-mode-map` to see the default keybindings.

#### Create your first note

Usually, you'll be creating notes from other notes. But to create your first
note, call `zt-create-in-directory`, and enter a directory where you would like
to store all your notes. This will open a new plain text file.

Try saving it, and notice that it has a filename that looks like
`20220922T200758.txt`. In zt, all files are prefixed with an ID; this is how zt
keeps track of them, and it's necessary for using zt.

By default, zt creates plain text files. If you want to use, say, Markdown
instead, call `zt-change-file-extension` (`C-c # .`) and enter `md`. You can
also change the default file format by customizing `zt-default-file-extension`,
but this is not recommended! Keep most of your notes in plain text, and mix in
other formats when you need the more advanced features.

zt treats the first line of the file as its title. Try changing this, saving the
buffer, and then calling `zt-find-file` (`C-c # f`). Notice how in the
completion buffer, zt extracts the title from the file automatically, even
though the filename is just an ID. Using a completion framework such as
[Ivy](https://github.com/abo-abo/swiper) is more or less a requirement for zt!

#### Branch off into a follower note

We could continue creating independent notes using this same method, but a
Zettelkasten is all about linking your notes. I recommend using an approach,
where you "chain" notes together to build up a kind of tree structure. When you
get a new idea, you insert a link to a so-called follower note (*Folgezettel*),
and then you open the new note and continue there.

At the bottom of the note you created in the last step, try calling
`zt-insert-new-id` (`C-c # t`). Then, after the ID, type a title for the new
note that you want to create. At this point, your buffer should look something
like this example (with `|` representing the point (cursor)):

```
File names and titles in zt

In zt, files are named after their IDs. Notes can have titles, which are
extracted from the file, and which are used when finding files with
`zt-find-file' or linking to them with `zt-insert-link`.

20220922T222825 Inserting links and backlinks in zt|
```

Now, with the point at the end of the line, call `zt-open-follower-at-point`
(`C-c # O`, or equivalently `C-u zt-open-at-point` (`C-u C-c # o`)). This will
create a new note with both the title you wrote as well as a link back to the
previous note inserted automatically, like this:

```
Inserting links and backlinks in zt

20220922T222648 File names and titles in zt|
```

From here, you can continue writing, and then repeat the process to create more
follower notes. You may choose to insert them at the end of the current
sequence, or to go back to a previous note and start a new branch. You can
navigate back and forth in the tree by clicking the "backward" links at the top
of each note and the "forward" links at the bottom. You can jump directly to an
existing note with `zt-find-file` (`C-c # f`)

Once you've built up a little tree of notes like this, move on to the next
section.

#### Add a structure note

**TODO: This section is missing an introduction!**

To insert a link to a note you already wrote, use `zt-insert-link` (`C-c # l`).
By default, this will insert both the ID and title of the selected note, which
is useful when building lists of notes.

Of course, you should not use links only when writing structure notes. When you
reference an idea from another note, you should also insert a link to that note
so that you can find it again in the future. When doing this, you may want to
insert only the ID of the chosen file by calling `zt-insert-link` with a prefix
argument (`C-u C-c # l`). By adding links, it also becomes possible to find
releted notes via `zt-find-linking-file` (`C-c # F`), which shows only those
files that link to the current note.

Below is a longer, more realistic example of what a structure note may actually
look like in practice. I wrote this note to summarize and index a previous
sequence of notes, and then added it to an overarching topic note, which I
inserted a backlink to with `zt-insert-linking-file` (`C-c # L`).

```
Folgezettel vs. index-oriented approaches to Zettelkasten

20220918T031544 Zettelkasten

By forcing yourself to add every note to an index, you risk creating bloated
indexes 20220913T214836, which reduces trust in the system 20220428T185922. The
Folgezettel-oriented approach more effectively induces the "conversation
partner" quality of the Zettelkasten 20220922T113137, and also removes the need
for a dedicated inbox.

Folgezettel in Luhmann's system
    20220124T000002 Luhmann wrote many linear chains of thought
    20220914T025344 Emulating Antinet numbering system in digital Zettelkasten
        20220914T184826 Using two-way "hard" links for branching
    20220914T024106 Disadvantages of digital Zettelkästen

Importance of simplicity
    20220920T233524 The different purposes of Zettelkasten and GTD
    20220920T233259 Zettelkasten is the wrong tool for managing collections of reference material
    20220920T003710 Collector's Fallacy

When and how to use indexes
    20220921T004228 Use structure notes to create "wormholes" between distant clusters
    20220918T163155 Meta-index for integrated Zettelkasten/GTD/journal system in Emacs
    20220921T010536 Don't add multiple notes about the same idea to one index
```

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
  needing to update existing links. A major advantage of zk compared to zt is
  that it doesn't have any dependencies on external programs.
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
