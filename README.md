# zt

zt is a simple but highly opinionated Emacs package for building a
[Zettelkasten](https://zettelkasten.de/posts/overview/). To get started, skip
ahead to [Installation](#installation) below.

#### Table of contents

* [Motivation](#motivation)
* [Installation](#installation)
  * [Non-Linux platforms](#non-linux-platforms-installation-of-ztf-companion)
* [Tutorial](#tutorial)
  * [Configure keybindings](#configure-keybindings)
  * [Create your first note](#create-your-first-note)
  * [Branch off into a follower-note](#branch-off-into-a-follower-note)
  * [Add a structure note](#add-a-structure-note)
* [Best practices](#best-practices)
  * [Zettelkasten isn't designed for keeping track of things](#zettelkasten-isnt-for-organization)
  * [My suggestion for an organizational system that uses Zettelkasten](#three-tools)
* [Comparison to other software](#comparison-to-other-software)
* [License](#license)

#### Screenshot

<img src="./screenshot.png?raw=true" width="500px"/>

## Motivation

zt is [yet another](#comparison-to-other-software) Emacs package for building a
Zettelkasten. Why another one? From a mechanistic viewpoint, zt is different
from other solutions on a few parameters:

1. zt does not (necessarily) store the title of a file in its filename; by
   default files are named like `20220921T182341.txt`. You may optionally add
   more stuff after the ID prefix if you want, but the ID prefix is required.
2. However, zt *does* support "logical titles" when selecting notes
   interactively (for example when inserting links). For plain text files, the
   title is the first line, for Markdown, it's the first heading (or `title:`
   property in the YAML frontmatter), and for Org-mode it's the `#+TITLE:` or
   first heading.
3. Links are just the ID of a note. Delimiters are not required. By default,
   commands that insert links also insert the title of the linked file and
   optionally a link back to the originating context. In Org-mode, the special
   `zt:` link type may be used, e.g. `zt:20220921T182341`.

The result of this is that a note can be renamed without changing its filename
and without updating existing links to that file. You can also use special
characters in titles without any issues.

As for *why this actually matters*, well, it's mostly a matter of aesthetics. A
Zettelkasten is supposed to be a simple system for building knowledge out of
small notes in an organic way. The [simplest possible
Zettelkasten](https://zettelkasten.de/posts/introduction-antinet-zettelkasten/)
is implemented in terms of slips of paper which are assigned identifiers for
linking between them.

In a digital system, the most straight-forward emulation of such a system is
with files whose filenames are unique IDs, and links that are simply the IDs
themselves. Many note-taking applications assume a model that is fundamentally
incompatible with such an approach, usually because they conflate titles with
filenames (as [Denote](https://github.com/protesilaos/denote) and
[zk.el](https://github.com/localauthor/zk) do), or even with IDs, as
[Obsidian](https://obsidian.md/) does! zt, in contrast, takes these simple,
plain-text foundations as its base, and then adds extra digital-only
functionality on top to make it an actually useful system.

If you have a directory of files named like `20220922T200758.txt` which refer to
each other by their IDs, then your system is already compatible with zt. From
there, zt adds support for following links by clicking on them, easily linking
to existing items by their title, finding backlinks to notes, and more, but at
it's core, it's simply a collection of uniquely named text files. Some software,
notably [The Archive](https://zettelkasten.de/the-archive/) and
[Zettlr](https://www.zettlr.com/) do share this design philosophy.

## Installation

First, install the zt package itself. If you're using
[straight.el](https://github.com/radian-software/straight.el), add this to your
`init.el`:

```emacs-lisp
(straight-use-package 'zt '(:host github :repo "c2d7fa/zt"))
```

Otherwise, install zt manually with:

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

Unfortunately, zt also requires a companion program called `ztf`. Since we
insist on not storing meaningful information in file names, we are forced to
actually examine the file contents to find the title of each note. Emacs' native
file operations aren't fast enough to be usable with more than a few hundred
notes. Therefore, we call out to this external program when listing files by
titles and also when finding backlinks.

If you're running x86-64 Linux, zt will ask to download a prebuilt executable
from the internet the first time it's needed. Otherwise, you must manually
download another executable and install it at `~/.local/share/zt/ztf` or
whatever `zt-ztf-executable-path` is set to; such executables can be downloaded
from the following URLs:

* [ztf-1-x86_64-macos](https://johv.dk/public/ztf-1-x86_64-macos)
* [ztf-1-x86_64-windows.exe](https://johv.dk/public/ztf-1-x86_64-windows.exe)
* [ztf-1-aarch64-linux](https://johv.dk/public/ztf-1-aarch64-linux)
* [ztf-1-aarch64-macos](https://johv.dk/public/ztf-1-aarch64-macos)
* [ztf-1-aarch64-windows.exe](https://johv.dk/public/ztf-1-aarch64-windows.exe)

Other platforms are not explicitly supported, but you should be able to build it
yourself; see [build-ztf.sh](./build-ztf.sh) for more information.

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
other formats only when you actually need the more advanced features.

zt treats the first line of the file as its title. Try writing something, saving
the buffer, and then calling `zt-find-file` (`C-c # f`) to see how the title is
extracted from the file automatically, even though the filename is just an
ID. In order to use this functionality, a completion framework such as
[Ivy](https://github.com/abo-abo/swiper) is more or less a requirement!

#### Branch off into a follower note

We could continue creating independent notes using this same method, but a
Zettelkasten is all about linking your notes. I recommend using an approach
where you "chain" notes together to build up a kind of tree structure. When you
get a new idea, insert a link to a so-called follower note (*Folgezettel*), and
open the new note and continue from there.

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
(`C-c # O`), or, equivalently, call `zt-open-at-point` with a prefix argument
(`C-u C-c # o`). This will create a new note with both the follower note's title
as well as a link back to the previous note inserted automatically, like this:

```
Inserting links and backlinks in zt

20220922T222648 File names and titles in zt|
```

From here, you can continue writing, and then repeat the process to create more
follower notes. You may choose to insert them at the end of the current
sequence, or you can go back to a previous note and start a new branch from
there. Browsing your notes consists of selecting an arbitrary starting point and
then using the "backward" links at the top of each note and the "forward" links
at the bottom to navigate the tree. You can jump directly to an existing note
with `zt-find-file` (`C-c # f`)

Once you've built up a little tree of notes like this, move on to the next
section.

#### Add a structure note

The sequences that we built up in the last step work well as a staging area for
new ideas. The advantage of such an approach is that you can just start writing
somewhere, and as long as you make sure to add links in both directions, there
is no risk of notes getting "lost". This reduces friction in the system; for
example, there is no need for any kind of inbox with this approach.

But as your Zettelkasten grows, you'll eventually want to introduce a second
layer: [structure
notes](https://zettelkasten.de/posts/three-layers-structure-zettelkasten/). A
structure note is essentially like a table of contents – it's a view of the
entire Zettelkasten if it were designed to answer a very specific question.

To create these structure notes in zt, use `zt-insert-link` (`C-c # l`), which
inserts a link to an existing note.  By default, this will insert both the ID
and title of the selected note, which is useful when building lists of
notes. Try using this functionality to create an interesting overview of some of
the notes you just wrote. Don't worry about adding every relevant note to the
index – focus on making it useful and interesting by not adding two notes that
are too similar.

Below is a realistic example of what a useful structure note may actually look
like in practice. I wrote this note to summarize and index a previous sequence
of notes, and then added it to an overarching topic note, to which I inserted a
backlink with `zt-insert-linking-file` (`C-c # L`).

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

Of course, links aren't only used for writing structure notes. Whenever you
reference an idea from another note, insert a link to that note so you can find
it again in the future. When doing this, you may want to insert only the ID of
the chosen file by calling `zt-insert-link` with a prefix argument (`C-u C-c # l`).
By adding links, it also becomes possible to find releted notes via
`zt-find-linking-file` (`C-c # F`), which shows only those files that link to
the current note.

That's it for the tutorial. You now know everything you need to use zt. If you
want some extra tips on how to use the Zettelkasten methodology most effectively
(in my opinion), check out the section below.

## Best practices

The following are some of my recommendations for how to get the most out of your
Zettelkasten, based on my experience. I don't claim that this is *the*
Zettelkasten methodology; these are just some tips that have worked for me.

<a id="zettelkasten-isnt-for-organization"></a>
#### Zettelkasten isn't designed for keeping track of things

Zettelkasten is designed for research and writing, not as an organizational
system. In fact, I like to think of Zettelkasten as a kind of "tool for
forgetting". You can use the Zettelkasten to think through complex problems in a
structured way, completely forget about everything you just wrote down, and then
wait for the Zettelkasten to bring those ideas up to you again, as though it
were a kind of conversation partner. Later, the little slips can be stictched
together into a publishable piece.

However, this process isn't magical. It isn't entirely reliable, and it
certainly isn't timely. So if you need a tool for organizing your life, look
elsewhere – Zettelkästen are completely unsuitable for storing things such as
reminders, calendars, project reference material, todo-lists, or other
information that's actually important for you to remember!

<a id="three-tools"></a>
#### My suggestion for an organizational system that integrates Zettelkasten

If you *do* need an organizational system, here is my suggestion for an approach
that uses zt to integrate your Zettelkasten with two other systems. I think
about it as three distinct "tools": one for thinking and writing (Zettelkasten),
one for remembering and keeping track of things, and one for planning out my day
(daily log).

*The Zettelkasten:* When there's something that I want to rediscover later, but
forget about in the meantime, I add it to the Zettelkasten. Luhmann's original
formulation is basically perfect for this purpose, and my digital Zettelkasten
implementation is designed to emulate it as closely as practically possible.

*The big list:* If Zettelkasten is a [tool for
forgetting](#zettelkasten-isnt-for-organization), then this is my "tool for
remembering". I just have a big list of items, separated into three sections
called *daily*, *weekly* and *monthly*. Each day, week or month respectively, I
look through each list, copy relevant items to my daily log (see below) and
remove items that are no longer relevant. This is how I handle stuff like task
lists and calendar events. Items may link to other items, in which case I follow
the links of the items I picked out and repeat the process recursively – that's
how I do project management. This list-based approach is very loosely based on
[GTD](https://gettingthingsdone.com/), although without the focus on *next
actions* and with many other aspects removed.

*The daily log:* Each day, I pick out relevant items from my *daily* section of
the big list and copy them into my daily log where I list out everything I need
to work on that day. I add new items thoughout the day, so the daily log acts as
a kind of inbox. At the end of the day, I look through all the non-completed
items, and for each item, I either drop it or move it to the big list for
later. This approach is inspired by the daily log in the [Bullet
Journal](https://bulletjournal.com/), although I don't use the other parts.

## Comparison to other software

[The Archive](https://zettelkasten.de/the-archive/) is a proprietary application
for macOS. It's designed for buliding Zettelkästen and values simple design. Its
authors are the same as those of the excellent
[zettelkasten.de](https://zettelkasten.de/) resource. If you use macOS
exclusively and don't mind using proprietary software, I strongly recommend
this!

[Zettlr](https://www.zettlr.com/) has a similar design philosophy to The
Archive, but it's free and open-source software and available on multiple
platforms. Zettlr, like zt, does't care about the filenames of notes and
supports finding notes by title, even if the title is different from the
filename. It supports only Markdown. For the average person, this would be my
second recommendation. If you're looking for something that's compatible with
plain text or Org-mode, or just want to use Emacs, keep reading.

**zt** is an Emacs package designed for Zettelkästen and valuing simple
design. It's compatible with multiple file-formats and handles links to other
files by their IDs, such that renaming a note does not require any updates to
filenames or existing links. It lets you interactively find files by their
"logical" title (in plain text, Markdown and Org), and makes including the title
in the filename optional. The biggest disadvantage is the requirement of an
external program `ztf` that's only officially available on Linux (but can be
built for other platforms).

[zk.el](https://github.com/localauthor/zk) is another Emacs package with a very
similar design philosophy to zt. It's also inspired by The Archive, is also
designed for plain text Zettelkästen and also values simplicity in its
design. zk stores the titles of files in their filenames for the purpose of
selecting files for linking with completion. If you aren't careful about keeping
filenames and titles in sync, then zk won't be able to help you when inserting
links. However, the links themselves are stored by ID, so it is at least
possible to rename files without needing to update existing links. A major
advantage of zk compared to zt is that it doesn't have any dependencies on
external programs.

[Denote](https://protesilaos.com/emacs/denote) is another simple note-taking
package for Emacs. Denote expects you to assign tags to your files. Like zk, and
unlike zt, Denote requires you to keep filenames in sync with thier titles for
searching; like both zk and zt, however, you don't need to update existing links
whenever you change the title of a note. If you're trying to build a
Zettelkasten, I recommend using zk or zt over Denote, but Denote may be
well-suited for more general note-taking purposes. Note that zt is designed to
be interoperable with Denote by using the same ID format, so if you're using
Org-mode, transitioning between these two is just a matter of `s/zt:/denote:/g`.

[Logseq](https://logseq.com/), [Obsidian](https://obsidian.md/), [Roam
Research](https://roamresearch.com/) and others *can* be used to implement a
Zettelkasten system, but they aren't really designed for it. These programs
conflate titles with IDs, so you must manually assign unique titles to all
files. Obsidian has a Zettelkasten plugin which just adds a unique timestamp to
each file, but it still requires you to link to files with verbose links that
include the entire title. Whenever you change the title of a note, you must also
update all links to it (which can be done automatically, of course, but only
from inside Obsidian itself). If you don't mind this – or especially if you want
to build something more like a personal wiki than a strict Zettelkasten – these
are all good solutions.

[Org-mode](https://orgmode.org/) can be used on its own to build a Zettelkasten
by simply linking between files by their IDs. Although frequently characterized
as "plain text", Org-mode in fact uses a totally proprietary ID-based linking
format that isn't interoperable with any other software to my knowledge. You can
use Org-mode with zt, zk or Denote and get the best of both worlds.

[Org-roam](https://github.com/org-roam/org-roam) is essentially a port of
Obsidian to Emacs, designed for use with Org-mode. It has the advantages of
plain Org-mode together with the advantages of Obsidian and has quite a few
features. But it also has the disadvantages of both, and for the purpose of
building a Zettelkasten, I found it somewhat clumsy (not least due to its
caching system for storing backlinks), but it may be well-suited if you want
something more like a personal wiki and if you're willing to commit to using
Emacs long-term.

[emacs-zettelkasten](https://sr.ht/~ymherklotz/emacs-zettelkasten/) seems to
have similar goals both zt and zk, but I haven't tried
it. [Zetteldeft](https://github.com/EFLS/zetteldeft) is the package that zk is
based on, but I haven't tried it either.

[Hyperbole](https://www.gnu.org/software/hyperbole/) is infamous for being
rather hard to grasp, but it does seem very cool, at least from a distance!  It
does a lot more than zt and isn't really designed with Zettelkästen in mind. I
can't give a more useful comparison beyond that.

## License

zt is licensed under the terms of the GNU AGPLv3 or any later version as
described in `LICENSE.md`.
