# zt

==TODO: Brief introduction==

## Motivation

==TODO: Briefly describe motivation for yet-another-Zettelkasten-package. See
Comparison to other software below. Especially justification for using just IDs
for filenames and handling titles as part of files themselves.==

## Installation and configuration

==TODO: Describe installation instructions. Figure out how to properly
distribute ztf. Justify use of ztf for titles (over, say, Org-roam-style
caching).==

## Getting started

==TODO: Describe recommended keybinding setup.==

==TODO: Describe expected file layout, how to use titles, Markdown, plain text
and Org files.==

## Best practices

==TODO: Write introduction to Zettelkasten generally and how to apply to Zt.==
See 20220918T031544 and related notes.

Outline:

- Understand what a Zettelkasten is and isn't. Consider using a different
  system, which can still be integrated with zt by sharing file naming scheme.
- Keep filenames and titles separate; prefer plain text, only use Org when you
  need it (mention `C-c # .`)
- Don't rely on automatic backlinks (although zt does support them)
- Do use Folgezettel to avoid overhead of adding everything to indexes and allow
  for serendipitous discovery later
- Use structure notes to enable creative connections between distant notes
- Use links sparingly; suprisingly, fewer links are better!

## License

==TODO: Release under AGPL.==

## Comparison to other software

- [The Archive](https://zettelkasten.de/the-archive/) is a proprietary software
  for macOS. It's designed for Zettelkasten and values simple design. If you use
  macOS and don't mind using proprietary software, this is what I recommend.
- [Zettlr](https://www.zettlr.com/) has a similar design philosophy to The
  Archive, but it's free and open-source software and available on multiple
  platforms. Zettlr, like zt, does not care about the filenames of notes and
  supports linking to notes by their title, even if the title is different from
  the filename. It supports only Markdown. For most people, this would be my
  second recommendation. If you're looking for something that's compatible with
  plain text or Org-mode, keep reading.
- **zt** is an Emacs package designed for Zettelkasten with a simple
  design. It's compatible with multiple file-formats and handles links to other
  files by their IDs, such that renaming a note does not require any updates to
  filenames or existing links. It lets you interactively find files by their
  "logical" title (in plain text, Markdown and Org), and makes including the
  title in the filename optional.
- [zk.el](https://github.com/localauthor/zk) is another Emacs package with a
  very similar design philosophy to zt. It's also inspired by The Archive, is
  also designed for Zettelkasten and also values simplicity in its design. zk
  stores the titles of files in their filenames for the purpose of selecting
  files for linking with completion, so if you forget to update the filename,
  you won't be able to find the file you want to link to. However, links are by
  ID, so it's possible to rename files without needing to update existing
  links. If you basically like zt but don't want to use it for some reason, this
  is what I recommend.
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
  Obsidian to Emacs, designed for use with Org-mode. I have only briefly
  evaluated it, but I think it has basically the same drawbacks as standard
  Org-mode with additional overhead (notably, requiring a cache for
  backlinking). The main advantage is that it adds backlinking support. For my
  purposes (building a Zettelkasten), it was clumsy and over-designed, but, like
  Obsidian, if you're building something more like a personal wiki, it might be
  a perfect fit.
- [emacs-zettelkasten](https://sr.ht/~ymherklotz/emacs-zettelkasten/): I haven't
  tried it.
- [Zetteldeft](https://github.com/EFLS/zetteldeft): I also haven't tried this;
  it was the inspiration for zk.
