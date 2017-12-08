# Quark to do list

## Bugs and the like
- "quark: user error (Curses[-1]:waddnstr)" moving cursor right sometimes
- Arrow keys (and other escaped keys) are not handled correctly immediately 
  following a terminal resize in xterm-256color

## Missing functionality

### Lexers

- Shell script
- C
- Go
- Matlab/Octave
- ...

### UI improvements

- Improved support for non 256-color terminals and tty
- Improve handling of permissions, write protected files and such
- Coloring in project view
- Some (presumably tab-like) UI element that shows open buffers

### Editor functionality

- Improved find and replace functionality
  - Replace all
  - Regex support

## Other things

- Code cleanup
- Improved documentation

## Further down the road (possibly)

- Something similar to Emacs IDO
- Source code formatting and similar per-language-tools (cf. brittany)
- Context aware code completion
- Macros (Haskell, Python and possibly other languages)
- Customization support
- Automatically keep project tree updated
- Navigation by typing in project tree
