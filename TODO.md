# Quark to do list

## Bugs and the like

- Arrow keys (and other escaped keys) are not handled correctly immediately 
  following a terminal resize
- Rust's '=>' operator is tokenized correctly but printed incorrectly
  ('=' has default color)

## Missing functionality

### Lexers

- Shell script
- C
- Go
- Matlab/Octave
- ...

### UI improvements

- Intelligent expansion of root (in setRoot' of Main.hs)
  - expand to include open files (assuming they exist under root)
  - expand previously expanded directories
- Support additional terminal environments/emulators and color modes
- Layout switching
- Layout resizing
- Improve handling of permissions, write protected files and such
- Coloring in project view
- Some (presumably tab-like) UI element that shows open buffers

### Editor functionality

- Improved Home key behaviour
  - Jump to first non-whitespace on line if not already there
- Improved find and replace functionality
  - Replace all
  - Regex support

## Other things

- Code cleanup
- Documentation

## In the distant future (perhaps)

- Context aware code completion
- Macros (Haskell, Python and possibly other languages)
- Customization support
- Automatically keep project tree updated
- Navigation by typing in project tree
