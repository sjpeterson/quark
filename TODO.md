# Quark to do list

## Bugs and the like

- Resizing is delayed, causing occasional crashes when terminal window is shrunk

## Missing functionality

### Lexers

- Shell script
- C
- Rust
- Go
- Matlab/Octave
- ...

### UI improvements

- Intelligent expansion of root (in setRoot' of Main.hs)
  - expand to include open files (assuming they exist under root)
  - expand previously expanded directories
- Scrolling in project view
- Support additional terminal environments/emulators and color modes
- Layout switching
- Layout resizing
- Improve handling of permissions, write protected files and such
- Coloring in project view
- Some (presumably tab-like) UI element that shows open buffers

### Editor functionality

- Handle Insert key
- Improved find and replace functionality
  - Replace all
  - Regex

## Other things

- Code cleanup
- Documentation

## In the distant future (perhaps)

- Context aware code completion
- Macros (Haskell, Python and possibly other languages)
- Customization support
