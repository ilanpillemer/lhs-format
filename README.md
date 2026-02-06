# lhs-format

A literate Haskell formatter for the acme text editor.

## Description

`lhs-format` is a command-line tool that formats bird-style literate Haskell (.lhs) files while preserving all prose text exactly as written. It extracts code lines (those starting with `>`), applies professional Haskell formatting, and reconstructs the literate file with properly formatted code.

The formatter integrates seamlessly with the acme text editor through acmego, providing automatic formatting on file save.

## Features

- **Prose Preservation**: All non-code text is preserved exactly as written
- **Professional Code Formatting**: Uses fourmolu (when available) for high-quality Haskell formatting
- **Smart Fallback**: Gracefully falls back to basic whitespace cleanup when advanced formatters aren't available
- **Line Count Handling**: Correctly handles formatters that add or remove lines (blank lines, imports, etc.)
- **Edge Case Support**: Handles comments, empty lines, indented prose, and complex code structures
- **Acme Integration**: Works seamlessly with acmego for automatic formatting in acme

## Requirements

- bash (POSIX-compliant shell)
- sed
- **Optional but recommended**: fourmolu, ormolu, or stylish-haskell for advanced formatting
- For acme integration: acmego with literate Haskell support

## Installation

### Standalone Installation

```bash
# Clone the repository
git clone https://github.com/ilanpillemer/lhs-format.git
cd lhs-format

# Make executable and copy to PATH
chmod +x lhs-format
cp lhs-format /usr/local/bin/
# or
cp lhs-format ~/bin/  # if ~/bin is in your PATH
```

### Install Formatters (Optional)

For best results, install a Haskell formatter:

```bash
# Install fourmolu (recommended)
cabal install fourmolu

# Or try ormolu
cabal install ormolu

# Or stylish-haskell
cabal install stylish-haskell
```

### Acme Integration

To enable automatic formatting in acme:

1. Ensure `lhs-format` is in your PATH
2. Use acmego with literate Haskell support (see [acmego fork](https://github.com/ilanpillemer/go/tree/fork/acme/acmego))
3. Run: `acmego -f`

## Usage

### Command Line

```bash
# Format file in-place
lhs-format myfile.lhs

# Format from stdin to stdout
cat myfile.lhs | lhs-format > formatted.lhs

# Use in a pipeline
lhs-format input.lhs && echo "Formatted successfully"
```

### Example

**Before formatting:**
```haskell
This is my literate Haskell program.

> module Example where
> import Data.List
> data Tree a=Empty|Node a(Tree a)(Tree a)
> func x y z= if x>0 then y+z else y-z
> complexExpr = map(\x->x*2+1)[1,2,3,4,5]

This explains the Tree data structure and helper functions.

> insert :: (Ord a) => a -> Tree a -> Tree a
> insert x Empty=Node x Empty Empty
> insert x (Node a left right)|x<a=Node a(insert x left)right
```

**After formatting with fourmolu:**
```haskell
This is my literate Haskell program.

> module Example where
> 
> import Data.List
> 
> data Tree a = Empty | Node a (Tree a) (Tree a)
> func x y z = if x > 0 then y + z else y - z
> complexExpr = map (\x -> x * 2 + 1) [1, 2, 3, 4, 5]

This explains the Tree data structure and helper functions.

> insert :: (Ord a) => a -> Tree a -> Tree a
> insert x Empty = Node x Empty Empty
> insert x (Node a left right)
>   | x < a = Node a (insert x left) right
>   | x > a = Node a left (insert x right)
```

## Supported Formatters

The formatter automatically detects and uses available Haskell formatters in order of preference:

1. **ormolu** - Modern formatter with minimal diffs (if compatible with your GHC version)
2. **fourmolu** - Configurable fork of ormolu with excellent results
3. **stylish-haskell** - Import and language pragma formatter
4. **basic** - Fallback whitespace cleanup

## How It Works

1. **Extract**: Code lines (starting with `>`) are extracted from the .lhs file
2. **Format**: The extracted Haskell code is formatted using the best available formatter
3. **Reconstruct**: The .lhs file is rebuilt with formatted code and original prose preserved
4. **Output**: Result is written to the original file (in-place) or stdout

The formatter handles complex scenarios where formatters add or remove lines by treating code sections as logical blocks rather than line-by-line replacements.

## Limitations

- Currently supports bird-style (`> code`) literate Haskell only
- LaTeX-style (`\begin{code}...\end{code}`) blocks are not yet supported
- Designed primarily for acme editor integration

## Contributing

Contributions are welcome! Areas for improvement:

- LaTeX-style literate Haskell support
- Better formatter detection and configuration
- Performance optimizations
- Additional editor integrations

## License

MIT License - see LICENSE file for details.

## Related Projects

- [acmego](https://github.com/ilanpillemer/go/tree/fork/acme/acmego) - Acme formatting daemon with .lhs support
- [fourmolu](https://github.com/fourmolu/fourmolu) - Haskell code formatter
- [ormolu](https://github.com/tweag/ormolu) - Haskell source code formatter

## Author

Created for use with the acme text editor as part of a literate programming workflow.