# Examples

This directory contains example literate Haskell files to demonstrate the formatter's capabilities.

## Files

- `simple.lhs` - Basic literate Haskell with factorial and fibonacci functions
- `complex.lhs` - More advanced example with data types, type classes, and tree operations  
- `edge-cases.lhs` - Tests various edge cases like comments, indented prose, and empty lines

## Testing the Formatter

Try formatting these examples:

```bash
# View original
cat examples/simple.lhs

# Format and view result
./lhs-format examples/simple.lhs
cat examples/simple.lhs

# Or format to stdout
cat examples/simple.lhs | ./lhs-format
```

## Expected Behavior

The formatter should:
- Preserve all prose text exactly as written
- Apply professional formatting to code sections (proper spacing, indentation)
- Handle complex expressions, data types, and function definitions
- Manage imports and blank lines appropriately (when using fourmolu)
- Work correctly with comments, empty lines, and other edge cases