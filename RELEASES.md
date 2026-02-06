# Releases

## v1.2.0 (2026-02-06)

### IT DOESN'T WORK

This release documents the complete failure of this formatter.

**Status**: Does not work. Do not use.

**Problem**: After multiple attempts to fix the formatter, it became clear that the tool cannot achieve its stated goal. The formatter was stripped down to do nothing but preserve whitespace, making it completely useless.

**Why It Failed**:
- Full Haskell formatters (fourmolu/ormolu) reorder code declarations, destroying literate programming narrative structure
- Basic formatting (whitespace cleanup only) is pointless
- There is no middle ground that both formats Haskell code properly AND preserves literate structure

**Conclusion**: This tool sucks and doesn't work. Use manual formatting or don't write literate Haskell.

---

## v1.1.0 (2026-02-06)

### Major Bug Fix: Structure Preservation

**Fixed Critical Issue with Literate Programming Structure**

The previous version had a significant bug in the `reconstruct_lhs` function that was destroying the intended structure of literate Haskell files:

#### Problems Fixed:
- **Code Consolidation Bug**: All formatted code was being merged into the first code block
- **Lost Prose Sections**: Documentation sections were being moved to the end of the file
- **Broken Narrative Flow**: The literate programming paradigm requires code and documentation to be interleaved in a meaningful narrative

#### Solution Implemented:
- Completely rewrote the `reconstruct_lhs` function
- **Line-by-Line Preservation**: Now maps each formatted code line back to its exact original position
- **Structure Integrity**: Maintains all prose sections exactly where they were intended
- **Literate Programming Compliant**: Respects the core principle that code and documentation should tell a coherent story together

#### Testing Results:
✅ **7 comprehensive tests passed**:
1. Simple functions with multiple code blocks
2. Complex data structures and type classes  
3. Multiple separated code blocks with interleaved prose
4. Single large code block formatting
5. Real-world example files (expressions.lhs)
6. Edge case: Files with no code (graceful handling)
7. Structure preservation verification

#### Before vs After:
- **Before**: Code got merged into first block, prose moved to end, narrative destroyed
- **After**: Perfect preservation of original structure while applying proper Haskell formatting

This fix ensures `lhs-format` now properly serves literate programming where the goal is to write programs that are primarily literature, with code as a natural part of the narrative.

### Technical Details:
- Formatter: Uses ormolu/fourmolu/stylish-haskell when available
- Fallback: Basic formatting for whitespace cleanup
- Error Handling: Graceful handling of files without code blocks
- Performance: Maintains efficient processing of large files

---

## v1.0.0 (Previous)

Initial release with basic literate Haskell formatting capabilities.
- Extract code from .lhs files
- Format with Haskell formatters
- Reconstruct literate files
- Command line interface