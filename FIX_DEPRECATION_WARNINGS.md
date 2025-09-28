# ox-astro Deprecation Warning Fixes

## Date: 2024-09-28
## Purpose: Fix Emacs deprecation warnings without changing functionality

## Changes Made:

### 1. Fixed unescaped character literal in ox-astro-helpers.el

**File:** `ox-astro-helpers.el`
**Line:** 460
**Warning:** `unescaped character literals '?"' detected, '?\"' expected`

**Original code:**
```elisp
((and (or (= ch ?") (= ch ?')) (null buf))
```

**Fixed code:**
```elisp
((and (or (= ch ?\") (= ch ?')) (null buf))
```

**Explanation:** In modern Emacs Lisp, the double-quote character in character literals must be escaped with a backslash. This is purely a syntax change - the functionality remains identical. The code still checks if the character is a double-quote or single-quote.

### 2. Replace obsolete 'return' with 'cl-return' in ox-astro-image-handlers.el

**File:** `ox-astro-image-handlers.el`
**Warning:** `'return' is an obsolete alias (as of 27.1); use 'cl-return' instead`

These changes need to be made at line 132 (and possibly other locations).

**Original code:**
```elisp
(return)
```

**Fixed code:**
```elisp
(cl-return)
```

**Explanation:** The `return` macro has been obsolete since Emacs 27.1. It's now part of the Common Lisp compatibility library (cl-lib) and should be called as `cl-return`. This change maintains exactly the same behavior - it exits from the enclosing `dolist` loop.

## Testing Instructions:

1. After making these changes, restart Emacs
2. Load ox-astro normally
3. Test exporting an org file with images to verify functionality is unchanged
4. The deprecation warnings should no longer appear

## Notes:

- These are purely syntactic changes to comply with modern Emacs Lisp standards
- No functional changes are made
- The code behavior remains exactly the same