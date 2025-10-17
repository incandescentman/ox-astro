# WARP.md

This file provides guidance to WARP (warp.dev) when working with code in this repository.

## Project Overview

ox-astro is an Emacs Lisp package that extends Org Mode's export engine to generate Astro-compatible MDX files. It's a specialized backend derived from ox-md that handles automatic asset management, front matter generation, and content transformation for static site generation.

**Core Purpose**: Transform Org files into production-ready MDX with automatic image/PDF processing, ES6 import generation, and YAML front matter.

## Development Commands

### Testing and Debugging

```bash
# Run a complete export test using the provided shell script
./run-export.sh

# Load and test specific functionality in Emacs batch mode
emacs --batch \
  --eval "(setq load-path (cons \"/path/to/ox-astro\" load-path))" \
  --eval "(require 'ox-astro)" \
  --eval "(find-file \"test-file.org\")" \
  --eval "(org-astro-export-to-mdx)"

# Run single test files
emacs --batch -l test-export.el
emacs --batch -l test-folder-preserve.el
emacs --batch -l test-conversion.el
```

### Manual Testing Commands

```elisp
;; Load the complete ox-astro system in Emacs
(add-to-list 'load-path "/path/to/ox-astro")
(require 'ox-astro-config)
(require 'ox-astro-helpers)
(require 'ox-astro-handlers)
(require 'ox-astro-table-handlers)
(require 'ox-astro-image-handlers)
(require 'ox-astro-pdf-handlers)
(require 'ox-astro)

;; Test export on current buffer
(org-astro-export-to-mdx)

;; Test subtree export (narrow first)
(org-narrow-to-subtree)
(org-astro-export-to-mdx)
```

## Architecture Overview

### Three-Phase Processing Pipeline

ox-astro uses a sophisticated three-phase pipeline to transform Org content:

1. **Pre-processing Phase**: Asset collection and copying before transcoding
2. **Element Transcoding Phase**: Convert Org elements to MDX equivalents
3. **Post-processing Phase**: Assemble front matter, imports, and final output

### Core Module Structure

- **`ox-astro.el`**: Main export engine, backend registration, and orchestration
- **`ox-astro-config.el`**: User-customizable configuration and defcustom definitions
- **`ox-astro-helpers.el`**: Core utilities (slugify, date formatting, YAML generation)
- **`ox-astro-handlers.el`**: Export filters and content processing pipeline
- **`ox-astro-table-handlers.el`**: Table transcoding functions
- **`ox-astro-image-handlers.el`**: Image detection, downloading, copying, and import generation
- **`ox-astro-pdf-handlers.el`**: PDF processing and public directory management

### Key Design Patterns

**Asset Management**: "Smart copy" system that detects images/PDFs from multiple sources (Org links, raw paths, URLs), processes them, and generates appropriate imports/components.

**Configuration System**: Flexible destination mapping with nickname support and whitespace-tolerant lookups.

**Front Matter Generation**: Automatic metadata extraction with intelligent fallbacks and source file updating.

## Critical Implementation Details

### Slug Generation Synchronization

**IMPORTANT**: Slug generation happens in multiple locations and must remain synchronized:

- `ox-astro.el` lines 118-125 (during export prep)
- `ox-astro.el` lines 187-214 (front matter phase)
- `ox-astro-helpers.el` lines 552-554 (utility functions)

Always search for "slug" across all files before making slug-related changes.

### Keyword Insertion Pattern

**Always use `org-astro--upsert-keyword-after-roam`** for inserting keywords into Org files. This helper respects org-roam properties blocks and handles narrowed buffers correctly.

**Never use** simple `insert` or `org-set-property` for keyword insertion.

### Configuration Lookups

Use **whitespace-tolerant lookups** for configuration:

```elisp
;; GOOD: Handles spaces and user input errors
(cl-find nickname org-astro-known-posts-folders
         :test (lambda (needle pair)
                 (string= needle (string-trim (car pair)))))

;; BAD: Breaks with trailing spaces
(assoc nickname org-astro-known-posts-folders)
```

### Image Data Persistence

Image import data can be lost between export phases. The system uses dual storage:

- `org-astro--current-body-images-imports` (global state)
- `:astro-body-images-imports` in info plist

Both must be updated when processing images.

## Documentation Structure

### Essential Reading Order

1. **`docs/the-ox-astro-approach.org`**: Philosophy and design rationale
2. **`docs/design-architecture.org`**: Technical architecture overview
3. **`docs/codebase-wisdom.org`**: Critical debugging insights and gotchas
4. **`docs/instructions.org`**: User operational guide

### AI Assistant Rules

**Pre-coding ritual** (DO NOT SKIP):
1. Read `docs/work-log.org` for recent session context
2. Check `docs/codebase-wisdom.org` for task-related warnings
3. Review `docs/design-architecture.org` for architectural patterns

**Common LLM Anti-patterns**:
- Modifying slug logic in one place without checking others
- Using simple string matching for config lookups
- Breaking keyword insertion by not using the roam-aware helper
- Forgetting dual-phase image data storage

### Work Session Handoff

When ending development sessions, update `docs/work-log.org` with:

- **COMPLETED**: Features fully implemented and tested
- **IN-PROGRESS**: Incomplete work with specific next steps
- **CONTEXT FOR NEXT SESSION**: Files touched, known issues, recommended reading

## Key Configuration Points

### User Customization

All user-facing configuration is in `ox-astro-config.el`:

- `org-astro-known-posts-folders`: Destination directory mappings
- `org-astro-source-root-folder`: Base directory for folder structure preservation
- `org-astro-default-author-image`: Default author image path
- `org-astro-debug-images`: Enable detailed diagnostic output

### Development Debugging

Enable debug mode for detailed diagnostics:

```elisp
(setq org-astro-debug-images t)
```

This adds extensive logging and MDX comments showing the processing pipeline.

## Testing Strategy

### Test Files Structure

- `test-*.el`: Automated test scripts for different features
- `test-*.org`: Sample Org files for testing various scenarios
- `test-files/`: Directory containing test assets and complex scenarios

### Manual Testing Workflow

1. Create test Org file with various image sources
2. Set `#+DESTINATION_FOLDER` to a test location
3. Run export and verify:
   - Images copied to correct `src/assets/images/posts/{slug}/`
   - PDFs copied to `public/pdfs/{slug}/`
   - Import statements generated correctly
   - Front matter populated with fallbacks
   - Source file updated with missing keywords

## Integration Notes

### Emacs Integration

ox-astro integrates deeply with:
- **Org Mode export engine**: Derives from `ox-md` backend
- **org-roam**: Respects properties blocks and file structure
- **Customization system**: Uses standard Emacs defcustom patterns

### Astro Project Integration

Assumes standard Astro project structure:
- `src/assets/images/posts/` for optimized images
- `public/pdfs/` for direct-serve PDFs
- ES6 import statements with `~/` alias (maps to `src/`)
- YAML front matter with specific fields

## Troubleshooting

Common issues and solutions are documented in `docs/codebase-wisdom.org`. Key areas:

- **Image processing failures**: Check file permissions and destination directories
- **Slug inconsistencies**: Verify slug generation synchronization across modules
- **Configuration lookup failures**: Ensure whitespace-tolerant matching
- **Missing imports in output**: Check dual storage of image data between phases