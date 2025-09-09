# ox-astro Project Understanding

## Project Overview
**ox-astro** is an Emacs Lisp package that extends Org Mode's export engine to generate Astro-compatible MDX files for static site generation and blogging workflows.

## Core Architecture

### File Structure
- `ox-astro.el` - Main exporter engine and Org backend registration
- `ox-astro-config.el` - User-customizable configuration options
- `ox-astro-helpers.el` - Core utility functions (slugify, date formatting, variable naming)
- `ox-astro-handlers.el` - Export filters and content processing pipeline
- `ox-astro-table-handlers.el` - Table transcoding functions
- `ox-astro-image-handlers.el` - Image processing and path management
- `ox-astro-pdf-handlers.el` - PDF detection, copying, and path handling

### Key Components

#### Export Pipeline
1. **Parse Tree Processing**: Collects and processes images before transcoding
2. **Content Transformation**: Converts Org syntax to MDX-compatible Markdown
3. **Asset Management**: Copies images and generates import statements
4. **Front Matter Generation**: Creates YAML metadata from Org keywords

#### Asset Handling Systems

##### Image Handling
- **Cover Images**: From `#+COVER_IMAGE` keyword → import statements + front matter
- **Body Images**: From `[[file:path]]` links → copied to assets + `<img>` tags
- **Raw Paths**: Standalone image paths → auto-detected and processed
- **Variable Naming**: Converts filenames to camelCase JS variables
- **Destination**: `src/assets/images/posts/{slug}/`

##### PDF Handling
- **Detection**: PDF links in `[[file:path.pdf]]` format
- **Copying**: From local paths to `public/pdfs/{slug}/`
- **Path Updates**: Source Org file updated with new location
- **MDX Output**: Site-relative paths `/pdfs/{slug}/filename.pdf`
- **URL Encoding**: Spaces encoded as `%20` in output

#### Content Transformations
- TODO items → Markdown task lists (`- [ ]` / `- [x]`)
- Example blocks → blockquotes
- Special code blocks (user/prompt/quote) → preserved with formatting
- Raw URLs → `<LinkPeek>` components
- PDF links → Markdown links with URL-encoded spaces
- Literal characters preserved (vs HTML entities)

## Configuration System

### Known Posts Folders
```elisp
;; New plist format with optional folder structure preservation
org-astro-known-posts-folders
'(("blog" . (:path "~/projects/blog/src/content/blog"))
  ("docs" . (:path "~/projects/docs/src/content/docs"))
  ("roam" . (:path "~/projects/roam-site/src/content"
             :preserve-folder-structure t)))

;; Source root for calculating relative paths
org-astro-source-root-folder
"~/org-files/roam"
```

When `:preserve-folder-structure t` is set for a destination:
- The exporter calculates the relative path from `org-astro-source-root-folder` to the source file
- Preserves that directory structure in the output destination
- Example: `~/org-files/roam/journal/2025.org` → `~/projects/roam-site/src/content/journal/2025.mdx`

### Front Matter Mapping
- `#+TITLE` → `title`
- `#+AUTHOR` → `author` (defaults to "Jay Dixit")  
- `#+DATE` → `publishDate`
- `#+EXCERPT` → `excerpt`
- `#+COVER_IMAGE` → `image`
- `#+TAGS` → `tags`
- `#+CATEGORIES` → `categories`
- `#+VISIBILITY: <value>` → `visibility: <value>` (e.g., `blog`, `hidden`, `example`; arbitrary strings allowed)
- `#+STATUS: draft` → `draft: true` (only if specified)

### Fallback Logic
- Title: First level-1 headline if no `#+TITLE`
- Excerpt: First paragraph if no `#+EXCERPT`
- Date: Current time if no `#+DATE`
- Cover image alt: Generated from filename if no `#+COVER_IMAGE_ALT`

## Recent Development

### PDF Handling Implementation (Latest)
- Implemented automatic PDF handling that mirrors the image processing system
- PDFs are automatically copied from local paths (e.g., Downloads) to `public/pdfs/{slug}/`
- Source Org files are updated with the new path after copying
- MDX output generates correct site-relative paths (`/pdfs/{slug}/filename.pdf`)
- Fixed docstring syntax error that was preventing PDF processing

### Previous Work
Based on CHANGE-LOG.org, recent work focused on raw image path processing - detecting absolute paths in document text and automatically converting them to proper Astro Image components with imports.

## Usage Workflow
1. User runs `M-x org-astro-export-to-mdx`
2. System determines posts folder via:
   - First: Check if `#+DESTINATION_FOLDER` matches a known folder nickname
   - Second: Check if `#+DESTINATION_FOLDER` is a valid absolute path
   - Third: Prompt user to select from known folders
3. Exporter processes content, copies assets, generates MDX with front matter
4. Output ready for Astro build pipeline

## Posts Folder Resolution
The `#+DESTINATION_FOLDER` keyword now supports three modes:
1. **Nickname**: `#+DESTINATION_FOLDER: jaydocs` → looks up in `org-astro-known-posts-folders`
2. **Absolute Path**: `#+DESTINATION_FOLDER: /full/path/to/posts/` → uses path directly if directory exists
3. **Interactive**: If not specified or invalid, prompts user to select from known folders

### Folder Structure Preservation
Destinations can optionally preserve source folder structure:
- Configured per-destination with `:preserve-folder-structure t` in `org-astro-known-posts-folders`
- When enabled, maintains relative path from `org-astro-source-root-folder` to source file
- Useful for maintaining organization when exporting large collections (e.g., org-roam databases)

## Development Reference Guide

### What Files to Look At When

#### Adding New Export Functionality
- **Primary**: `ox-astro-helpers.el` - Add new transcoding functions (e.g., `org-astro-new-element`)
- **Registration**: `ox-astro.el` - Add to `:translate-alist` to override specific Org elements
- **Reference**: `org-reference-backends/ox-md.el` - Check how standard markdown handles the element first

#### Modifying Content Processing Pipeline
- **Pre-processing**: `ox-astro-handlers.el` → `org-astro-prepare-images-filter` (parse tree modifications)
- **Post-processing**: `ox-astro-handlers.el` → `org-astro-final-output-filter` (string transformations)
- **Body Assembly**: `ox-astro-handlers.el` → `org-astro-body-filter` (front matter + imports + content)

#### Image/Asset Handling Issues
- **Path Processing**: `ox-astro-helpers.el` → `org-astro--process-image-path`, `org-astro--collect-images-from-tree`
- **Import Generation**: `ox-astro-handlers.el` → `org-astro-prepare-images-filter`
- **Component Output**: `ox-astro-helpers.el` → `org-astro-paragraph`, `org-astro-plain-text`

#### Front Matter Problems
- **Data Collection**: `ox-astro-helpers.el` → `org-astro--get-front-matter-data`
- **YAML Generation**: `ox-astro-helpers.el` → `org-astro--gen-yaml-front-matter`
- **Keyword Mapping**: `ox-astro.el` → `:options-alist`

#### Configuration Issues
- **User Settings**: `ox-astro-config.el` - Default values, folder mappings
- **Export Options**: `ox-astro.el` → `:options-alist` - Keyword to plist mappings

#### Debugging Export Errors
1. **Enable debug mode**: `(setq org-astro-debug-images t)` - enables comprehensive logging
2. **Check debug log**: `ox-astro-debug.log` in project directory - contains detailed export trace
3. **Check clipboard**: File paths (source, output, debug) are automatically copied on export
4. **Check parse tree**: `ox-astro-handlers.el` → `org-astro-prepare-images-filter` (runs first)
5. **Check transcoding**: `ox-astro-helpers.el` → specific `org-astro-*` functions
6. **Check final output**: `ox-astro-handlers.el` → `org-astro-final-output-filter` (runs last)

#### Debug System Features
- **Comprehensive Logging**: Set `org-astro-debug-images t` to enable detailed logging to `ox-astro-debug.log`
- **Automatic Clipboard Copy**: Export automatically copies source, output, and debug file paths to clipboard
- **Session Headers**: Each export session includes timestamp and file paths in debug log
- **Gallery Debugging**: Detailed logging of gallery image matching and processing
- **Image Processing Trace**: Complete trace of image collection, copying, and import generation

#### Understanding Design Decisions
- **Architecture**: `docs/design-approach.org` - Why we override certain elements, design rationale
- **Change History**: `CHANGE-LOG.org` - Recent development and feature additions

### Testing Your Changes
Always test with problematic files like:
- `20250731235900-fringe_global_south.org` (complex content with embedded data)
- Files in `debug/` folder (edge cases and test scenarios)

## Project Structure Notes
- `debug/` - Test files and debugging content (e.g., test-arbitrary-folder.org)
- `docs/` - Documentation and architecture decisions
- `org-reference-backends/` - Reference implementations from standard Org exporters
- `future/` - Planned features and roadmap
