# ox-astro Project Understanding

## Project Overview
**ox-astro** is an Emacs Lisp package that extends Org Mode's export engine to generate Astro-compatible MDX files for static site generation and blogging workflows.

## Core Architecture

### File Structure
- `ox-astro.el` - Main exporter engine and Org backend registration
- `ox-astro-config.el` - User-customizable configuration options
- `ox-astro-helpers.el` - Utility functions (slugify, date formatting, variable naming)
- `ox-astro-handlers.el` - Export filters and content processing pipeline

### Key Components

#### Export Pipeline
1. **Parse Tree Processing**: Collects and processes images before transcoding
2. **Content Transformation**: Converts Org syntax to MDX-compatible Markdown
3. **Asset Management**: Copies images and generates import statements
4. **Front Matter Generation**: Creates YAML metadata from Org keywords

#### Image Handling System
- **Cover Images**: From `#+COVER_IMAGE` keyword � import statements + front matter
- **Body Images**: From `[[file:path]]` links � copied to assets + `<img>` tags
- **Raw Paths**: Standalone image paths � auto-detected and processed
- **Variable Naming**: Converts filenames to camelCase JS variables

#### Content Transformations
- TODO items � Markdown task lists (`- [ ]` / `- [x]`)
- Example blocks � blockquotes
- Special code blocks (user/prompt/quote) � preserved with formatting
- Raw URLs � `<LinkPeek>` components
- Literal characters preserved (vs HTML entities)

## Configuration System

### Known Posts Folders
```elisp
org-astro-known-posts-folders
'(("actions" . "/path/to/actions/src/content/blog")
  ("jaydocs" . "/path/to/jaydocs/src/content/blog")
  ("socratic" . "/path/to/socratic/src/content/blog"))
```

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
1. **Check parse tree**: `ox-astro-handlers.el` → `org-astro-prepare-images-filter` (runs first)
2. **Check transcoding**: `ox-astro-helpers.el` → specific `org-astro-*` functions
3. **Check final output**: `ox-astro-handlers.el` → `org-astro-final-output-filter` (runs last)

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
