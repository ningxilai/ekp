# Emacs-KP: Knuth-Plass Line Breaking for Emacs

Emacs-kp implements the Knuth-Plass optimal line breaking algorithm with full support for CJK (Chinese, Japanese, Korean) and Latin mixed text typesetting.

## Features

- **Optimal Line Breaking**: Uses Knuth-Plass algorithm for globally optimal paragraph layout.
- **CJK Support**: Full support for Chinese, Japanese, Korean with mixed Latin text.
- **Hyphenation**: Frank Liang's algorithm with language-specific dictionaries.
- **Text Properties Preserved**: Font faces, colors, and other Emacs text properties are maintained.
- **TypeScript Acceleration**: Optional Deno-based TypeScript module for high-performance DP computation.
- **Pure Elisp Fallback**: Always works even without TypeScript/Deno.
- **Automatic Font Handling**: Spacing parameters computed from actual font metrics.

---

## Quick Start

### Prerequisites

- **Emacs 28.1+**
- **Deno** (optional, for TypeScript acceleration)

### Installation

```elisp
(add-to-list 'load-path "/path/to/ekp")
(require 'ekp)
```

### Basic Usage

```elisp
;; Justify text to 600 pixels width
(ekp-pixel-justify "Your paragraph text here..." 600)

;; Find optimal width in a range (returns (text . optimal-width))
(ekp-pixel-range-justify "Your text" 400 800)
```

### Enable TypeScript Acceleration (Optional)

```elisp
;; Enable in Emacs
(setq ekp-use-deno-bridge t)
```
---

## Configuration

### Language Settings

**`ekp-latin-lang`** (default: `"en_US"`)

Supported languages in `dictionaries/`:
- `en_US`, `en_GB` - English
- `de_DE` - German
- `fr` - French
- `es` - Spanish
- And more...

```elisp
(setq ekp-latin-lang "de_DE")
```

### Spacing Parameters

Use `ekp-param-set` to configure spacing (in pixels):

```elisp
(ekp-param-set lws-ideal lws-stretch lws-shrink
               mws-ideal mws-stretch mws-shrink
               cws-ideal cws-stretch cws-shrink)
```

| Parameter Group | Description |
|:----------------|:------------|
| `lws-*` | Latin Word Space: between Latin words |
| `mws-*` | Mixed Word Space: between Latin and CJK |
| `cws-*` | CJK Word Space: between CJK characters |

### K-P Algorithm Parameters

| Variable | Default | Description |
|:---------|:--------|:------------|
| `ekp-line-penalty` | 10 | Base cost per line break |
| `ekp-hyphen-penalty` | 50 | Extra cost for hyphenated breaks |
| `ekp-adjacent-fitness-penalty` | 100 | Cost for inconsistent line tightness |
| `ekp-last-line-min-ratio` | 0.5 | Minimum fill ratio for last line |
| `ekp-looseness` | 0 | Target line count offset (±n lines) |

---

## Architecture

```
ekp.el          - Main Elisp implementation
ekp-bridge.el   - Fork of deno-bridge (soft dependency)
ekp.ts          - TypeScript DP implementation (optional)
dictionaries/   - Hyphenation pattern files
```

### How It Works

1. **Elisp**: Handles text preprocessing, box/glue construction, font metrics
2. **TypeScript** (optional): High-performance Knuth-Plass DP computation via deno-bridge
3. **Fallback**: Pure Elisp DP when TypeScript is unavailable

---

## Credits

- **Core Algorithm**: ["Breaking Paragraphs into Lines"](https://gwern.net/doc/design/typography/tex/1981-knuth.pdf) by Donald E. Knuth and Michael F. Plass (1981)
- **Hyphenation**: Adapted from [Pyphen](https://github.com/Kozea/Pyphen), using Liang's algorithm
- **Dictionaries**: [Hunspell hyphenation patterns](https://github.com/Kozea/Pyphen)
- **deno-bridge**: [Andy Stewart](https://github.com/manateelazycat/deno-bridge)
