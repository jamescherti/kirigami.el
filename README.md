# kirigami.el - A Unified Method to Fold and Unfold Text in Emacs Across a Diverse Set of Modes
![Build Status](https://github.com/jamescherti/kirigami.el/actions/workflows/ci.yml/badge.svg)
[![MELPA](https://melpa.org/packages/kirigami-badge.svg)](https://melpa.org/#/kirigami)
[![MELPA Stable](https://stable.melpa.org/packages/kirigami-badge.svg)](https://stable.melpa.org/#/kirigami)
![License](https://img.shields.io/github/license/jamescherti/kirigami.el)
![](https://jamescherti.com/misc/made-for-gnu-emacs.svg)

The **kirigami** Emacs package provides a unified method to fold and unfold text in Emacs across a diverse set of Emacs modes.

**Supported modes include:** `outline-mode`, `outline-minor-mode`, `outline-indent-minor-mode`, `org-mode`, `markdown-mode`, `gfm-mode`, `embark-collect-mode`, `vdiff-mode`, `vdiff-3way-mode`, `hide-ifdef-mode`, `vimish-fold-mode`, `TeX-fold-mode` (AUCTeX), `fold-this-mode`, `origami-mode`, `yafolding-mode`, `folding-mode`, `ts-fold-mode`, `treesit-fold-mode`, and `hs-minor-mode` (hideshow).

With Kirigami, folding key bindings only need to be configured **once**. After that, the same keys work consistently across all supported major and minor modes, providing a unified and predictable experience for opening and closing folds. The available commands include:

- `kirigami-open-fold`: Open the fold at point.
- `kirigami-open-fold-rec`: Open the fold at point recursively.
- `kirigami-close-fold`: Close the fold at point.
- `kirigami-open-folds`: Open all folds in the buffer.
- `kirigami-close-folds`: Close all folds in the buffer.
- `kirigami-toggle-fold`: Toggle the fold at point.

In addition to unified interface, the kirigami package enhances folding behavior in `outline`, `markdown-mode`, and `org-mode` packages. It ensures that deep folds open reliably, allows folds to be closed even when the cursor is positioned inside the content, and ensures that sibling folds at the same level are visible when a sub-fold is expanded. When Kirigami closes outline folds, it preserves the visibility of folded headings in the window. Additionally, it resolves upstream Emacs issues, such as [bug#79286](https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-08/msg01128.html).

If **kirigami** enhances your workflow, please show your support by **⭐ starring kirigami.el on GitHub** to help more Emacs users discover its benefits.

## Features

Here are the features that **kirigami** offers:
- **Uniform commands**: The same commands and keys can be used to open, close, toggle, or check folds, no matter what mode is active. (Commands: `kirigami-open-fold`, `kirigami-open-fold-rec`, `kirigami-open-folds`, `kirigami-close-fold`, `kirigami-toggle-fold`, `kirigami-close-folds`)
- **Automatic handler selection**: Kirigami automatically chooses the right folding method based on the mode being used.
- **Extensible fold list**: Users can easily add or customize folding methods for different modes through the `kirigami-fold-list` alist.
- Support for multiple folding backends, including:
  - `outline-mode`, `outline-minor-mode` (built-in)
  - `hs-minor-mode` (hideshow, built-in)
  - `outline-indent-minor-mode` ([outline-indent.el](https://github.com/jamescherti/outline-indent.el), a package that enables code folding based on indentation)
  - `org-mode` (built-in)
  - `markdown-mode` and `gfm-mode` ([markdown-mode](https://github.com/jrblevin/markdown-mode))
  - `vdiff-mode` and `vdiff-3way-mode`
  - `treesit-fold-mode` ([treesit-fold](https://github.com/emacs-tree-sitter/treesit-fold), which provides intelligent code folding by using the structural understanding of the built-in tree-sitter parser)
  - `embark-collect-mode`
  - `hide-ifdef-mode`
  - `vimish-fold-mode`
  - `origami-mode`
  - `TeX-fold-mode` (AUCTeX)
  - `fold-this-mode`
  - `yafolding-mode`
  - `ts-fold-mode`
- In addition to unified interface for opening and closing folds, the **kirigami** package:
  - **Visual Stability:** Avoids the disruptive window jump by preserving the cursor's exact vertical position when expanding or collapsing headings, maintaining a constant relative distance between the cursor and the window start.
  - **Enhances outline:** Improves folding in `outline-mode`, `outline-minor-mode`, `markdown-mode`, `gfm-mode`, and `org-mode` by ensuring deep folds open reliably, keeping folded headings visible, and collapsing to the shallowest heading level.
  - **Hooks:** `kirigami-pre-action-predicates` and `kirigami-post-action-functions` run before and after each fold, allowing actions to be allowed or blocked and enabling UI or external updates after changes.

The kirigami package supports Emacs version 26.3 and above.

## Installation

To install **kirigami** from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).

2. Add the following code to your Emacs init file to install **kirigami** from MELPA:
```elisp
(use-package kirigami)
```

## Usage

### Vanilla Emacs Key bindings

Here is an example of default key bindings:
```elisp
(global-set-key (kbd "C-c z o") 'kirigami-open-fold)     ; Open fold at point
(global-set-key (kbd "C-c z O") 'kirigami-open-fold-rec) ; Open fold recursively
(global-set-key (kbd "C-c z r") 'kirigami-open-folds)    ; Open all folds
(global-set-key (kbd "C-c z c") 'kirigami-close-fold)    ; Close fold at point
(global-set-key (kbd "C-c z m") 'kirigami-close-folds)   ; Close all folds
(global-set-key (kbd "C-c z a") 'kirigami-toggle-fold)   ; Toggle fold at point
```

### Evil-mode Key Bindings

Evil-mode users can override the default folding keys with the following configuration:

```elisp
;; Configure Kirigami to replace the default Evil-mode folding key bindings
(with-eval-after-load 'evil
  (define-key evil-normal-state-map "zo" 'kirigami-open-fold)
  (define-key evil-normal-state-map "zO" 'kirigami-open-fold-rec)
  (define-key evil-normal-state-map "zc" 'kirigami-close-fold)
  (define-key evil-normal-state-map "za" 'kirigami-toggle-fold)
  (define-key evil-normal-state-map "zr" 'kirigami-open-folds)
  (define-key evil-normal-state-map "zm" 'kirigami-close-folds))
```

### Configuring and Enabling Folding Backends

Kirigami acts as a unified interface and requires an underlying folding backend to be active in the current buffer. The following section provides a comprehensive guide on installing and enabling some of the supported folding modes.

**NOTE:** When configuring folding backends, ensure that no more than one folding minor mode is active concurrently in a single buffer, as conflicts and unexpected behavior may occur. For this reason, adding folding hooks to broad categories like `prog-mode-hook` or `text-mode-hook` is discouraged. Instead, hooks should be applied specifically to individual language modes.

#### Built-in Emacs Modes

These modes are included with Emacs by default. They only need to be enabled in the configuration.

- **outline-minor-mode:** Useful for structured text and code.
  ```elisp
  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (add-hook 'conf-mode-hook #'outline-minor-mode)
  ```

- **hs-minor-mode (Hide-Show):** Ideal for C-style languages and others that use braces `{}`.
  ```elisp
  ;; Systems and General Purpose
  (add-hook 'c-mode-hook #'hs-minor-mode)
  (add-hook 'c++-mode-hook #'hs-minor-mode)
  (add-hook 'java-mode-hook #'hs-minor-mode)
  (add-hook 'rust-mode-hook #'hs-minor-mode)
  (add-hook 'go-mode-hook #'hs-minor-mode)
  (add-hook 'ruby-mode-hook #'hs-minor-mode)

  ;; Web and Frontend
  (add-hook 'js-mode-hook #'hs-minor-mode)
  (add-hook 'typescript-mode-hook #'hs-minor-mode)
  (add-hook 'css-mode-hook #'hs-minor-mode)

  ;; Scripting, Data, and Infrastructure
  (add-hook 'sh-mode-hook #'hs-minor-mode) ; for bash/shell scripts
  (add-hook 'json-mode-hook #'hs-minor-mode)
  (add-hook 'lua-mode-hook #'hs-minor-mode)
  ```

- **org-mode:** Enabled automatically when visiting `.org` files. No additional folding setup is required for Kirigami detection.

#### Third-Party Packages

These packages must be installed from a package repository such as MELPA prior to configuration.

- **outline-indent-minor-mode:** Provides code folding based on indentation levels (highly recommended for Python, Haskell, and YAML).
  ```elisp
  ;; The outline-indent package provides a minor mode that enables code
  ;; folding based on indentation levels.
  ;; In addition to code folding, outline-indent allows:
  ;; - Moving indented blocks up and down
  ;; - Indenting/unindenting to adjust indentation levels
  ;; - Inserting a new line with the same indentation level as the current line
  ;; - Move backward/forward to the indentation level of the current line
  ;; - and other features.
  ;; URL: https://github.com/jamescherti/outline-indent.el
  (use-package outline-indent
    :commands outline-indent-minor-mode
    :custom
    (outline-indent-ellipsis " ▼"))

  ;; Python
  (add-hook 'python-mode-hook #'outline-indent-minor-mode)
  (add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)

  ;; Yaml
  (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
  (add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode)

  ;; Haskell
  (add-hook 'haskell-mode-hook #'outline-indent-minor-mode)
  ```

- **treesit-fold-mode and ts-fold-mode:** Provides structural code folding using Tree-sitter.
  ```elisp
  ;; Intelligent code folding by using the structural understanding of the
  ;; built-in tree-sitter parser. Unlike traditional folding methods that rely on
  ;; regular expressions or indentation, treesit-fold uses the actual syntax tree
  ;; of the code to accurately identify foldable regions such as functions,
  ;; classes, comments, and documentation strings.
  ;; URL: https://github.com/emacs-tree-sitter/treesit-fold
  (use-package treesit-fold
    :commands (treesit-fold-close
               treesit-fold-close-all
               treesit-fold-open
               treesit-fold-toggle
               treesit-fold-open-all
               treesit-fold-mode
               global-treesit-fold-mode
               treesit-fold-open-recursively
               treesit-fold-line-comment-mode)

    :custom
    (treesit-fold-line-count-show t)
    (treesit-fold-line-count-format " ▼")

    :config
    (set-face-attribute 'treesit-fold-replacement-face nil
                        :foreground "#808080"
                        :box nil
                        :weight 'bold))

  ;; Systems and General Purpose
  (add-hook 'c-ts-mode-hook #'treesit-fold-mode)
  (add-hook 'c++-ts-mode-hook #'treesit-fold-mode)
  (add-hook 'java-ts-mode-hook #'treesit-fold-mode)
  (add-hook 'rust-ts-mode-hook #'treesit-fold-mode)
  (add-hook 'go-ts-mode-hook #'treesit-fold-mode)
  (add-hook 'ruby-ts-mode-hook #'treesit-fold-mode)

  ;; Web and Frontend
  (add-hook 'js-ts-mode-hook #'treesit-fold-mode)
  (add-hook 'typescript-ts-mode-hook #'treesit-fold-mode)
  (add-hook 'tsx-ts-mode-hook #'treesit-fold-mode)
  (add-hook 'css-ts-mode-hook #'treesit-fold-mode)
  (add-hook 'html-ts-mode-hook #'treesit-fold-mode)

  ;; Scripting and Infrastructure
  (add-hook 'bash-ts-mode-hook #'treesit-fold-mode)
  (add-hook 'cmake-ts-mode-hook #'treesit-fold-mode)
  (add-hook 'dockerfile-ts-mode-hook #'treesit-fold-mode)

  ;; Data and Configuration
  (add-hook 'json-ts-mode-hook #'treesit-fold-mode)
  (add-hook 'toml-ts-mode-hook #'treesit-fold-mode)

  ;; Third-party
  ;; (add-hook 'kotlin-ts-mode-hook #'treesit-fold-mode)
  ;; (add-hook 'swift-ts-mode-hook #'treesit-fold-mode)
  ;; (add-hook 'elixir-ts-mode-hook #'treesit-fold-mode)
  ;; (add-hook 'zig-ts-mode-hook #'treesit-fold-mode)
  ```

- **markdown-mode and gfm-mode:** The standard major modes for Markdown files, which include native outline folding capabilities.
  ```elisp
  ;; The markdown-mode package provides a major mode for Emacs for syntax
  ;; highlighting, editing commands, and preview support for Markdown documents.
  ;; It supports core Markdown syntax as well as extensions like GitHub Flavored
  ;; Markdown (GFM).
  ;; URL: https://github.com/jrblevin/markdown-mode
  (use-package markdown-mode
    :commands (gfm-mode
               gfm-view-mode
               markdown-mode
               markdown-view-mode)
    :mode (("\\.markdown\\'" . markdown-mode)
           ("\\.md\\'" . markdown-mode)
           ("README\\.md\\'" . gfm-mode))
    :bind
    (:map markdown-mode-map
          ("C-c C-e" . markdown-do)))

  ;; Folding mode
  (add-hook 'markdown-mode-hook #'outline-minor-mode)
  ```

### Commands

Kirigami defines several interactive commands. These commands abstract over all supported folding systems:

- `kirigami-open-fold`: Open the fold at point.
- `kirigami-open-fold-rec`: Open the fold at point recursively.
- `kirigami-open-folds`: Open all folds in the buffer.
- `kirigami-close-fold`: Close the fold at point.
- `kirigami-toggle-fold`: Toggle the fold at point.
- `kirigami-close-folds`: Close all folds in the buffer.

### Extending Kirigami: Adding other fold methods

The core behavior is driven by `kirigami-fold-list`, a customizable list that associates folding actions with sets of major or minor modes. Each entry in the list specifies:

- A list of modes that act as a predicate.
- A property list describing supported folding actions.

Properties include:

- `:open-all`
  Function to open every fold in the current buffer.

- `:close-all`
  Function to close every fold in the current buffer.

- `:toggle`
  Function to toggle the fold at point.

- `:open`
  Function to open the fold at point.

- `:open-rec`
  Function to open the fold at point recursively.

- `:close`
  Function to close the fold at point.

Each property must specify a function. A value of `nil` indicates that the corresponding action is ignored for that handler.

Here is an example using the built-in `hs-minor-mode`, which Kirigami supports by default. This example demonstrates how additional folding actions can be configured:
```elisp
(push
 '((hs-minor-mode)
   :open-all    hs-show-all
   :close-all   hs-hide-all
   :toggle      hs-toggle-hiding
   :open        hs-show-block
   :open-rec    nil
   :close       hs-hide-block)
 kirigami-fold-list)
```

## Frequently Asked Questions

### Why code folding?

Code folding is about managing cognitive load, preserving spatial memory, and controlling screen real estate:

- Navigating through code (e.g., with LSP) can create a vacuum of context. Folding an entire file to its top-level headings allows the manipulation of the file skeleton directly in the main buffer. Revealing only a specific entry and its parents provides an immediate understanding of the hierarchy without losing position.
- When tasked with debugging a 20,000 line legacy file, immediate refactoring is rarely an option. Folding enables the visual modularization of massive files on the fly, making hostile codebases readable.
- Every visible line of code on the screen requires a fraction of subconscious attention to ignore. During deep debugging sessions, folding adjacent functions or complex implementations acts as a visual garbage collector.
- Moving or deleting a massive function or block is prone to selection errors. When a block is folded, it behaves as a single logical unit. Large chunks of logic can be cut, copied, or moved safely and cleanly without manually highlighting hundreds of lines.
- Folding is effective for tracking progress during extensive pull requests. Collapsing previously examined functions or blocks actively filters out visual noise and enforces a strict focus on the unreviewed code.

### Why the author developed the kirigami package?

Code folding in Emacs has historically suffered from reliability issues, which led to the development of **kirigami**. Even built-in modes such as `outline-mode` and `outline-minor-mode` which are also used by `org-mode`, `gfm-mode`, and `markdown-mode` contain bugs that have not yet been addressed upstream, and **kirigami** fixes them.

The **kirigami** package also provides a unified interface for opening and closing folds. Without it, users must manually configure keybindings for each individual mode, a tedious process. It functions as a set-and-forget enhancement for code folding. It requires configuration only once. Subsequently, the same keys and functions enable consistent folding and unfolding across all supported major and minor modes.

(The **kirigami** package is highly recommended for use with outline-based modes, such as `markdown-mode`, `gfm-mode`, `org-mode`, `outline-minor-mode`, or `outline-indent-minor-mode`. This package resolves persistent inconsistencies and prevents incorrect folding behavior.)

### What code folding packages does the author use in addition to kirigami.el?

In addition to the kirigami package, the author uses two reliable code folding packages:

1. **Indentation-based folding** (Python, YAML, Haskell, etc.): [outline-indent](https://github.com/jamescherti/outline-indent.el)
2. **Tree-sitter-based folding:** [treesit-fold](https://github.com/emacs-tree-sitter/treesit-fold) (The integration of Tree-sitter allows Emacs to operate on the Abstract Syntax Tree, making folding structurally accurate rather than heuristic.)
3. The built-in `outline-minor-mode` for `emacs-lisp-mode`, `markdown-mode`, and `conf-mode`/`conf-unix-mode`.

NOTE: The author prefers using `outline-indent` for languages like Python, despite having `treesit-fold` installed. The advantage of `outline-indent` is that it allows for infinite folding depth; it enables the folding of classes, functions within them, and even nested structures like `while` loops and `if` statements.

### Maintaining Vertical Cursor Position During Folding

Folding packages such as Outline, Outline Indent, and Org mode depend on the native Emacs redisplay engine to handle visibility changes. Expanding or collapsing folds can cause `window-start` to shift relative to the current line. When this displacement moves the cursor off-screen, Emacs triggers an abrupt recentering. This visual discontinuity disrupts spatial context and requires manual effort to relocate the cursor.

To address these issues, the following option can be used to eliminate visual discontinuities by preventing window shifts and suppressing forced recentering when headings are expanded or collapsed:

```elisp
(setq kirigami-preserve-visual-position t)
```

### What is the meaning of the word Kirigami?

Kirigami is a form of Origami, the Japanese art that transforms a flat sheet of paper into a figure through controlled folds. In kirigami, the sheet is both folded and cut to form a three-dimensional structure that rises from the surface.

Kirigami and code folding in Emacs share a structural analogy based on selective concealment and controlled revelation. In kirigami, cuts and folds create regions that can be collapsed or expanded, revealing depth or hiding detail depending on how the paper is manipulated.

Code folding in Emacs operates on a similar principle. Blocks of text remain present in the buffer, but their visibility is adjusted by folding or unfolding sections.

## Comments from users

- [neurolit on GitHub](https://github.com/jamescherti/kirigami.el/pull/1#issuecomment-3599434757): "Thank you very much for this package!"

- [jcs090218](https://github.com/jamescherti/kirigami.el/issues/2#issue-3719002530): "Thanks for this great package!"

- [RideAndRoam3C](https://www.reddit.com/r/emacs/comments/1r1vvqr/comment/o52dnak/): "I enabled it today while doing some Org technical docs work. Solid."

## Author and License

The *kirigami* Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version. Special thanks to the developers of Evil mode. Kirigami builds upon the idea and function from Evil mode to provide enhanced and unified code folding functionality to all Emacs users.

Copyright (C) 2025-2026 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [kirigami.el @GitHub](https://github.com/jamescherti/kirigami.el)

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim's Tab Bar.
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el): An Emacs package that prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking their execution if they would break the parenthetical structure.
- [stripspace.el](https://github.com/jamescherti/stripspace.el): Ensure Emacs Automatically removes trailing whitespace before saving a buffer, with an option to preserve the cursor column.
- [persist-text-scale.el](https://github.com/jamescherti/persist-text-scale.el): Ensure that all adjustments made with text-scale-increase and text-scale-decrease are persisted and restored across sessions.
- [pathaction.el](https://github.com/jamescherti/pathaction.el): Execute the pathaction command-line tool from Emacs. The pathaction command-line tool enables the execution of specific commands on targeted files or directories. Its key advantage lies in its flexibility, allowing users to handle various types of files simply by passing the file or directory as an argument to the pathaction tool. The tool uses a .pathaction.yaml rule-set file to determine which command to execute. Additionally, Jinja2 templating can be employed in the rule-set file to further customize the commands.
- [kirigami.el](https://github.com/jamescherti/kirigami.el): The *kirigami* Emacs package offers a unified interface for opening and closing folds across a diverse set of major and minor modes in Emacs, including `outline-mode`, `outline-minor-mode`, `outline-indent-minor-mode`, `org-mode`, `markdown-mode`, `vdiff-mode`, `vdiff-3way-mode`, `hs-minor-mode`, `hide-ifdef-mode`, `origami-mode`, `yafolding-mode`, `folding-mode`, and `treesit-fold-mode`. With Kirigami, folding key bindings only need to be configured **once**. After that, the same keys work consistently across all supported major and minor modes, providing a unified and predictable folding experience.
