# kirigami.el - A unified interface for text folding across a diverse set of Emacs modes
![Build Status](https://github.com/jamescherti/kirigami.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/kirigami.el)
![](https://raw.githubusercontent.com/jamescherti/kirigami.el/main/.images/made-for-gnu-emacs.svg)

The **kirigami** package offers a unified interface for text folding across a diverse set of major and minor modes in Emacs, including `outline-mode`, `outline-minor-mode`, `outline-indent-mode`, `org-mode`, `markdown-mode`, `vdiff-mode`, `vdiff-3way-mode`, `hs-minor-mode`, `hide-ifdef-mode`, `origami-mode`, `yafolding-mode`, `folding-mode`, and `treesit-fold-mode`.

With Kirigami, folding key bindings only need to be configured **once**. After that, the same keys work consistently across all supported major and minor modes, providing a unified and predictable folding experience. The available commands include:

* `kirigami-open-fold`: Open the fold at point.
* `kirigami-open-fold-rec`: Open the fold at point recursively.
* `kirigami-close-fold`: Close the fold at point.
* `kirigami-open-folds`: Open all folds in the buffer.
* `kirigami-close-folds`: Close all folds in the buffer.
* `kirigami-toggle-fold`: Toggle the fold at point.

If **kirigami** enhances your workflow, please show your support by **⭐ starring kirigami.el on GitHub** to help more Emacs users discover its benefits.

(In addition to unified interface, the **kirigami** package enhances folding behavior in `outline-mode`, `outline-minor-mode`, `markdown-mode`, and `org-mode`. It ensures that deep folds open reliably and allows folds to be closed even when the cursor is positioned inside the content.)

## Features

Here are the features that **kirigami** offers:
* **Uniform commands**: The same commands and keys can be used to open, close, toggle, or check folds, no matter what mode is active. (Commands: `kirigami-open-fold`, `kirigami-open-fold-rec`, `kirigami-open-folds`, `kirigami-close-fold`, `kirigami-toggle-fold`, `kirigami-close-folds`)
* **Automatic handler selection**: Kirigami automatically chooses the right folding method based on the mode being used.
* **Extensible fold list**: Users can easily add or customize folding methods for different modes through the `kirigami-fold-list` alist.
* Support for multiple folding backends, including:
  * `outline-mode`, `outline-minor-mode`
  * `outline-indent-mode` ([outline-indent.el](https://github.com/jamescherti/outline-indent.el), a package that enables code folding based on indentation)
  * `org-mode`
  * `markdown-mode`
  * `vdiff-mode` and `vdiff-3way-mode`
  * `hs-minor-mode`
  * `hide-ifdef-mode`
  * `origami-mode`
  * `treesit-fold-mode`
* In addition to unified interface, the kirigami package enhances folding behavior in `outline-mode`, `outline-minor-mode`, `markdown-mode`, and `org-mode`. It ensures that deep folds open reliably and allows folds to be closed even when the cursor is positioned inside the content. Additionally, it resolves upstream Emacs issues, such as [bug#79286](https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-08/msg01128.html).

## Installation

### Emacs: use-package and straight

To install *kirigami* with `straight.el`:

1. It if hasn't already been done, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package kirigami
  :ensure t
  :straight (kirigami
             :type git
             :host github
             :repo "jamescherti/kirigami.el"))
```

### Alternative installation: use-package and :vc (Built-in feature in Emacs version >= 30)

To install *kirigami* with `use-package` and `:vc` (Emacs >= 30):

``` emacs-lisp
(use-package kirigami
  :ensure t
  :vc (:url "https://github.com/jamescherti/kirigami.el"
       :rev :newest))
```

### Alternative installation: Doom Emacs

Here is how to install *kirigami* on Doom Emacs:

1. Add to the `~/.doom.d/packages.el` file:
```elisp
(package! kirigami
  :recipe
  (:host github :repo "jamescherti/kirigami.el"))
```

2. Run the `doom sync` command:
```
doom sync
```

## Usage

### Vanilla Emacs Key bindings

Here is an example of default key bindings:
```elisp
(global-set-key (kbd "C-c k o") 'kirigami-open-fold)     ; Open fold at point
(global-set-key (kbd "C-c k O") 'kirigami-open-fold-rec) ; Open fold recursively
(global-set-key (kbd "C-c k m") 'kirigami-close-folds)   ; Close all folds
(global-set-key (kbd "C-c k c") 'kirigami-close-fold)    ; Close fold at point
(global-set-key (kbd "C-c k r") 'kirigami-open-folds)    ; Open all folds
(global-set-key (kbd "C-c k TAB") 'kirigami-toggle-fold) ; Toggle fold at point
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

### Commands

Kirigami defines several interactive commands. These commands abstract over all supported folding systems:

* `kirigami-open-fold`
  Open the fold at point.

* `kirigami-open-fold-rec`
  Open the fold at point recursively.

* `kirigami-open-folds`
  Open all folds in the buffer.

* `kirigami-close-fold`
  Close the fold at point.

* `kirigami-toggle-fold`
  Toggle the fold at point.

* `kirigami-close-folds`
  Close all folds in the buffer.

## Customizations

### Extending Kirigami: Adding other fold methods

The core behavior is driven by `kirigami-fold-list`, a customizable list that associates folding actions with sets of major or minor modes. Each entry in the list specifies:

* A list of modes that act as a predicate.
* A property list describing supported folding actions.

Properties include:

* `:open-all`
  Function to open every fold in the current buffer.

* `:close-all`
  Function to close every fold in the current buffer.

* `:toggle`
  Function to toggle the fold at point.

* `:open`
  Function to open the fold at point.

* `:open-rec`
  Function to open the fold at point recursively.

* `:close`
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

## Author and License

The *kirigami* Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version. Special thanks to the developers of Evil mode. Kirigami builds upon the idea and function from Evil mode to provide enhanced and unified code folding functionality to all Emacs users.

Copyright (C) 2025 James Cherti

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
