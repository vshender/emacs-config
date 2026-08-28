# My Emacs Config

Personal Emacs configuration using [Elpaca](https://github.com/progfolio/elpaca) package manager with a modular architecture.

## Installation

```bash
git clone https://github.com/vshender/.emacs.d ~/.emacs.d
```

On first launch, Elpaca will automatically install all required packages.  Tree-sitter grammars are compiled on demand when a file of the corresponding language is opened (see [Core](#core) for the tools this needs).

## Dependencies

External programs the configuration relies on, grouped by area.  Everything outside [Core](#core) is optional: a missing tool only disables the corresponding feature.

### Core

- [git](https://git-scm.com/) - Version control.
  Used by Elpaca to fetch packages, by `magit`, and to clone tree-sitter grammars.
  Install via `sudo pacman -S git` (Arch) or `xcode-select --install` (macOS).

- C compiler (gcc or clang) - Needed to compile tree-sitter grammars, which are installed automatically into `var/tree-sitter/`.
  Install via `sudo pacman -S gcc` (Arch) or `xcode-select --install` (macOS).

- [ripgrep](https://github.com/BurntSushi/ripgrep) - Fast recursive search tool.
  Used by `consult-ripgrep` for project-wide text search and by the MCP ripgrep server for LLM-assisted code search.
  Install via `sudo pacman -S ripgrep` (Arch) or `brew install ripgrep` (macOS).

- [JetBrainsMono Nerd Font](https://www.nerdfonts.com/) (Linux) - Frame font; its Nerd Font glyphs also serve `nerd-icons`.
  Install via `sudo pacman -S ttf-jetbrains-mono-nerd` (Arch).  On macOS the frame font is Menlo, so install the icon font with `M-x nerd-icons-install-fonts`.

### Programming languages

#### OCaml

OCaml tooling is installed via [opam](https://opam.ocaml.org/):

```bash
opam install ocaml-lsp-server ocamlformat ocp-indent utop
```

- [ocaml-lsp-server](https://github.com/ocaml/ocaml-lsp) - OCaml language server.
  Provides code completion, diagnostics, and navigation via eglot and `ocaml-eglot`.

- [ocamlformat](https://github.com/ocaml-ppx/ocamlformat) - OCaml code formatter.
  Used by ocaml-lsp-server (through `ocamlformat-rpc`) to pretty-print type signatures shown on hover.  Code formatting itself is only invoked explicitly (`eglot-format`) and requires an `.ocamlformat` file in the project.

- [ocp-indent](https://github.com/OCamlPro/ocp-indent) - OCaml indentation tool.
  Used by `ocp-indent` mode for automatic indentation of OCaml code.

- [utop](https://github.com/ocaml-community/utop) - Improved OCaml REPL.
  Used by `utop` mode for interactive evaluation from OCaml buffers.

#### Python

- [basedpyright](https://github.com/DetachHead/basedpyright) - Python language server.
  Provides code completion, diagnostics, and navigation via eglot.
  Install via `uv tool install basedpyright` (any platform) or `brew install basedpyright` (macOS), or into the project's virtual environment.

- [ruff](https://github.com/astral-sh/ruff) - Fast Python linter and formatter.
  Used by `flymake-ruff` for real-time linting.
  Install via `uv tool install ruff` (any platform), `sudo pacman -S ruff` (Arch), or `brew install ruff` (macOS), or into the project's virtual environment.

- [dasel](https://github.com/TomWright/dasel) - JSON/YAML/TOML query tool.
  Used by `pet` to parse Python project config files (pyproject.toml, etc.) for virtual environment detection.
  Install `dasel` from the AUR (Arch) or via `brew install dasel` (macOS).

#### Web and data formats

- [vscode-html-languageserver](https://github.com/microsoft/vscode-html-languageservice) - HTML language server.
  Provides completion, hover documentation, and formatting for HTML files.
  Install via `sudo pacman -S vscode-html-languageserver` (Arch).

- [vscode-css-languageserver](https://github.com/microsoft/vscode-css-languageservice) - CSS language server.
  Provides completion, diagnostics, and color previews for CSS/SCSS/LESS files.
  Install via `sudo pacman -S vscode-css-languageserver` (Arch).

- [vscode-json-languageserver](https://github.com/microsoft/vscode-json-languageservice) - JSON language server.
  Provides schema validation, completion, and diagnostics for JSON/JSONC files.
  Install via `sudo pacman -S vscode-json-languageserver` (Arch).

- [yaml-language-server](https://github.com/redhat-developer/yaml-language-server) - YAML language server.
  Provides schema validation, completion, and diagnostics for YAML files.
  Install via `sudo pacman -S yaml-language-server` (Arch) or `brew install yaml-language-server` (macOS).

- [jq](https://github.com/jqlang/jq) - JSON processor.
  Used by `jq-mode` for interactive JSON filtering.
  Install via `sudo pacman -S jq` (Arch) or `brew install jq` (macOS).

Note: outside Arch, the `vscode-*-languageserver` servers above are shipped by the npm package `vscode-langservers-extracted`, whose executables are named `vscode-*-language-server` (with a hyphen); the configuration expects the Arch names.

### Documents and notes

- [pandoc](https://pandoc.org/) - Universal document converter.
  Used by `markdown-mode` for document preview and export.
  Install via `sudo pacman -S pandoc-cli` (Arch) or `brew install pandoc` (macOS).

- [TeX Live](https://tug.org/texlive/) and [ImageMagick](https://imagemagick.org/) - LaTeX preview in Org (`pdflatex` + `magick`).
  The preview preamble uses the `fontenc` (T2A), `babel` (russian), `tikz`, and `algpseudocode` packages.
  Install via `sudo pacman -S texlive-basic texlive-latex texlive-langcyrillic texlive-pictures texlive-mathscience imagemagick` (Arch) or `brew install --cask mactex` and `brew install imagemagick` (macOS).

### Notifications

- [libnotify](https://gitlab.gnome.org/GNOME/libnotify) (Linux) / [terminal-notifier](https://github.com/julienXX/terminal-notifier) (macOS) - Desktop notification tools.
  Used by `alert` for sending desktop notifications (e.g., pomodoro timer events).
  Install via `sudo pacman -S libnotify` (Arch), `sudo apt install libnotify-bin` (Debian/Ubuntu), or `brew install terminal-notifier` (macOS).

### AI assistants

- [Claude Code](https://docs.anthropic.com/en/docs/claude-code) CLI (`claude`) - Required by `claude-code.el`.
  Install `claude-code` from the AUR (Arch) or via `brew install --cask claude-code` (macOS).

- [Node.js](https://nodejs.org/) (`npx`) and [uv](https://github.com/astral-sh/uv) (`uvx`) - Run the MCP servers configured for `mcp.el` on demand: filesystem and ripgrep via `npx`, fetch and shell via `uvx`.
  Install via `sudo pacman -S nodejs npm uv` (Arch) or `brew install node uv` (macOS).

- API keys for the gptel backends.  See [gptel documentation](https://github.com/karthink/gptel?tab=readme-ov-file#setup) for details.
