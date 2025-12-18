# My Emacs Config

Personal Emacs configuration using [Elpaca](https://github.com/progfolio/elpaca) package manager with a modular architecture.

## Installation

```bash
git clone https://github.com/vshender/.emacs.d ~/.emacs.d
```

On first launch, Elpaca will automatically install all required packages.

## Dependencies

### Tools

- [fd](https://github.com/sharkdp/fd) - Fast file finder.
  Used for quickly locating files in projects.

- [ripgrep](https://github.com/BurntSushi/ripgrep) - Fast recursive search tool.
  Used by `consult-ripgrep` for project-wide text search and by the MCP ripgrep server for LLM-assisted code search.

- [dasel](https://github.com/TomWright/dasel) - JSON/YAML/TOML query tool.
  Used by pet to parse Python project config files (pyproject.toml, etc.) for virtual environment detection.

### Language Servers

- [basedpyright](https://github.com/DetachHead/basedpyright) - Python language server.
  Provides code completion, diagnostics, and navigation for Python via eglot.
  Can be installed in the project's virtual environment.

### Linters

- [ruff](https://github.com/astral-sh/ruff) - Fast Python linter and formatter.
  Used by `flymake-ruff` for real-time Python linting.
  Can be installed in the project's virtual environment.

## LLM Configuration

gptel requires API keys for the configured backends.  See [gptel documentation](https://github.com/karthink/gptel?tab=readme-ov-file#setup) for more details.
