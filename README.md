# My Emacs Config

Personal Emacs configuration using [Elpaca](https://github.com/progfolio/elpaca) package manager with a modular architecture.

## Installation

```bash
git clone https://github.com/vshender/.emacs.d ~/.emacs.d
```

On first launch, Elpaca will automatically install all required packages.

## Dependencies

The following external tools are required:

- [fd](https://github.com/sharkdp/fd) - Fast file finder.
  Used for quickly locating files in projects.

- [ripgrep](https://github.com/BurntSushi/ripgrep) - Fast recursive search tool.
  Used by `consult-ripgrep` for project-wide text search and by the MCP ripgrep server for LLM-assisted code search.

## LLM Configuration

gptel requires API keys for the configured backends.  See [gptel documentation](https://github.com/karthink/gptel?tab=readme-ov-file#setup) for more details.
