-- AstroCommunity: import any community modules here
-- We import this file in `lazy_setup.lua` before the `plugins/` folder.
-- This guarantees that the specs are processed before any user plugins.
--
-- Browse all available packs at https://github.com/AstroNvim/astrocommunity

---@type LazySpec
return {
  "AstroNvim/astrocommunity",

  -- Lua: lua_ls + completion for editing this Neovim config itself
  { import = "astrocommunity.pack.lua" },

  -- TypeScript / JavaScript: vtsls language server (jump-to-def, hover, rename,
  -- completion, inlay hints), treesitter parsers, and the js-debug adapter.
  { import = "astrocommunity.pack.typescript" },

  -- Python: composed from sub-packs so we get Ruff (not black/isort) for formatting.
  --   base -> treesitter, debugpy + nvim-dap-python, venv-selector (<Leader>lv)
  --   ruff -> linting + formatting, runs as its own LSP (hover handed to pyright)
  -- NOTE: We intentionally do NOT use astrocommunity.pack.python.basedpyright here.
  -- basedpyright is a PyPI package whose nodejs-wheel-binaries dep has no wheel for
  -- this machine's stock python3 (3.8) and fails to build from source. Instead we use
  -- pyright (installed via npm by Mason, configured in plugins/mason.lua + astrolsp.lua),
  -- which gives the same navigation/type-checking features.
  { import = "astrocommunity.pack.python.base" },
  { import = "astrocommunity.pack.python.ruff" },
}
