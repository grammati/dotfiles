-- Customize Mason
--
-- NOTE: The TypeScript/Python language servers and tools (vtsls, basedpyright,
-- ruff, js-debug-adapter, debugpy) are installed automatically by the
-- astrocommunity packs imported in `lua/community.lua`. Only add packages here
-- that aren't already pulled in by a pack.

---@type LazySpec
return {
  -- use mason-tool-installer for automatically installing Mason packages
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    -- overrides `require("mason-tool-installer").setup(...)`
    opts = {
      -- Make sure to use the names found in `:Mason`
      ensure_installed = {
        -- language servers
        "lua-language-server",
        "pyright", -- Python type checking / navigation (npm-based; basedpyright's PyPI build fails on this box's python3.8)

        -- formatters
        "stylua", -- Lua
        "prettier", -- JS/TS/JSON/CSS/Markdown (used via none-ls)

        -- debuggers
        "debugpy",

        -- install any other package
        "tree-sitter-cli",
      },
    },
  },
}
