-- Customize None-ls sources (non-LSP formatters / linters)

---@type LazySpec
return {
  "nvimtools/none-ls.nvim",
  opts = function(_, opts)
    local null_ls = require "null-ls"

    -- Check supported formatters and linters
    -- https://github.com/nvimtools/none-ls.nvim/tree/main/lua/null-ls/builtins/formatting
    -- https://github.com/nvimtools/none-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics

    -- Only insert new sources, do not replace the existing ones
    opts.sources = require("astrocore").list_insert_unique(opts.sources, {
      -- Prettier formats JS/TS/JSON/CSS/HTML/Markdown/etc. and respects a
      -- project-local .prettierrc if one exists. vtsls's own formatter is
      -- disabled in astrolsp.lua so this is the single source of truth.
      null_ls.builtins.formatting.prettier,
    })
  end,
}
