-- AstroLSP allows you to customize the features in AstroNvim's LSP configuration engine
-- Configuration documentation can be found with `:h astrolsp`
-- NOTE: We highly recommend setting up the Lua Language Server (`:LspInstall lua_ls`)
--       as this provides autocomplete and documentation while editing

---@type LazySpec
return {
  "AstroNvim/astrolsp",
  ---@type AstroLSPOpts
  opts = {
    -- Configuration table of features provided by AstroLSP
    features = {
      codelens = true, -- enable/disable codelens refresh on start
      inlay_hints = false, -- enable/disable inlay hints on start
      semantic_tokens = true, -- enable/disable semantic token highlighting
    },
    -- customize lsp formatting options
    formatting = {
      -- control auto formatting on save
      format_on_save = {
        enabled = false, -- DISABLED per your choice: format manually with <Leader>lf. Set true to auto-format on save.
        allow_filetypes = {},
        ignore_filetypes = {},
      },
      disabled = { -- disable formatting capabilities for the listed language servers
        "vtsls", -- let Prettier (none-ls) own JS/TS formatting instead of the TS server
      },
      timeout_ms = 1000, -- default format timeout
    },
    -- enable servers that you already have installed without mason
    servers = {},
    -- customize language server configuration passed to `vim.lsp.config`
    -- client specific configuration can also go in `lsp/` in your configuration root (see `:h lsp-config`)
    ---@diagnostic disable: missing-fields
    config = {
      pyright = {
        -- Default the analyzed interpreter to whatever `python` is on PATH.
        -- `<Leader>lv` (venv-selector) overrides this per-project at runtime.
        before_init = function(_, c)
          c.settings = c.settings or {}
          c.settings.python = c.settings.python or {}
          c.settings.python.pythonPath = vim.fn.exepath "python"
        end,
        settings = {
          python = {
            analysis = {
              typeCheckingMode = "basic", -- "off" | "basic" | "strict"
              autoImportCompletions = true,
              diagnosticSeverityOverrides = {
                reportUnusedImport = "information",
                reportUnusedVariable = "information",
              },
            },
          },
        },
      },
    },
    -- customize how language servers are attached
    handlers = {
      -- ["*"] = function(server) vim.lsp.enable(server) end
    },
    -- Configure buffer local auto commands to add when attaching a language server
    autocmds = {
      lsp_codelens_refresh = {
        cond = "textDocument/codeLens",
        {
          event = { "InsertLeave", "BufEnter" },
          desc = "Refresh codelens (buffer)",
          callback = function(args)
            if require("astrolsp").config.features.codelens then vim.lsp.codelens.enable(true, { bufnr = args.buf }) end
          end,
        },
      },
    },
    -- mappings to be set up on attaching of a language server
    mappings = {
      n = {
        gD = {
          function() vim.lsp.buf.declaration() end,
          desc = "Declaration of current symbol",
          cond = "textDocument/declaration",
        },
        ["<Leader>uY"] = {
          function() require("astrolsp.toggles").buffer_semantic_tokens() end,
          desc = "Toggle LSP semantic highlight (buffer)",
          cond = function(client)
            return client:supports_method "textDocument/semanticTokens/full" and vim.lsp.semantic_tokens ~= nil
          end,
        },
      },
    },
    -- A custom `on_attach` function to be run after the default `on_attach` function
    -- takes two parameters `client` and `bufnr`  (`:h lsp-attach`)
    on_attach = function(client, bufnr) end,
  },
}
