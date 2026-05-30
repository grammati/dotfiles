---@type LazySpec
return {
  "TaDaa/vimade",
  event = "VeryLazy",
  config = function()
    require("vimade").setup({})

    local group = vim.api.nvim_create_augroup("TmuxFocusDim", { clear = true })
    local dim_bg = "#0d0e11" -- ~half brightness of nvim's normal bg (#1a1d23)
    local groups = { "Normal", "NormalNC", "EndOfBuffer", "SignColumn" }
    local saved = {}

    local function snapshot()
      saved = {}
      for _, g in ipairs(groups) do
        saved[g] = vim.api.nvim_get_hl(0, { name = g })
      end
    end

    vim.api.nvim_create_autocmd("FocusLost", {
      group = group,
      callback = function()
        if vim.tbl_isempty(saved) then snapshot() end
        for _, g in ipairs(groups) do
          local hl = vim.tbl_extend("force", saved[g] or {}, { bg = dim_bg })
          vim.api.nvim_set_hl(0, g, hl)
        end
      end,
    })

    vim.api.nvim_create_autocmd("FocusGained", {
      group = group,
      callback = function()
        for _, g in ipairs(groups) do
          if saved[g] then vim.api.nvim_set_hl(0, g, saved[g]) end
        end
      end,
    })

    vim.api.nvim_create_autocmd("ColorScheme", {
      group = group,
      callback = function() saved = {} end,
    })
  end,
}
