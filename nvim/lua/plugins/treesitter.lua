-- Treesitter setup for AstroNvim v5 on Neovim 0.12.
--
-- Two things are needed to make treesitter highlighting work in this combination:
--   1. Pin nvim-treesitter to the classic `master` branch. AstroNvim v5 uses classic
--      treesitter (compiles parsers with your local `cc`, no external CLI), but the
--      repo's DEFAULT branch is now `main` (the rewrite, which needs tree-sitter-cli
--      >=0.26.1 — unavailable on this box's glibc 2.31). `commit = false` ignores any
--      stale `main` commit left in lazy-lock.json so lazy tracks the `master` HEAD.
--   2. Start highlighting natively per-buffer. AstroNvim v5's classic highlight
--      auto-attach silently fails on Neovim 0.12, so we call `vim.treesitter.start`
--      ourselves on FileType (no-op when the buffer has no parser, and guarded so it
--      doesn't double-start buffers classic highlighting already handles, e.g. lua).

---@type LazySpec
return {
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "master",
    commit = false,
    pin = false,
  },
  {
    "AstroNvim/astrocore",
    ---@type AstroCoreOpts
    opts = {
      autocmds = {
        treesitter_highlight = {
          {
            event = "FileType",
            desc = "Start native treesitter highlighting (classic auto-attach is broken on nvim 0.12)",
            callback = function(args)
              if not vim.treesitter.highlighter.active[args.buf] then pcall(vim.treesitter.start, args.buf) end
            end,
          },
        },
      },
    },
  },
}
