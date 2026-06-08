-- Override AstroNvim v5's aerial pin so it works on Neovim 0.12.
--
-- AstroNvim v5 pins aerial.nvim 2.7.0, which predates Neovim 0.12's
-- `Query:iter_matches()` change (it now returns a list of nodes per capture).
-- On 0.12 the pinned version errors when opening a file
-- ("attempt to call method 'start' (a nil value)"). Latest aerial handles the
-- new API, so we let it track its branch instead of AstroNvim's pin.
-- This is the one plugin override the v5 + Neovim 0.12 combo needs.

---@type LazySpec
return {
  "stevearc/aerial.nvim",
  version = false, -- ignore AstroNvim's version pin
  commit = false, -- ignore AstroNvim's commit pin
  pin = false, -- let `:Lazy update` move it
}
