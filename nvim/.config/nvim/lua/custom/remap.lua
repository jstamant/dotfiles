-- TODO move screen by lines

-- TODO change <C-g> to an alias for escape, or set <C-c>, since it does a similar thing

-- Move lines around in visual mode, and indents properly
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

vim.keymap.set("n", "J", "mzJ`z") -- join lines, but keep cursor where it is
-- Scroll up/down, but keep cursor in center of screen
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
-- Search terms stay in the middle
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- Paste over visual selections without overwritting the clipboard
-- vim.keymap.set("x", "<leader>p", [["_dP]])

-- next greatest remap ever : asbjornHaland
-- Copy into system clipboard, so you keep vim and system clipboards separate
vim.keymap.set("n", "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])
vim.keymap.set("v", "<leader>y", [["+y]])

--vim.keymap.set({"n", "v"}, "<leader>d", [["_d]])

-- Leave insert mode with C-c
vim.keymap.set("i", "<C-c>", "<Esc>")

-- Adding this one will prevent you from running that last recorded macro?
--vim.keymap.set("n", "Q", "<nop>")
-- This is for Prime's tmux integration
--vim.keymap.set("n", "<C-f>", "<cmd>silent !tmux neww tmux-sessionizer<CR>")
--vim.keymap.set("n", "<leader>f", vim.lsp.buf.format)

-- For navigating errors in a file
--vim.keymap.set("n", "<C-k>", "<cmd>cnext<CR>zz")
--vim.keymap.set("n", "<C-j>", "<cmd>cprev<CR>zz")
--vim.keymap.set("n", "<leader>k", "<cmd>lnext<CR>zz")
--vim.keymap.set("n", "<leader>j", "<cmd>lprev<CR>zz")

-- Starts a replace/substitution on the word you were on
--vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])
-- Adds executable permission to the current file. Good for bash scripts
--vim.keymap.set("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })
