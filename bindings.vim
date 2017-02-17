" If you have neovim with neoterm installed (and stack-run from cabal/stack)

function! GetRightCommand(key)
    call neoterm#kill()

    let l:folder = getcwd()

    if l:folder ==# '/home/andesil/TextToNumber'
        if a:key ==# 'b'
            silent call neoterm#do('clear; stack build')
        endif
        if a:key ==# 'e'
            silent call neoterm#do('clear; stack run')
        endif
        if a:key ==# 't'
            silent call neoterm#do('clear; stack test')
        endif
endfunction

" nnoremap [Space]b :call neoterm#kill()<CR>:T clear; stack build<CR>
nnoremap [Space]b :silent call GetRightCommand('b')<CR>
nnoremap [Space]e :silent call GetRightCommand('e')<CR>
nnoremap [Space]t :silent call GetRightCommand('t')<CR>
nnoremap [Space]c :call neoterm#kill()<CR>
nnoremap [Space]r :InteroReload<CR>
