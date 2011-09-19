" This is a Haskell configuration for the Vim Tagbar plugin that uses
" lushtags.
"
" Tagbar can be found at:
" http://www.vim.org/scripts/script.php?script_id=3465
" http://majutsushi.github.com/tagbar/
"
" Paste this in to your vimrc file
" OR copy this file into your .vim/plugin directory
" OR load it from your vimrc file by adding a line like:
"
" source /path/to/tagbar-haskell.vim

if executable('lushtags')
    let g:tagbar_type_haskell = {
        \ 'ctagsbin' : 'lushtags',
        \ 'ctagsargs' : '--ignore-parse-error --',
        \ 'kinds' : [
            \ 'm:module:0',
            \ 'e:exports:1',
            \ 'i:imports:1',
            \ 't:declarations:0',
            \ 'd:declarations:1',
            \ 'n:declarations:1',
            \ 'f:functions:0',
            \ 'c:constructors:0'
        \ ],
        \ 'sro' : '.',
        \ 'kind2scope' : {
            \ 'd' : 'data',
            \ 'n' : 'newtype',
            \ 'c' : 'constructor',
            \ 't' : 'type'
        \ },
        \ 'scope2kind' : {
            \ 'data' : 'd',
            \ 'newtype' : 'n',
            \ 'constructor' : 'c',
            \ 'type' : 't'
        \ }
    \ }
endif
