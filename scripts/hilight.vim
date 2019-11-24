" It is convenient to have vim highlight implemented sequences

hi Implemented cterm=undercurl ctermfg=219
syn region  TyLit start='@\@<=\d' end='[^\d]\@=' contains=Implemented

command Hiupd exec 'syn keyword Implemented contained '.readfile('data/impl')[0]
Hiupd
