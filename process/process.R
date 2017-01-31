library(dplyr)
library(ogbox)
library(stringr)

system('svn checkout https://github.com/eepMoody/open5e/trunk/source/monsters')
system('mv monsters data-raw/monsters')

root = list.files('data-raw/monsters/',full.names = TRUE)
allFiles = list.files('data-raw/monsters/',full.names = TRUE,recursive = TRUE)
allFiles = allFiles[!allFiles %in% root & !grepl('index',allFiles)]

monsterText = allFiles %>% lapply(readLines)
# just for quick naming
names(monsterText) = monsterText %>% sapply(function(x){
    x[4]
})

monsterParse = function(text){
    monster = list()
    monster$name = text[4]
    monster$size = str_extract(text[7],'^([^\\s]+)\\s') %>% str_trim()
    list[monster$type,monster$alignment] = text[7] %>% str_split(pattern = ',') %>%
    {out = .[[1]]
    out[1] = str_replace(out[1],'^([^\\s]+)\\s','')
    out}
    monster$armor$AC = text[9] %>% str_extract('[0-9]+')
    monster$armor$armor = text[9] %>% str_extract('(\\s|[A-Za-z])*?(?=\\))')

    monster$HP$average = text[11] %>% str_extract('[0-9]+')
    monster$HP$roll = text[11] %>%  str_extract('(?<=\\().*(?=\\))')

    special = c('fly','burrow','climb','swim')

    monster$speed$walk =text[13] %>% str_extract()

}
