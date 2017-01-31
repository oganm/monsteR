library(dplyr)
library(ogbox)
library(stringr)
library(assertthat)

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
    tryCatch({
        monster = list()
        monster$text = text
        monster$name = text[4]
        monster$size = str_extract(text[7],'^([^\\s]+)\\s') %>% str_trim()
        list[monster$type,monster$alignment] = text[7] %>% str_split(pattern = ',') %>%
        {out = .[[1]]
        out[1] = str_replace(out[1],'^([^\\s]+)\\s','')
        out}
        monster$armor$AC = text[grep(pattern = 'Armor Class',text)] %>% str_extract('[0-9]+') %>% as.numeric
        assert_that(noNA(monster$armor$AC))


        monster$armor$armor = text[grep(pattern = 'Armor Class',text)] %>% str_extract('(\\s|[A-Za-z])*?(?=\\))')

        monster$HP$average = text[grep(pattern = 'Hit Points',text)] %>% str_extract('[0-9]+') %>% as.numeric
        assert_that(noNA(monster$HP$average))

        monster$HP$roll =  text[grep(pattern = 'Hit Points',text)] %>%  str_extract('(?<=\\().*(?=\\))')

        speedStrings = c('\\*','fly','burrow','climb','swim')

        monster$speed =
            speedStrings %>% lapply(function(x){
                text[grep(pattern = 'Speed',text)] %>% str_extract(paste0("(?<=",x,'\\s)[0-9]*')) %>% as.numeric
            })
        names(monster$speed) = c('normal','fly','burrow','climb','swim')

        monster$stats = text[grep(pattern = '\\| STR',text)+2] [2] %>% str_extract_all('(?<=\\|\\s)[0-9]*') %>% {.[[1]]} %>% as.numeric %>%
        {names(.) = c('STR','DEX','CON','INT','WIS','CHA')
        .} %>% as.list

        monster$languages = text[grep(pattern = 'Languages',text)] %>% str_replace('\\*\\*Languages\\*\\*\\s','') %>%
            strsplit(',') %>% {.[[1]]}



        class(monster) = append(class(monster), 'monster')
        return(monster)
    },error = function(e){
        monster = list()
        monster$text = text
        class(monster) = append(class(monster), 'monsterNoParse')
        return(monster)
    })
}

monsters = monsterText %>% lapply(monsterParse)
monsters[(monsters %>% lapply(class) %>% purrr::map(2)  %>% unlist) %in% 'monsterNoParse']
