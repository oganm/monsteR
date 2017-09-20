library(dplyr)
library(ogbox)
library(stringr)
library(assertthat)

sizes = c('Tiny',
          'Small',
          'Medium',
          'Large',
          'Huge',
          'Gargantuan')

types=c('aberration',
        'beast',
        'celestial',
        'construct',
        'dragon',
        'elemental',
        'fey',
        'fiend',
        'giant',
        'humanoid',
        'monstrosity')
order = c('chaotic','neutral','lawful')
nice = c('good','neutral','evil')
other = c('unaligned','any','neutral')

system('svn checkout https://github.com/eepMoody/open5e/trunk/source/monsters')
system('mv monsters data-raw/monsters')

download.file('https://dl.dropboxusercontent.com/s/iwz112i0bxp2n4a/5e-SRD-Monsters.json')

sizes = c('Tiny',
          'Small',
          'Medium',
          'Large',
          'Huge',
          'Gargantuan')

types=c('aberration',
        'undead',
        'ooze',
        'beast',
        'plant',
        'celestial',
        'construct',
        'dragon',
        'elemental',
        'fey',
        'fiend',
        'giant',
        'humanoid',
        'monstrosity')
order = c('chaotic','neutral','lawful','true')
nice = c('good','neutral','evil')
other = c('unaligned','any')


root = list.files('data-raw/monsters/',full.names = TRUE)
allFiles = list.files('data-raw/monsters/',full.names = TRUE,recursive = TRUE)
allFiles = allFiles[!allFiles %in% root & !grepl('index',allFiles)]

monsterText = allFiles %>% lapply(readLines) %>% lapply(paste,collapse='\n')
# just for quick naming
names(monsterText) = monsterText %>% sapply(function(x){
    x %>% str_split('\n') %>% {.[[1]][4]}
    # x %>% str_extract('^(?:[^\n]*\n){3}([^\n]*)') %>% str_extract('(?<=\n).*?$')
})



monsterParse = function(text){
    tryCatch({

    monster = list()
    monster$text = text
    monster$name = text %>% str_split('\n') %>% {.[[1]][4]}
    print(monster$name)
    monster$size = text %>%
        str_extract_all(paste0(regexMerge(sizes),'(?=.*?Armor Class\\*\\*)') %>% regex(dotall=TRUE)) %>%
        {.[[1]]}

    monster$type = text %>%
        str_extract_all(paste0(regexMerge(types),'(?=.*?Armor Class\\*\\*)') %>%
                            regex(dotall=TRUE)) %>%
                            {.[[1]]}

    monster$subtype = text %>%
        str_extract_all(paste0(regexMerge(types),'.*?(?=Armor Class\\*\\*)') %>%
                            regex(dotall=TRUE)) %>%
                            {.[[1]]} %>% str_extract(paste0(
                                '(?<=',regexMerge(types),'\\s\\()',
                                '.*?(?=\\))'
                            )) %>% str_split(', ',simplify = TRUE) %>% as.character()
    if(length(monster$subtype)==0){
        monster$subtype = NULL
    }

    monster$alignment = text %>% str_extract_all(paste0('(?<=,)',regexMerge(),
                                                        '(?=.*?Armor Class\\*\\*)') %>%
                                                     regex(dotall=TRUE)) %>%
                                                     {.[[1]]}


    class(monster) = append(class(monster), 'monster')
    return(monster)
    },
    error = function(e){
        monster = list()
        monster$name = text %>% str_split('\n') %>% {.[[1]][4]}
        monster$text = text
        class(monster) = append(class(monster), 'monsterNoParse')
        return(monster)
    })

}

monsters = monsterText %>% lapply(monsterParse)
monsters %>% map('subtype')
monsters[(monsters %>% lapply(class) %>% purrr::map(2)  %>% unlist) %in% 'monsterNoParse']

monsters %>% map('size')


monsterParse = function(text){
    tryCatch({
        monster = list()
        textBackup = text
        text = textBackup
        monster$text = text
        monster$name = text[4]
        # name is extracted top is not needed
        text = text[-(1:6)]
        # remove empty lines and possible lines for figures
        # text = text[!grepl('^$',text)]
        text = text[!grepl(ogbox::regexMerge(c('figure::',
                                               ':figclass:',
                                               ':target:',
                                               '©',
                                               'rst-class')),text)]



        # size and type infor is now the first line, take it
        monster$size = str_extract_all(text[1],regexMerge(sizes)) %>% {.[[1]]}
        text[1] = str_replace_all(text[1],regexMerge(sizes),'')
        monster$type = text[1] %>% str_extract_all(regexMerge(types)) %>% {.[[1]]}
        monster$subtype = text[1]%>% str_extract_all('(?<=\\()[a-zA-Z\\s]*?(?=\\))') %>% {.[[1]]}
        text[1] = str_replace_all(text[1],'(?<=\\()[a-zA-Z\\s]*?(?=\\))','')

        monster$alignment = text[1] %>%str_extract_all(
            paste0('(',regexMerge(order),'\\s',regexMerge(nice),')|(',
                   regexMerge(other),')')) %>% {.[[1]]}
            #str_extract('(?<!,)\\s*([^,]+)$') %>% str_trim()
        text = text[-(1:2)]

        # function to get data from an entire section
        sectionLines = function(pattern){
            if(pattern == 'statblock'){
                return((grep(pattern = '\\| STR',text)-1):(grep(pattern = '\\| STR',text)+3))
            } else if(pattern =='Actions'){
                initial = grep(pattern = '^Actions',text)+3
                end = grep('^Legendary Actions',text)-1
                if(length(end)==0){
                    end = grep('^$',text)[grep('^$',text)>initial]
                    end = end[!(end+1) %in% grep('^\\*\\*',text)] - 1
                    if(length(end)==0){
                        end = length(text)
                    }
                }
                return(initial:end)
            } else if(pattern = '^Legendary Actions'){
                initial = grep(pattern = 'Legendary Actions',text)+3
                end = grep('^$',text)[grep('^$',text)>initial]
                end = end[!(end+1) %in% grep('^\\*\\*',text)] - 1
                if(length(end)==0){
                    end = length(text)
                }
                return(initial:end)
            } else{
                initial = grep(pattern = pattern,text)
                textTemp = text[(initial+1):length(text)]
                end = grep(pattern = ogbox::regexMerge(c('^$','^\\*\\*','^\\+','Actions','Legendary Actions')),textTemp)[1]
                initial:(initial+end-1)
            }
        }

        ACtext = text[sectionLines('*Armor Class')] %>% paste(collapse=' ')
        monster$armor$AC = ACtext %>% str_extract_all('[0-9]+') %>% {.[[1]]} %>% as.numeric
        assert_that(noNA(monster$armor$AC))

        monster$armor$AC = text[grep(pattern = 'Armor Class',text)] %>% str_extract_all('[0-9]+') %>% {.[[1]]} %>% as.numeric
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

        monster$stats = text[grep(pattern = '\\| STR',text)+2] %>% str_extract_all('(?<=\\|\\s)[0-9]*') %>% {.[[1]]} %>% as.numeric %>%
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
monsterParse2 = function(text){
    tryCatch({
        monster = list()
        monster$text = text
        monster$name = text[4]


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
