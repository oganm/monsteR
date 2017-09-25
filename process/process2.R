library(jsonlite)
library(dplyr)
library(purrr)
library(ogbox)
library(stringr)
library(wizaRd)

download.file('https://dl.dropboxusercontent.com/s/iwz112i0bxp2n4a/5e-SRD-Monsters.json',destfile = "data-raw/monsters.json")
system('svn checkout https://github.com/eepMoody/open5e/trunk/source/monsters')
system('mv monsters data-raw/monsters')

monsters = read_json('data-raw/monsters.json')
monsters = monsters[-length(monsters)]

names(monsters) = monsters %>% purrr::map_chr('name')

speeds = c('burrow','climb','fly','swim')


root = list.files('data-raw/monsters/',full.names = TRUE)
allFiles = list.files('data-raw/monsters/',full.names = TRUE,recursive = TRUE)
allFiles = allFiles[!allFiles %in% root & !grepl('index',allFiles)]

monsterText = allFiles %>% lapply(readLines) %>% lapply(paste,collapse='\n')
# just for quick naming
names(monsterText) = monsterText %>% sapply(function(x){
    x %>% str_split('\n') %>% {.[[1]][4]}
    # x %>% str_extract('^(?:[^\n]*\n){3}([^\n]*)') %>% str_extract('(?<=\n).*?$')
})

absentNames = names(monsterText)[!names(monsterText) %in% names(monsters)]
# names(monsters)[!names(monsters) %in% names(monsterText)]

monsters %<>% lapply(function(x){
    out = list()
    print(x$name)
    if(x$name %in% names(monsterText)){
        out$text = monsterText[[x$name]]
        out$name = x$name
    } else{
        newName = absentNames %>% strsplit(split = '\\s|,\\s')%>%
            sapply(function(y){
                all(y %in% (x$name %>% strsplit(split = '\\s|,\\s') %>% {.[[1]]}))
                }) %>%
                {absentNames[.]}
        if(length(newName)==0){
            out$name = x$name
            out$text = NULL
        } else{
            out$name = newName
            out$text = monsterText[[newName]]
        }
    }


    out$name = x$name
    out$size = x$size
    out$type = x$type
    out$subtype = x$subtype
    out$alignment = x$alignment
    out$AC = x$armor_class
    out$HP = x$hit_points
    out$HPdice = x$hit_dice
    speed = x$speed %>% str_split(', ') %>% {.[[1]]}
    speedType = speed %>% str_extract(regexMerge(speeds))
    speed %<>% str_extract('[0-9]+') %>% as.integer()
    names(speed) = speedType
    names(speed)[1] ='normal'
    out$speed = speed

    out$abilityScores = c(x$strength,
                          x$dexterity,
                          x$constitution,
                          x$intelligence,
                          x$wisdom,
                          x$intelligence)
    names(out$abilityScores) = c("Str", "Dex", "Con", "Int", "Wis", "Chr")


    out$saves = out$abilityScores %>% stat2mod()
    for(y in names(x)[grepl('save',names(x))]){
        out$saves[which.min(adist(names(out$saves) %>% tolower(),substr(y,1,3)) %>% as.vector())] = x[[y]]
    }


    out$vulnerabilities = x$damage_vulnerabilities
    out$resistances = x$damage_resistances
    out$immunities = x$damage_immunities
    out$conditionImmunities
    out$senses = x$senses
    out$languages = x$languages %>% strsplit(', ')
    out$CR = x$challenge_rating %>% teval

    out$specialAbilities = x$special_abilities
    if(!is.null(out$specialAbilities)){
        names(out$specialAbilities) = x$special_abilities %>% map_chr('name')
    }
    if('Spellcasting' %in% names(out$specialAbilities)){
        out$spellcasting = list()
        out$spellcasting$DC = out$specialAbilities$Spellcasting$desc %>% str_extract('(?<=DC )[0-9]+') %>% as.integer()
        out$spellcasting$attack_bonus =  out$specialAbilities$Spellcasting$desc %>% str_extract('(\\+|-)[0-9]+(?= to hit)') %>% as.integer()
        spellNames =wizaRd::spells %>% names %>% tolower()

        out$spellcasting$spells =  wizaRd::spells[spellNames %in% (out$specialAbilities$Spellcasting$desc %>% strsplit('(, )|(: )') %>% {.[[1]]})]

    }


    out$actions = x$actions
    if(!is.null(out$actions)){
        names(out$actions) = x$actions %>% map_chr('name')
    }

    out$legendaryActions= x$legendary_actions
    if(!is.null(out$legendaryActions)){
        names(out$legendaryActions) = x$legendary_actions %>% map_chr('name')
    }


    class(out) = append(class(out), 'monster')
    return(out)

})
class(monsters) = append(class(monsters),'monsterList')
use_data(monsters,overwrite = TRUE)
