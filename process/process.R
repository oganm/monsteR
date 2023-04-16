# libraries and variables ----------
library(jsonlite)
library(dplyr)
library(purrr)
library(ogbox)
library(stringr)
library(wizaRd)
library(diceSyntax)
library(snakecase)

devtools::load_all()
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
other = c('unaligned','any alignment','neutral','any [a-z\\-]* alignment')

speeds = c('burrow','climb','fly','swim')

skillNames = c('Athletics',
               'Acrobatics',
               'Sleight of Hand',
               'Stealth',
               'Arcana',
               'History',
               'Investigation',
               'Nature',
               'Religion',
               'Animal Handling',
               'Insight',
               'Medicine',
               'Perception',
               'Survival',
               'Deception',
               'Intimidation',
               'Performance',
               'Persuasion')

skillAttributes = c('Str',
                    rep('Dex',3),
                    rep('Int',5),
                    rep('Wis',5),
                    rep('Cha',4))

# read files ------------
# download.file('https://dl.dropboxusercontent.com/s/iwz112i0bxp2n4a/5e-SRD-Monsters.json',destfile = "data-raw/monsters.json")
# system('svn checkout https://github.com/eepMoody/open5e/trunk/source/monsters')
# system('mv monsters data-raw/monsters')

monsters = read_json('data-raw/monsters.json')
monsters = monsters[-length(monsters)]

names(monsters) = monsters %>% purrr::map_chr('name')


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
        inMD = TRUE
    } else{
        newName = absentNames %>% strsplit(split = '\\s|,\\s')%>%
            sapply(function(y){
                all(y %in% (x$name %>% strsplit(split = '\\s|,\\s') %>% {.[[1]]}))
                }) %>%
                {absentNames[.]}
        if(length(newName)==0){
            out$name = x$name
            out$text = NULL
            inMD = FALSE
        } else{
            out$name = newName
            out$text = monsterText[[newName]]
            inMD = TRUE
        }
    }
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
                          x$charisma)
    names(out$abilityScores) = c("Str", "Dex", "Con", "Int", "Wis", "Cha")


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

    # if in MD, overwrite bits of it -------------
    # some bits of the JSON file is wrong/missing. Get the easily parsed bits from MD file

    if(inMD){
        text = monsterText[[out$name]]
        typeSizeText = text %>% str_extract('(?<=-\\n).*(?=.*?Armor Class\\*\\*)' %>% regex(dotall=TRUE))

        out$size = typeSizeText %>%
            str_extract_all(regexMerge(sizes) %>% regex(dotall=TRUE)) %>%
            {.[[1]]}

        out$type = typeSizeText %>% str_extract_all(regexMerge(types))%>% {.[[1]]}

        out$subtype = text %>%
            str_extract_all(paste0(regexMerge(types),'.*?(?=Armor Class\\*\\*)') %>%
                                regex(dotall=TRUE)) %>%
                                {.[[1]]} %>% str_extract(paste0(
                                    '(?<=',regexMerge(types),'\\s\\()',
                                    '.*?(?=\\))'
                                )) %>% str_split(', ',simplify = TRUE) %>% as.character()
        if(length(out$subtype)==0){
            out$subtype = ''
        }

        out$alignment = text %>% str_extract_all(paste0('(?<=, )','(',paste0(regexMerge(order),' ',regexMerge(nice)),')|(',
                                                            regexMerge(other),')',
                                                            '(?=.*?Armor Class\\*\\*)') %>%
                                                         regex(dotall=TRUE)) %>%
                                                         {.[[1]]}

        out$AC = text %>% str_extract_all(paste0("(?<=\\*\\*Armor Class\\*\\*).*(?=\\*\\*Hit Points)") %>% regex(dotall=TRUE)) %>%
        {.[[1]]} %>% str_extract_all('\\d+') %>% {.[[1]]} %>% as.integer()

        hpText = text %>% str_extract_all(paste0("(?<=\\*\\*Hit Points\\*\\*).*(?=\\*\\*Speed)") %>% regex(dotall=TRUE)) %>% {.[[1]]}

        out$HPdice = hpText %>% str_extract_all('(?<=\\()[0-9]+?d[0-9]+.*(?=\\))') %>% {.[[1]]}
        out$HP = hpText %>% str_extract_all('\\d+?(?= \\()') %>% {.[[1]]} %>% as.integer()
    } else{
        # HP dice is always wrong. fix it regardless
        parsedDice = out$HPdice %>% diceParser()
        out$HPdice = paste(out$HPdice,'+',ceiling(out$HP - parsedDice$diceCount*(parsedDice$diceSide/2+.5)))
        if (out$subtype == ''){
            out$subtype = NA
        }
    }

    out$skills = rep(0, length(skillNames))
    out$skills = stat2mod(out$abilityScores)[skillAttributes]
    names(out$skills) = skillNames


    out$skills[to_title_case(names(x)[names(x) %in% tolower(skillNames)])] =
        x[names(x)[names(x) %in% tolower(skillNames)]] %>% unlist %>% unname


    class(out) = append(class(out), 'monster')
    return(out)

})
class(monsters) = append(class(monsters),'monsterList')

cat(jsonlite::toJSON(monsters),file = 'data-raw/processed.json')
use_data(monsters,overwrite = TRUE)
