
monsteR
=======

A list of D&D 5e monsters provided through SRD and some functions to fiddle with them.

Data is taken from the json file shared by [u/droiddruid](https://www.reddit.com/r/dndnext/comments/43a09o/srd_monsters_in_json_format/) combined with the human readable files from [open5e](https://github.com/eepMoody/open5e).

Installation
------------

``` r
devtools::install_github('oganm/monsteR')
```

Usage
-----

The package includes a list that include all SRD monsters called `monsters`. Each element of this list is a list that holds monster attributes

``` r
ls(monsters$Acolyte)
```

    ##  [1] "abilityScores"    "AC"               "actions"         
    ##  [4] "alignment"        "CR"               "HP"              
    ##  [7] "HPdice"           "immunities"       "languages"       
    ## [10] "name"             "resistances"      "saves"           
    ## [13] "senses"           "size"             "specialAbilities"
    ## [16] "speed"            "spellcasting"     "subtype"         
    ## [19] "text"             "type"             "vulnerabilities"

``` r
monsters$Acolyte$actions
```

    ## $Club
    ## $Club$name
    ## [1] "Club"
    ## 
    ## $Club$desc
    ## [1] "Melee Weapon Attack: +2 to hit, reach 5 ft., one target. Hit: 2 (1d4) bludgeoning damage."
    ## 
    ## $Club$attack_bonus
    ## [1] 2
    ## 
    ## $Club$damage_dice
    ## [1] "1d4"

Individual monsters have a special print function (`print.monster`). When a monster is called, the text taken from open5e is displayed

``` r
monsters$Acolyte
```

    ## 
    ## .. _srd:acolyte:
    ## 
    ## Acolyte
    ## -------
    ## 
    ## Medium humanoid (any race), any alignment
    ## 
    ## **Armor Class** 10
    ## 
    ## **Hit Points** 9 (2d8)
    ## 
    ## **Speed** 30 ft.
    ## 
    ## +-----------+-----------+-----------+-----------+-----------+-----------+
    ## | STR       | DEX       | CON       | INT       | WIS       | CHA       |
    ## +===========+===========+===========+===========+===========+===========+
    ## | 10 (+0)   | 10 (+0)   | 10 (+0)   | 10 (+0)   | 14 (+2)   | 11 (+0)   |
    ## +-----------+-----------+-----------+-----------+-----------+-----------+
    ## 
    ## **Skills** Medicine +4, Religion +2
    ## 
    ## **Senses** passive Perception 12
    ## 
    ## **Languages** any one language (usually Common)
    ## 
    ## **Challenge** 1/4 (50 XP)
    ## 
    ## **Spellcasting**: The acolyte is a 1st-level spellcaster. Its
    ## spellcasting ability is Wisdom (spell save DC 12, +4 to hit with spell
    ## attacks). The acolyte has following cleric spells prepared:
    ## 
    ##     **Cantrips (at will)**: :ref:`srd:light`, :ref:`srd:sacred-flame`, :ref:`srd:thaumaturgy`
    ## 
    ##     **1st level (3 slots)**: :ref:`srd:bless`, :ref:`srd:cure-wounds`, :ref:`srd:sanctuary`
    ## 
    ## Actions
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## 
    ## **Club**: *Melee Weapon Attack*: +2 to hit, reach 5 ft., one target.
    ## *Hit*: 2 (1d4) bludgeoning damage.
    ## 
    ## Acolytes are junior members of a clergy, usually answerable to a priest.
    ## They perform a variety of functions in a temple and are granted minor
    ## spellcasting power by their deities.

The package includes an `attack` function. This function looks at all available actions of a monster, detects if they have attack rolls and damage dice associated with them, and rolls the dice

``` r
attack(monsters$`Bandit Captain`)
```

    ## $Scimitar
    ## attack damage 
    ##     10      8 
    ## 
    ## $Dagger
    ## attack damage 
    ##      2      4

If the monster is a spellcaster it will have a `spellcasting` list which lists spell DC, spell attack bonus and spells. Note that spells are `spellList` objects from [wizaRd](https://github.com/oganm/wizaRd) package.

``` r
monsters$Acolyte$spellcasting
```

    ## $DC
    ## [1] 12
    ## 
    ## $attack_bonus
    ## [1] 4
    ## 
    ## $spells
    ## Cantrips
    ## ========
    ## Light
    ## Sacred Flame
    ## 
    ## Level 1
    ## =======
    ## Bless
    ## Cure Wounds
    ## Sanctuary

``` r
monsters$Acolyte$spellcasting$spells$`Cure Wounds`
```

    ## **1st-level evocation**
    ## 
    ## **Casting Time**: 1 action
    ## 
    ## **Range**: Touch
    ## 
    ## **Components**: V, S
    ## 
    ## **Duration**: Instantaneous
    ## 
    ## A creature you touch regains a number of hit points equal to 1d8 + your spellcasting ability modifier. This spell has no effect on undead or constructs.
    ## 
    ## **At Higher Levels.** When you cast this spell using a spell slot of 2nd level or higher, the healing increases by 1d8 for each slot level above 1st.
    ## 1d8 1d8
    ## [1] "Rolls: [ 7 ]"
    ## [1] "Rolls: [ 6 ]"
    ## 1d8 1d8 
    ##   7   6
