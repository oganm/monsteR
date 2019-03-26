#' @export
print.monster = function(x){
    if(!is.null(x$text)){
        cat(x$text,sep='\n')
    } else{
        cat(x$name)
    }
}

#' @export
"[.monster" = function(x,i){
    class(x) = 'list'
    x = x[i]
    class(x) = append(class(x),'monsterList')
    return(x)
}

#' @export
"c.monster" = function(x,...){
    class(x) = 'list'
    x = c(x,...)
    class(x) = append(class(x),'monsterList')
    return(x)
}

#'@export
cMonster = function(...){
    x = list(...)
    names(x) = x %>% purrr::map_chr('name')
    class(x) = append(class(x),'monsterList')
    return(x)
}

#'@export
print.monsterList = function(monsters){
    if(length(monsters)==0){
        return(invisible(NULL))
    } else{
        cat(names(monsters),sep = '\n')
    }

}


#'@export
attack = function(monster,attacks = 'all', vocal = FALSE){
    if(attacks == 'all'){
        attacks = names(monster$actions)
    } else{
        attacks = names(monster$actions)[names(monster$actions) %in% attacks]
    }
    attacks = attacks[attacks %in% attackable(monster$actions)]

    monster$actions[attacks] %>% lapply(singleAttack,vocal= vocal)

}


swarmAttack = function(monster, attacks = 'all', count, AC, advantage = c('N','D','A'), vocal = FALSE){
    advantage = match.arg(advantage)
    if(attacks == 'all'){
        attacks = names(monster$actions)
    } else{
        attacks = names(monster$actions)[names(monster$actions) %in% attacks]
    }
    attacks = attacks[attacks %in% attackable(monster$actions)]

    monster$actions[attacks] %>% lapply(function(x){
        diceSyntax::swarm(AC = AC,
                          count = count,
                          damageDice = x$damage_dice,
                          attackBonus = x$attack_bonus,
                          damageBonus = x$damage_bonus,
                          advantage = advantage) %>% sum
    })
}

singleAttack = function(action,vocal = FALSE){
    if(is.null(action$attack_bonus)){
        action$attack_bonus = 0
    }
    if(is.null(action$damage_bonus)){
        action$damage_bonus = 0
    }

    roll = diceSyntax::roll('1d20',vocal = vocal)
    attack = roll +  action$attack_bonus
    damage = diceSyntax::roll(action$damage_dice,vocal = vocal) + action$damage_bonus
    if(roll == 20){
        damage = damage + diceSyntax::roll(action$damage_dice,vocal = vocal)
    } else if (roll == 1){
        damage = 0
    }

    return(c(attack = attack,
             damage = damage))
}

attackable = function(actions){
    actions %>% sapply(function(x){
        !is.null(x$damage_dice)
    }) %>% {names(actions)[.]}
}

