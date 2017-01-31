#' @export
stat2mod = function(x){
    floor(x/2-5)
}

#' @export
print.monster = function(x){
    cat(monster$text,sep='\n')
}
