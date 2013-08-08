load.libraries <-
function(needed.packs){
  installed.packs <- installed.packages()

  for(pack in needed.packs){
    if(length(grep(pack,installed.packs))==0){
      install.packages(pack)
    }
    library(pack,character.only=T)
  }
}
