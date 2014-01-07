load.libraries <- function(needed.packs,quietly=T){
  installed.packs <- installed.packages()

  for(pack in needed.packs){
    if(!pack %in% installed.packs){
      install.packages(pack,repos='http://cran.cnr.Berkeley.edu')
    }
    if(quietly){
      suppressPackageStartupMessages(library(pack,character.only=T,quietly=quietly))
    }else{
      library(pack,character.only=T)
    }
  }
}
