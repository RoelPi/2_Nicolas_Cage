getActor <- function(link,year,filename,nameOfActor) {
    actor <- list(link,year)
    if (!file.exists(paste0(filename,".Rda"))) { 
        actorDT <- getTables(actor,nameOfActor) 
        saveRDS(actorDT,paste0(filename,".Rda"))
        actorDT
    } else { 
        readRDS(paste0(filename,".Rda"))
    }
} 