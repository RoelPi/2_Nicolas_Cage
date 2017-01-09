getTables <- function(bankruptActor) {
    url <- bankruptActor[[1]]
    bankrupt <- bankruptActor[[2]]
    
    page <- read_html(url)
    vMovies <- page %>% html_node(".filmo-category-section") %>% html_nodes(".filmo-row b a") %>% html_text()
    
    # IDs
    vIDs <- page %>% html_node(".filmo-category-section") %>% html_nodes(".filmo-row b a") %>% html_attrs() %>% unlist() %>% as.character()
    vIDs <- gsub("/\\?ref_\\=nm_flmg_act_.*","",vIDs)
    vIDs <- gsub("/title/","",vIDs)
    
    # Years
    vYears <- page %>% html_node(".filmo-category-section") %>% html_nodes(".filmo-row .year_column") %>% html_text()
    vYears <- gsub("\n","",vYears) 
    vYears <- gsub("/I","",vYears)
    vYears <- trimws(vYears)
    vYears <- substr(vYears,2,5)
    vYears <- as.numeric(vYears)
    
    # Connect to OMDB API
    urlRating <- paste0("http://www.omdbapi.com/?i=",as.character(vIDs))
    if (!exists("listDetails")) { listDetails <- lapply(urlRating, function(x) fromJSON(x)) }
    
    # Rating
    vRating <- sapply(listDetails, function(x) as.numeric(x$imdbRating))
    
    # Votes
    vVotes <- sapply(listDetails, function(x) as.numeric(gsub(",","",x$imdbVotes)))
    
    # Runtime
    vRuntime <- sapply(listDetails, function(x) as.numeric(gsub(" min","",x$Runtime)))
    
    # Metascore
    vMeta <- sapply(listDetails, function(x) as.numeric(x$Metascore))
    
    # Bankrupt dummy
    vBankrupt <- ifelse(vYears > bankrupt,TRUE,FALSE)
    
    # Cohort n
    vCohortYear <- vYears - bankrupt
    
    # Make Movie-level Data Table
    dt <- data.table(Title=vMovies, Year=vYears,Runtime=vRuntime,Rating=vRating,Votes=vVotes,Metascore=vMeta,Bankrupt=vBankrupt,CohortYear=vCohortYear)
    
    # Only movies
    dt <- subset(dt,Runtime>60)
    
    # Make Year-level Data Table
    dtYear <- dt[,.(Runtime=mean(Runtime),Rating=mean(Rating),Votes=mean(Votes),Metascore=mean(Metascore),Bankrupt=head(Bankrupt,1),CohortYear=head(CohortYear,1)),by=Year]
    
    dataset <- list(dt,dtYear)
    dataset
}