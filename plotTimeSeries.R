plotTimeSeries <- function(bankruptActorDT = nicolasCageDT,metric='Rating',titleOfPlot = "Evolution of Nicolas Cage Movies") {
    dtYear <- bankruptActorDT[[2]]
    p <- plot_ly(dtYear,x=~dtYear$CohortYear,y=~dtYear[[metric]],name="Rating",type="scatter",mode="lines") %>% 
        layout(title=titleOfPlot,xaxis=list(title="Year"),yaxis=list(title="IMDb rating",rangemode='tozero'))
    p
}