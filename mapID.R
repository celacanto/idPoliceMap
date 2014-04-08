library('XML')
library('countrycode')
library('rworldmap')

aroundTheWorldIdWiki <- "http://en.wikipedia.org/wiki/List_of_national_identity_card_policies_by_country"
idsTables <- readHTMLTable(aroundTheWorldIdWiki, which=c(1,3,4), stringsAsFactors = FALSE)
names(idsTables) <- c("Compulsory identity cards", "Non-compulsory identity cards", "No identity cards")

countrys <- gsub("^\\W+|\\W+$", "", unlist(lapply(idsTables, function(x) x[,1])))
countrysCode <- countrycode(countrys, "country.name",  "iso3c")
nTypeId <- sapply(idsTables, nrow)
idsTypes <-unlist(mapply(rep, names(nTypeId), nTypeId))

worldIdDf <- data.frame("ISO3V10" = countrysCode, 'Country' = countrys, "id_policies" = idsTypes, stringsAsFactors = FALSE )
rownames(worldIdDf) <- NULL


file <- paste0("IDAroudTheWorld.png")
png(filename = file, width = 1200/58, height = 480/58, pointsize = 20, units="in", res = 120)




par(mai = c(0, 0, 0, 0), 
    xaxs = "i", 
    yaxs = "i", 
    mar = c(0,0,0,0),
    family = "NewCenturySchoolbook")
sPDF <- joinCountryData2Map(dF = worldIdDf, 
                            joinCode = "ISO3", 
                            nameJoinColumn = "ISO3V10",
                            mapResolution = 'high')
sPDF <- sPDF[-which(row.names(sPDF) == 'Antarctica'),]
idColour <- c("#7E827A", "#A8676B", "#2C3E50")
col_noData <- "#D7DADB"
mapParams <- mapCountryData(sPDF, mapTitle= "", nameColumnToPlot = "id_policies", borderCol = "#2F343B", lwd = 0.5,
                            addLegend = FALSE,
                            catMethod = "categorical",
                            oceanCol = "#8BB4D9", missingCountryCol = col_noData,
               colourPalette= idColour)

legend(x=-170, y=0, cex = 0.9,legend=c("Compulsory identity cards", "No identity cards", "Non-compulsory identity cards",
                             "No Data"), fill=c(idColour, col_noData), bg = rgb(1,1,1,0.3))
text(x = 180, y = -57, labels = "Source: http://en.wikipedia.org/wiki/List_of_national_identity_card_policies_by_country", pos = 2,
     cex = 0.9, font = 3)

dev.off()
