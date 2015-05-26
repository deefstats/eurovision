library(maps)
library(maptools)
library(geosphere)
library(Cairo)
library(plyr)
library(ggplot2)
library(ggmap)
library(reshape)
library(grid)

# Define our visualisation parameters

# Draw scores greater than and equal to DrawAllor12
DrawAllor12 = 1

# Actually draw the map? Used for testing world drawing
DrawTheLines = 1
DrawThePies = 0

# How many points between the start and finish?
numberOfPoints = 60

# Background Colours
colourBackground = "#404040"

# Country Colours
colourCountry = "#757575"
colourCountryBorders = colourCountry

# Start line colour
colourLow = "#505050"

# End line colours
colourHigh1 = "#00FF00"
colourHigh2 = "#FFFF00"

widthOutputBase = 0.55

# Zoom on Europe
mapxLeft = -38
mapxRight = 100
mapyBottom = 26
mapyTop = 68
windowScale = 0.05

# Load data file
voteData <- read.csv("data/2015clean.csv",header=TRUE,sep=",")
# Column order: Vote.From, Type, country...

# Load country data we have
countryData <- read.csv("data/CountryData.csv",header=TRUE,sep=",")
countryData <- within(countryData, lat[Country=="Russia"] <- (mapyTop - 2))
countryData <- within(countryData, lon[Country=="Russia"] <- (mapxRight - 2))
countryData <- within(countryData, lat[Country=="Australia"] <- mapyBottom + 2)
countryData <- within(countryData, lon[Country=="Australia"] <- (mapxRight - 2))

# Melt the data to make a better table with Vote To column
voteTable <- melt(voteData, id=c("Vote.From","Type"))
voteTable <- rename(voteTable, c("variable"="Vote.To"))
voteTable$Vote.To <- gsub("\\."," ",voteTable$Vote.To)

#voteTable$Vote.To <- replace(voteTable$voteTo,

# Sort the data
# Need to sort it by biggest total vote score countries in the country To column, or it draws lines into
# biggest countries last, with highest vote scores last within the country
voteTable = voteTable[order(voteTable$value),]

# Remove NA rows
voteTable <- na.omit(voteTable)
if (DrawAllor12 < 12) {
	voteTable <- subset(voteTable, value >= DrawAllor12)
}

# Now we have a nice dataset
# Column order: Vote.From, Type, Vote.To, value

# Let's get our list of countries
countryList = unique(c(as.character(voteData$Vote.From),as.character(voteData$Vote.To)))
countryList <- as.data.frame(countryList)
countryList <- cbind(countryList,lat=0)
countryList <- cbind(countryList,lon=0)
countryList <- rename(countryList, c("countryList"="Country"))

# Load map data
data(wrld_simpl)
# Prepare map for drawing
w <- fortify(wrld_simpl)
names(w) <- list("long","lat","order","hole","piece","group", "region")

# Check unique country
loadCountryFromGoogle = 0
if (loadCountryFromGoogle > 0) {
	checkColumn <- countryList
	for (i in 1:nrow(checkColumn)) {
		checkCountry = as.character(checkColumn[i,"Country"])
		cat(paste("Googling geodata for",checkCountry,"\n"))
		fromgeo <- geocode(checkCountry)
		countryList[i,"lat"] = as.double(fromgeo["lat"])
		countryList[i,"lon"] = as.double(fromgeo["lon"])
	}
	write.csv(countryList ,
		file="data/CountryData.csv" ,
		quote=FALSE ,
		row.names=FALSE)
} else {
	countryList <- countryData
}


windowWidth = (mapxRight - mapxLeft) * windowScale
windowHeight = (mapyTop - mapyBottom) * windowScale * 2

CairoWin(width = windowWidth, height = windowHeight)
cat(paste0("Width=", windowWidth, ", Height=", windowHeight," (Ratio of ",ceiling(windowWidth/windowHeight),")\n"))

p <- ggplot()
#p <- p + theme_nothing(legend = FALSE)
p <- p + theme(panel.background = element_rect(fill=colourBackground))
p <- p + theme(line = element_line(color="#505050",linetype="dotted"))
p <- p + theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())
p <- p + theme(panel.grid.minor.y=element_blank(),panel.grid.major.y=element_blank())
p <- p + theme(axis.line=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
p <- p + theme(axis.text.x=element_blank(),axis.text.y=element_blank())
p <- p + theme(axis.ticks=element_blank())
p <- p + theme(legend.position = "none")
p <- p + coord_map()
p <- p + coord_cartesian(xlim=c(mapxLeft, mapxRight),ylim=c(mapyBottom, mapyTop))
p <- p + geom_polygon(aes(long,lat,group=group),data=w,fill=colourCountry,color=colourCountryBorders,size=0.2)
#p <- p + scale_colour_gradient(low="#4040AA",high="#00FF00",guide = FALSE)

coloursA <- c(colourLow,colourHigh1,colourLow,colourHigh2)
valuesA <- c(0,0.5,0.50001,1)
#p <- p + scale_colour_gradient(low="#AA4040", high="#FFFF00", guide = FALSE)
p <- p + scale_colour_gradientn(colours=coloursA, values=valuesA, guide=FALSE)

# Let's draw
if (DrawTheLines == 1 || DrawThePies == 1) {
	for (i in 1:nrow(voteTable)) {
		voteFrom = as.character(voteTable[i,"Vote.From"])
		voteTo = as.character(voteTable[i,"Vote.To"])
		voteType = as.character(voteTable[i,"Type"])
		voteScore = as.integer(voteTable[i,"value"])
	
		fromLat = countryList$lat[countryList$Country == voteFrom]
		fromLon = countryList$lon[countryList$Country == voteFrom]
		toLat = countryList$lat[countryList$Country == voteTo]
		toLon = countryList$lon[countryList$Country == voteTo]

		if (DrawTheLines == 1) {
	
			points = as.data.frame(gcIntermediate(c(fromLon, fromLat), c(toLon, toLat), numberOfPoints, addStartEnd=TRUE, breakAtDateLine = T))
		
			# Sort our visualisation
			if (voteType == "Jury") {
				# Blue to green
				points[, "id"] <- 1:nrow(points)
			} else {
				# Red to yellow
				points[, "id"] <- (1:nrow(points))+(numberOfPoints+2)
			}
		
			maxAlphaScore = 100 + numberOfPoints + 2
			points[, "lwidth"] <- as.double(101:maxAlphaScore)

			points$lwidth <- (((((points$lwidth / 100) - 1) / (numberOfPoints+2)) * 100 ) * voteScore ) / 12
			#points[1,"lwidth"] = 1
	
			widthOutput = widthOutputBase * voteScore / 12
			#alphaIndex = voteScore / 12
	
			#cat(paste0(i," - ",voteFrom," -> ",voteScore," -> ",voteTo," of type ",voteType," ",colourOutput),"\n")
	
			p <- p + geom_path(
				data=points,
				aes(x=lon,y=lat,color=id,alpha=lwidth),
				size=widthOutput,
				#alpha=alphaIndex,
				lineend="round"
				#arrow=arrow(angle=10,ends="last",length=unit(0.1,"cm"),type="open")
			)
		}
	}
}

# Do the drawing
p

cat("Finished")