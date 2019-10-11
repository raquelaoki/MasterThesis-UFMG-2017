
#install.packages("rworldmap")
require(rworldmap)

setwd("C:\\Users\\Raquel Aoki\\Google Drive\\UFMG\\Pesquisa\\V032")

leagues = read.table("v031_escala_phi_jogos.csv", header=T, sep=";")
leagues = subset(leagues , equipes >=7)

leagues$country = as.character(leagues$country)
leagues$country[leagues$country=="Austria "]="Austria"
leagues$country[leagues$country=="Brazil "]="Brazil"
leagues$country[leagues$country=="Czech Republic "]="Czech Republic"
leagues$country[leagues$country=="England"]="United Kingdom"
leagues$country[leagues$country=="Scotland"]="United Kingdom"
leagues$country[leagues$country=="Wales"]="United Kingdom"
leagues$country[leagues$country=="USA"]="United States"
leagues$country[leagues$country=="Luxemburgo"]="Luxembourg"
leagues$country[leagues$country=="South Coreia"]="South Korea"
leagues$country[leagues$country=="Ulkraine"]="Ukraine"
leagues$country[leagues$country=="Maldova"]="Moldova, Republic of"
leagues$country[leagues$country=="Russia"]="Russian Federation"
leagues$country[leagues$country=="Singagore"]="Singapore"
leagues$country[leagues$country=="Vietnam"]="Viet Nam"
leagues$country[leagues$country=="South Korea"]="Korea, Republic of"


leagues$country[leagues$country=="United Arab Emirates "]="United Arab Emirates"
leagues = data.frame(table(leagues$country))

data(countrySynonyms)
aux = countrySynonyms[,c(2,3)]
leagues1 = merge(leagues, aux, all.x=T,by.x="Var1",by.y="name1")
aux = as.data.frame(sapply(leagues1$ISO3, toupper)) 
aux0 = row.names(aux); row.names(aux)= NULL
aux = data.frame(cbind(aux0, aux))
names(aux) = c("v1", "v2")
leagues2 = merge(leagues1, aux, by.x = "ISO3", by.y="v1")
leagues2 = leagues2[,-1]
names(leagues2) = c("Country", "leagues", "ISO3")
leagues2$ISO3 = as.character(leagues2$ISO3)


sPDF <- joinCountryData2Map(leagues2,joinCode = "ISO3",nameJoinColumn = "ISO3")
library(RColorBrewer)
#getting colours
colourPalette <- brewer.pal(7,'Blues')
#colourPalette <- brewer.pal(7,'Greys')
#plot map
mapDevice() #create world map shaped window
mapParams <- mapCountryData(sPDF,nameColumnToPlot="leagues",addLegend = F,
	colourPalette=colourPalette, borderCol = "grey",mapTitle = "" )

savePlot(filename = "world_map",type="png", device=dev.cur())


#https://journal.r-project.org/archive/2011-1/RJournal_2011-1_South.pdf
