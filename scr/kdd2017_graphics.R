#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
#			GRAFICOS 1 - 1x3
#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
 rm(list=ls(all=TRUE))
 setwd("C:\\Users\\Raquel Aoki\\Google Drive\\UFMG\\Pesquisa\\V035")
skill = read.table("v035_skill.csv", header=T, sep=";")
coefs = read.table("v035_coeficientes_estimados.csv", header=T, sep=";")
temporadas = c("1112","1213","1314","1415","1516") 
anos = 2012:2016 
require(ggplot2)
require(RColorBrewer )
require(gridExtra )

#Graphic with correlations

Correlation=c()
i=2
for(indice in 1:length(anos)){
	Correlation[indice] = cor.test(skill[,i],exp(skill[,i+1]))$estimate
	i = i + 4
}
Seasons = anos

Correlation = data.frame(cbind(Seasons,Correlation))
aux <- ggplot(Correlation,aes(x=Seasons,y=Correlation))
plot1 <- aux + geom_bar(stat= "identity",fill="dodgerblue3") +
coord_cartesian(ylim=c(0,1))+ theme_bw()

#----- graphic with 2016 season correlation
i = 18
s16= skill[,c(i,i+1,i+2,i+3)]
s16$shape = c()
s16$shape[s16$pos2016==1]= "Champion"
s16$shape[s16$pos2016==2]= "2º Position"
s16$shape[s16$pos2016==4]= "4º Positions"
s16$shape[s16$pos2016==8]= "8º Positions"
s16$shape[s16$pos2016==16]= "16º Positions"
s16$shape[s16$pos2016>16]= "Not Classified"
s16$shape = as.factor(s16$shape)
values.shape = c(80,50,52,56,49,78)

aux = brewer.pal(6,"Pastel1") #Paired
s16$cores = c()
s16$cores[s16$pos2016==1]= "gold"
s16$cores[s16$pos2016==2]= "gray66"
s16$cores[s16$pos2016==4]= "lightgreen"
s16$cores[s16$pos2016==8]= aux[2]
s16$cores[s16$pos2016==16]= aux[5]
s16$cores[s16$pos2016>16]= aux[1]

aux1 <- ggplot(s16, aes(won2016, hab2016))
plot2 <- aux1 +  geom_point(colour=s16$cores, size = 5.5) +
geom_point(data=s16,mapping=aes(shape=as.factor(shape)),size=3)+
scale_shape_manual(values=values.shape)+
scale_y_continuous(name="Teams' Skill") +
scale_x_continuous(name="Games Won") +
guides(shape=FALSE)+
theme_bw()


#----- graphic coeficientes nas temporadas 

coefs = t(coefs)
temporadas = as.factor(as.character(2012:2016))
cores = c("red","blue","green","orange","pink")
nomes = c("INTCP","CO","A5","AP","VL","RC","SI","RE")
i = 1
tab = data.frame(Value = coefs[,i],Coefficient  = nomes, 
	Season = rep(temporadas[i],length(nomes)))
for(i in 2:dim(coefs)[2]){
	tab.aux = data.frame(Value = coefs[,i],Coefficient  = nomes, 
	Season = rep(temporadas[i],length(nomes)))
	tab = rbind(tab, tab.aux)
}
tab$Coefficient <- ordered(tab$Coefficient, levels = nomes)

p5 <- ggplot(tab, aes(x = Coefficient, y = Value, group = Season, color=Season))
plot3<- p5 + geom_line(linetype='dashed', size=1, color="black",y=0)+
geom_line(size=1.2) + geom_point(size=2.5)+theme_bw()+
 theme(legend.position="top")+guides(col = guide_legend(ncol = 3))


g = grid.arrange(plot1, plot2,plot3, nrow=1, ncol=3)
ggsave(filename="graficos1.png", g)


#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
#			GRAFICOS 2 - 1x3
#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
 rm(list=ls(all=TRUE))
skill = read.table("v035_skill.csv", header=T, sep=";")
coefs = read.table("v035_coeficientes_estimados.csv", header=T, sep=";")
temporadas = c("1112","1213","1314","1415","1516") 
anos = 2012:2016 
require(ggplot2)
require(RColorBrewer )
require(gridExtra )

#Graphic with correlations

Correlation=c()
i=2
for(indice in 1:length(anos)){
	Correlation[indice] = cor.test(skill[,i],exp(skill[,i+1]))$estimate
	i = i + 4
}
Seasons = anos

Correlation = data.frame(cbind(Seasons,Correlation))
aux <- ggplot(Correlation,aes(x=Seasons,y=Correlation))
plot1 <- aux + geom_bar(stat= "identity",fill="dodgerblue3") +
coord_cartesian(ylim=c(0,1))+ theme_bw()

#----- graphic with 2016 season correlation
i = 18
s16= skill[,c(i,i+1,i+2,i+3)]
s16$shape = c()
s16$shape[s16$pos2016==1]= "Champion"
s16$shape[s16$pos2016==2]= "2º Position"
s16$shape[s16$pos2016==4]= "4º Positions"
s16$shape[s16$pos2016==8]= "8º Positions"
s16$shape[s16$pos2016==16]= "16º Positions"
s16$shape[s16$pos2016>16]= "Not Classified"
s16$shape = as.factor(s16$shape)
values.shape = c(80,50,52,56,49,78)

aux = brewer.pal(6,"Pastel1") #Paired
s16$cores = c()
s16$cores[s16$pos2016==1]= "gold"
s16$cores[s16$pos2016==2]= "gray66"
s16$cores[s16$pos2016==4]= "lightgreen"
s16$cores[s16$pos2016==8]= aux[2]
s16$cores[s16$pos2016==16]= aux[5]
s16$cores[s16$pos2016>16]= aux[1]

aux1 <- ggplot(s16, aes(won2016, hab2016))
plot2 <- aux1 +  geom_point(colour="lightgreen", size = 5.5) +
geom_point(data=s16,mapping=aes(shape=as.factor(shape)),size=3)+
scale_shape_manual(values=values.shape)+
scale_y_continuous(name="Teams' Skill") +
scale_x_continuous(name="Games Won") +
guides(shape=FALSE)+
theme_bw()


#----- graphic coeficientes nas temporadas 

coefs = t(coefs)
temporadas = as.factor(as.character(2012:2016))
cores = c("red","blue","green","orange","pink")
nomes = c("INTCP","CO","A5","AP","VL","RC","SI","RE")
i = 1
tab = data.frame(Value = coefs[,i],Coefficient  = nomes, 
	Season = rep(temporadas[i],length(nomes)))
for(i in 2:dim(coefs)[2]){
	tab.aux = data.frame(Value = coefs[,i],Coefficient  = nomes, 
	Season = rep(temporadas[i],length(nomes)))
	tab = rbind(tab, tab.aux)
}
tab$Coefficient <- ordered(tab$Coefficient, levels = nomes)

p5 <- ggplot(tab, aes(x = Coefficient, y = Value, group = Season, color=Season))
plot3<- p5 + geom_line(linetype='dashed', size=1, color="black",y=0)+
geom_line(size=1.2) + geom_point(size=2.5)+theme_bw()+
 theme(legend.position="top")+guides(col = guide_legend(ncol = 3))


g = grid.arrange(plot1, plot2,plot3, nrow=1, ncol=3)
ggsave(filename="graficos2.png", g)

#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
#			GRAFICOS 3- 1x3
#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
 rm(list=ls(all=TRUE))
skill = read.table("v035_skill.csv", header=T, sep=";")
coefs = read.table("v035_coeficientes_estimados.csv", header=T, sep=";")
temporadas = c("1112","1213","1314","1415","1516") 
anos = 2012:2016 
require(ggplot2)
require(RColorBrewer )
require(gridExtra )

#Graphic with correlations

Correlation=c()
i=2
for(indice in 1:length(anos)){
	Correlation[indice] = cor.test(skill[,i],exp(skill[,i+1]))$estimate
	i = i + 4
}
Seasons = anos

Correlation = data.frame(cbind(Seasons,Correlation))
aux <- ggplot(Correlation,aes(x=Seasons,y=Correlation))
plot1 <- aux + geom_bar(stat= "identity",fill="dodgerblue3") +
coord_cartesian(ylim=c(0,1))+ theme_bw()

#----- graphic with 2016 season correlation
i = 18
s16= skill[,c(i,i+1,i+2,i+3)]
s16$shape = c()
s16$shape[s16$pos2016==1]= "Champion"
s16$shape[s16$pos2016==2]= "2º Position"
s16$shape[s16$pos2016==4]= "4º Positions"
s16$shape[s16$pos2016==8]= "8º Positions"
s16$shape[s16$pos2016==16]= "16º Positions"
s16$shape[s16$pos2016>16]= "Not Classified"
s16$shape = as.factor(s16$shape)
values.shape = c(80,50,52,56,49,78)

aux = brewer.pal(6,"Pastel1") #Paired
s16$cores = c()
s16$cores[s16$pos2016==1]= "gold"
s16$cores[s16$pos2016==2]= "gray66"
s16$cores[s16$pos2016==4]= "lightgreen"
s16$cores[s16$pos2016==8]= aux[2]
s16$cores[s16$pos2016==16]= aux[5]
s16$cores[s16$pos2016>16]= aux[1]

aux1 <- ggplot(s16, aes(won2016, hab2016))
plot2 <- aux1 +  #geom_point(colour=s16$cores, size = 5.5) +
geom_point(data=s16,mapping=aes(shape=as.factor(shape)),size=3)+
scale_shape_manual(values=values.shape)+
scale_y_continuous(name="Teams' Skill") +
scale_x_continuous(name="Games Won") +
guides(shape=FALSE)+
theme_bw()


#----- graphic coeficientes nas temporadas 

coefs = t(coefs)
temporadas = as.factor(as.character(2012:2016))
cores = c("red","blue","green","orange","pink")
nomes = c("INTCP","CO","A5","AP","VL","RC","SI","RE")
i = 1
tab = data.frame(Value = coefs[,i],Coefficient  = nomes, 
	Season = rep(temporadas[i],length(nomes)))
for(i in 2:dim(coefs)[2]){
	tab.aux = data.frame(Value = coefs[,i],Coefficient  = nomes, 
	Season = rep(temporadas[i],length(nomes)))
	tab = rbind(tab, tab.aux)
}
tab$Coefficient <- ordered(tab$Coefficient, levels = nomes)

p5 <- ggplot(tab, aes(x = Coefficient, y = Value, group = Season, color=Season))
plot3<- p5 + geom_line(linetype='dashed', size=1, color="black",y=0)+
geom_line(size=1.2) + geom_point(size=2.5)+theme_bw()+
 theme(legend.position="top")+guides(col = guide_legend(ncol = 3))


g = grid.arrange(plot1, plot2,plot3, nrow=1, ncol=3)
ggsave(filename="graficos3.png", g)





#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
#			GRAFICOS 4 - 1x3
#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
 rm(list=ls(all=TRUE))
skill = read.table("v035_skill.csv", header=T, sep=";")
coefs = read.table("v035_coeficientes_estimados.csv", header=T, sep=";")
temporadas = c("1112","1213","1314","1415","1516") 
anos = 2012:2016 
require(ggplot2)
require(RColorBrewer )
require(gridExtra )

#Graphic with correlations

Correlation=c()
i=2
for(indice in 1:length(anos)){
	Correlation[indice] = cor.test(skill[,i],exp(skill[,i+1]))$estimate
	i = i + 4
}
Seasons = anos

Correlation = data.frame(cbind(Seasons,Correlation))
aux <- ggplot(Correlation,aes(x=Seasons,y=Correlation))
plot1 <- aux + geom_point(color="dodgerblue3",size=3.5) +
coord_cartesian(ylim=c(0,1))+ theme_bw()+
geom_line(color="dodgerblue3",size=1.5)
#geom_line(y=0,color="black",size=0.5)

#----- graphic with 2016 season correlation
i = 18
s16= skill[,c(i,i+1,i+2,i+3)]
s16$shape = c()
s16$shape[s16$pos2016==1]= "Champion"
s16$shape[s16$pos2016==2]= "2º Position"
s16$shape[s16$pos2016==4]= "4º Positions"
s16$shape[s16$pos2016==8]= "8º Positions"
s16$shape[s16$pos2016==16]= "16º Positions"
s16$shape[s16$pos2016>16]= "Not Classified"
s16$shape = as.factor(s16$shape)
values.shape = c(80,50,52,56,49,78)

aux = brewer.pal(6,"Pastel1") #Paired
s16$cores = c()
s16$cores[s16$pos2016==1]= "gold"
s16$cores[s16$pos2016==2]= "gray66"
s16$cores[s16$pos2016==4]= "lightgreen"
s16$cores[s16$pos2016==8]= aux[2]
s16$cores[s16$pos2016==16]= aux[5]
s16$cores[s16$pos2016>16]= aux[1]

aux1 <- ggplot(s16, aes(won2016, hab2016))
plot2 <- aux1 +  geom_point(colour="lightgreen", size = 5.5) +
geom_point(data=s16,mapping=aes(shape=as.factor(shape)),size=3)+
scale_shape_manual(values=values.shape)+
scale_y_continuous(name="Teams' Skill") +
scale_x_continuous(name="Games Won") +
guides(shape=FALSE)+
theme_bw()


#----- graphic coeficientes nas temporadas 

coefs = t(coefs)
temporadas = as.factor(as.character(2012:2016))
cores = c("red","blue","green","orange","pink")
nomes = c("INTCP","CO","A5","AP","VL","RC","SI","RE")
i = 1
tab = data.frame(Value = coefs[,i],Coefficient  = nomes, 
	Season = rep(temporadas[i],length(nomes)))
for(i in 2:dim(coefs)[2]){
	tab.aux = data.frame(Value = coefs[,i],Coefficient  = nomes, 
	Season = rep(temporadas[i],length(nomes)))
	tab = rbind(tab, tab.aux)
}
tab$Coefficient <- ordered(tab$Coefficient, levels = nomes)

p5 <- ggplot(tab, aes(x = Coefficient, y = Value, group = Season, color=Season))
plot3<- p5 + geom_line(linetype='dashed', size=1, color="black",y=0)+
geom_line(size=1.2) + geom_point(size=2.5)+theme_bw()+
 theme(legend.position="top")+guides(col = guide_legend(ncol = 3))


g = grid.arrange(plot1, plot2,plot3, nrow=1, ncol=3)
ggsave(filename="graficos4.png", g)


#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
#			GRAFICOS 5 - probabily win, lose, draw
#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#

require(ggplot2)
anos = 2007:2015
arq = paste("soccer_Brazil_men_",anos,".txt",sep="")


i = 1
hom = c();emp = c();vis = c()
for(i in 1:length(anos)){
 bd = read.table(arq[i],header=T, sep=";")
 hom[i] = dim(subset(bd, gol_man>gol_vis))[1]/dim(bd)[1]
 emp[i] = dim(subset(bd, gol_man==gol_vis))[1]/dim(bd)[1]
 vis[i] = dim(subset(bd, gol_man<gol_vis))[1]/dim(bd)[1]
}

Probability = c(hom, emp, vis)
Season = c(anos, anos, anos)
Result = c(rep("Home Win", length(anos)),rep("Draw", length(anos)),
	rep("Away Win", length(anos)))
tab1 = data.frame(Probability, Season, Result)

p6 <- ggplot(tab1, aes(x = Season, y = Probability, group = Result, color=Result))
g <- p6 + geom_line(size=1.2) + geom_point(size=2.5)+theme_bw()+
 	theme(text = element_text(size=12),axis.text=element_text(size=12),
	legend.position=c(0.32,0.9),legend.title=element_text(size=13),
	legend.text=element_text(size=13))+guides(col = guide_legend(ncol = 3))+
  	ylim(0,1)
g
ggsave(filename="graficos5.png", g)


#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
#			GRAFICOS 6 - boxplot by sport scale phi
#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#

rm(list = ls( all = T))
bd = read.table("v031_escala_phi_jogos.csv",header=T, sep=";")
bd = subset(bd, equipes>=7)

bd$sport = as.character(bd$sport)
bd$sport[bd$sport=="basketball"]="Basketball"
bd$sport[bd$sport=="handball"]="Handball"
bd$sport[bd$sport=="soccer"]="Soccer"
bd$sport[bd$sport=="voleyball"]="Volleyball"

par(mar=c(4.2,4,1,1))
#boxplot(bd$valor.phi~bd$sport,col = "seagreen3",main="",xlab="Sport",
#	ylab=expression(paste("Scale ",phi,sep=" ")),ylim=c(-1,1),cex.axis=1.2,cex.lab=1.2)
boxplot(bd$valor.phi~bd$sport,col = "seagreen3",main="",xlab="Sport",
	ylab="Skill Coefficient",ylim=c(-1,1),cex.axis=1.2,cex.lab=1.2)
abline(h = seq(-2,2,by=0.5),lty=2,lwd=1,col="lightgrey")
boxplot(bd$valor.phi~bd$sport,col = "seagreen3",axes=F,add=T)
arrows(3.3, -0.5, x1 = 3.1, y1 = -0.6, length = 0.15, 
	angle = 30, col="red",cex = 1.5,lwd=2)
text(3.9,-0.46,"Ghana 2014", col="red",cex = 1.2)
text(3,-0.99,"*",col="red",lwd=5,cex = 1.5)
arrows(3.3, -0.89, x1 = 3.1, y1 = -0.99, length = 0.15, 
	angle = 30, col="red",cex = 1.5,lwd=2)
text(3.9,-0.85,"Algeria 2015", col="red",cex = 1.2)

#* citar argelia
savePlot(filename="escala_phi_esportes", type="png", device=dev.cur())


#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
#			GRAFICOS 7 - cumulative phi
#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#

rm(list = ls( all = T))
bd = read.table("v035_escala_phi_jogos_acumulado.csv",header=T, sep=";", dec=",")
bdt = read.table("v035_escala_phi_jogos_todos.csv", header=T, sep=";", dec=",")
require(RColorBrewer )
require(gridExtra )
require(ggplot2)

liga = c("NBA", "Primera División", "Série A","Premier League")
aux1 = subset(bd, league=="NBA")
aux2 = subset(bd, league=="Primera Division")
aux3 = subset(bd, league=="Serie A")
aux4 = subset(bd, league=="Premier League")
aux1t = subset(bdt, league=="NBA" )
aux2t = subset(bdt, league=="Primera Division" & country=="Spain")
aux3t = subset(bdt, league=="Serie A" & country=="Brazil")
aux4t = subset(bdt, league=="Premier League"& country=="England" )


anos = 2007:2016
aux = data.frame(Phi = aux1$valor.phi,Season = anos[1:dim(aux1)[1]], 
	League = liga[1], Simbolo = aux1$pch) 
aux.0 = data.frame(Phi = aux2$valor.phi,Season = anos[1:dim(aux2)[1]], 
	League = liga[2],Simbolo = aux2$pch) 
aux.1 = data.frame(Phi = aux3$valor.phi,Season = anos[1:dim(aux3)[1]], 
	League = liga[3],Simbolo = aux3$pch)
aux.2 = data.frame(Phi = aux4$valor.phi,Season = anos[1:dim(aux4)[1]], 
	League = liga[4],Simbolo = aux4$pch)
auxa = rbind(aux, aux.0, aux.1, aux.2)

aux = data.frame(Phi = aux1t$valor.phi,Season = anos[1:dim(aux1t)[1]], 
	League = liga[1], Simbolo = aux1t$pch) 
aux.0 = data.frame(Phi = aux2t$valor.phi,Season = anos[1:dim(aux2t)[1]], 
	League = liga[2],Simbolo = aux2t$pch) 
aux.1 = data.frame(Phi = aux3t$valor.phi,Season = anos[1:dim(aux3t)[1]], 
	League = liga[3],Simbolo = aux3t$pch)
aux.2 = data.frame(Phi = aux4t$valor.phi,Season = anos[1:dim(aux4t)[1]], 
	League = liga[4],Simbolo = aux4t$pch)
auxt = rbind(aux, aux.0, aux.1, aux.2)


aux1t$perc = 100*aux1t$retirados/aux1t$equipes
aux2t$perc = 100*aux2t$retirados/aux2t$equipes
aux3t$perc = 100*aux3t$retirados/aux3t$equipes
aux4t$perc = 100*aux4t$retirados/aux4t$equipes

aux = data.frame(Percent = aux1t$perc,Season = anos[1:dim(aux1t)[1]], 
	League = liga[1], Simbolo = aux1t$pch) 
aux.0 = data.frame(Percent= aux2t$perc,Season = anos[1:dim(aux2t)[1]], 
	League = liga[2],Simbolo = aux2t$pch) 
aux.1 = data.frame(Percent= aux3t$perc,Season = anos[1:dim(aux3t)[1]], 
	League = liga[3],Simbolo = aux3t$pch)
aux.2 = data.frame(Percent= aux4t$perc,Season = anos[1:dim(aux4t)[1]], 
	League = liga[4],Simbolo = aux4t$pch)
auxp = rbind(aux, aux.0, aux.1, aux.2)


auxa$Phi = as.numeric(as.character(auxa$Phi))
f1 <- ggplot(auxa, aes(x = Season, y = Phi, group = League, color=League, shape = Simbolo))
f11 <- f1 + geom_line(size=1.) + geom_point(size=2.5)+theme_bw()+ ggtitle("(b)")+  
	ylim(0,1.05)+ylab("Skill Coefficient")+
      theme(legend.position=c(0.51,0.14),text = element_text(size=11),
	axis.text=element_text(size=12), legend.title=element_text(size=12),
	legend.text=element_text(size=11))+
	guides(col = guide_legend(ncol = 3))+guides(shape=FALSE)
f11
auxt$Phi = as.numeric(as.character(auxt$Phi))
f2 <- ggplot(auxt, aes(x = Season, y = Phi, group = League, color=League, shape = Simbolo))
f22 <- f2 + geom_line(size=1.) + geom_point(size=2.5)+theme_bw()+ ggtitle("(a)") +
	ylim(0,1.05)+ylab("Skill Coefficient")+
      theme(legend.position=c(0.45,0.14),text = element_text(size=11),
	axis.text=element_text(size=12), legend.title=element_text(size=12),
	legend.text=element_text(size=12))+
	guides(col = guide_legend(ncol = 3))+guides(shape=FALSE)+guides(color=FALSE)
f22
auxp$Percent = as.numeric(as.character(auxp$Percent))
f3 <- ggplot(auxp, aes(x = Season, y = Percent, group = League, color=League, shape = Simbolo))
f33 <- f3 + geom_line(size=1.) + geom_point(size=2.5)+theme_bw()+ ggtitle("(c)") +
	ylim(-15,105)+ylab("% Teams Removed")+
      theme(legend.position=c(0.25,0.1),text = element_text(size=11),
	axis.text=element_text(size=12), legend.title=element_text(size=12),
	legend.text=element_text(size=12))+
	guides(shape=FALSE)+guides(color=FALSE)

f33

g = grid.arrange(f22,f11,f33, nrow=1, ncol=3)
ggsave(filename="graficos7b.png",g)



#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
#			Tabela 1 - 1x3
#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
rm(list=ls(all=TRUE))
setwd("C:\\Users\\Raquel Aoki\\Google Drive\\UFMG\\Pesquisa\\V035")
skill = read.table("v035_skill.csv", header=T, sep=";")
playof = read.table("inicio_playoff.csv", header=T, sep=",")

anos= 2012:2016
temp = c("1112","1213","1314","1415","1516")
i=1
for(i in 1:length(anos)){
	jogos = read.table(paste("jogos_basket_nba_eua_",temp[i],".csv",sep=""),
		sep=";",header=T)
	jogos=  subset(jogos, data2<playof$inicio_playof[playof$NBA==as.numeric(as.character(temp[i]))])
	jogos$dif = jogos$score_x - jogos$score_y
	col = paste("hab",anos[i],sep="")
	sk1 = subset(skill, select=c("team",col))
	names(sk1)[2] = "hab_x"
	jogos = merge(jogos, sk1, by.x="home", by.y="team", all.x=T)
	names(sk1)[2] = "hab_y"
	jogos = merge(jogos, sk1, by.x="visitor", by.y="team", all.x=T)
	jogos$hdif = jogos$hab_x - jogos$hab_y		
	jogos$prod = jogos$dif*jogos$hdif 
	jogos$prod[jogos$prod >0]=1
	jogos$prod[jogos$prod <=0]=0
	if(i==1){
	tab1 = table(jogos$prod) 
	}else{
		tab1 = rbind(tab1,table(jogos$prod))
	}
}

tab1 = data.frame(cbind(anos,tab1,rowSums(tab1)))
names(tab1) = c("anos", "under","notunder","total")
tab1$under.p =  tab1$under*100/tab1$total
tab1$notunder.p =  tab1$notunder*100/tab1$total
tab1 = tab1[,c(1,2,5,3,6,4)]
require(xtable)

tab1$anos = as.character(tab1$anos)
tab1$under = as.character(tab1$under)
tab1$notunder = as.character(tab1$notunder)
tab1$total = as.character(tab1$total)
print(xtable(tab1,align="lc|cc|cc|c",digits=2,caption="Number of matches where the underdog won"),
	include.rownames=F,caption.placement="top")


#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
#			Tabela 1 - adicionando mandante
#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
rm(list=ls(all=TRUE))
setwd("C:\\Users\\Raquel Aoki\\Google Drive\\UFMG\\Pesquisa\\V035")
skill = read.table("v035_skill.csv", header=T, sep=";")
playof = read.table("inicio_playoff.csv", header=T, sep=",")
removidos = read.table("v035_escala_phi_jogos_todos.csv", header=T, sep=";")
removidos = subset(removidos , league=="NBA")

anos= 2012:2016
temp = c("1112","1213","1314","1415","1516")
i=1
for(i in 1:length(anos)){
	jogos = read.table(paste("jogos_basket_nba_eua_",temp[i],".csv",sep=""),
		sep=";",header=T)
	jogos=  subset(jogos, data2<playof$inicio_playof[playof$NBA==as.numeric(as.character(temp[i]))])
	jogos$dif = jogos$score_x - jogos$score_y
	col = paste("hab",anos[i],sep="")
	sk1 = subset(skill, select=c("team",col))
	names(sk1)[2] = "hab_x"
	jogos = merge(jogos, sk1, by.x="home", by.y="team", all.x=T)
	names(sk1)[2] = "hab_y"
	jogos = merge(jogos, sk1, by.x="visitor", by.y="team", all.x=T)
	jogos$hdif = jogos$hab_x - jogos$hab_y		
	
	home.best = dim(subset(jogos,hdif>0 & dif>0))[1]
	away.under= dim(subset(jogos,hdif>=0 & dif<0))[1]
	home.under = dim(subset(jogos,hdif<=0 & dif>0))[1]
	away.best = dim(subset(jogos,hdif<0 & dif<0))[1]
	
	resul = c(home.best,away.best ,away.under, home.under)	

	if(i==1){
	tab1 = resul 
	}else{
		tab1 = rbind(tab1,resul)
	}
}

tab1 = data.frame(cbind(anos,tab1,rowSums(tab1)))
names(tab1) = c("anos", "home.best", "away.best", "away.under", "home.under","total")
tab1$home.best.p  = paste(round(tab1$home.best*100/tab1$total,2),"%",sep="")
tab1$away.best.p  = paste(round(tab1$away.best*100/tab1$total,2),"%",sep="")
tab1$away.under.p = paste(round(tab1$away.under*100/tab1$total,2),"%",sep="")
tab1$home.under.p = paste(round(tab1$home.under*100/tab1$total,2),"%",sep="")

tab1 = tab1[,c(1,2,7,3,8,5,10,4,9,6)]

require(xtable)

print(xtable(tab1,align="lc|cc|cc|cc|cc|c",digits=0,caption="Amount of matches that each group won"),
	include.rownames=F,caption.placement="top")


#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
#			Tabela 1 - adicionando mandante e removidos
#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#
rm(list=ls(all=TRUE))
setwd("C:\\Users\\Raquel Aoki\\Google Drive\\UFMG\\Pesquisa\\V035")
skill = read.table("v035_skill.csv", header=T, sep=";")
playof = read.table("inicio_playoff.csv", header=T, sep=",")
removidos = read.table("v035_escala_phi_jogos_todos.csv", header=T, sep=";")
removidos = subset(removidos , league=="NBA")

anos= 2012:2016
temp = c("1112","1213","1314","1415","1516")
i=1
for(i in 1:length(anos)){
	rem = subset(removidos,year == anos[i])$equipes.retiradas
	aux = unlist(strsplit(as.character(rem),",",fixed=T))
	auxp = grep("+", aux,  value=TRUE,fixed=T)
	auxn = grep("-", aux,  value=TRUE,fixed=T)
	auxp = gsub("+","",auxp,fixed=T)
	auxn = gsub("-","",auxn,fixed=T)
	auxp= data.frame(eq = auxp, rep("P",length(auxp)))
	auxn= data.frame(eq = auxn, rep("N",length(auxn)))
	names(auxp) = names(auxn) = c("eq","sinal")
	aux = rbind(auxp,auxn)

	jogos = read.table(paste("jogos_basket_nba_eua_",temp[i],".csv",sep=""),
		sep=";",header=T)
	jogos=  subset(jogos, data2<playof$inicio_playof[playof$NBA==as.numeric(as.character(temp[i]))])
	jogos$dif = jogos$score_x - jogos$score_y
	col = paste("hab",anos[i],sep="")
	sk1 = subset(skill, select=c("team",col))
	names(sk1)[2] = "hab_x"
	jogos = merge(jogos, sk1, by.x="home", by.y="team", all.x=T)
	names(sk1)[2] = "hab_y"
	jogos = merge(jogos, sk1, by.x="visitor", by.y="team", all.x=T)
	jogos$hdif = jogos$hab_x - jogos$hab_y		

	jogos1 = subset(jogos, dif>0)
	jogos2 = subset(jogos, dif<0)
	jogos1 = merge(jogos1, aux, by.x = "nome.home",by.y="eq",all.x=T)
	jogos2 = merge(jogos2, aux, by.x = "nome.visitor",by.y="eq",all.x=T)	
	jogos = rbind(jogos1,jogos2)
	jogos$sinal = as.character(jogos$sinal)
	jogos$sinal[is.na(jogos$sinal)]="-"

	#positivos 
	jogos.a = subset(jogos, sinal=="P")
	#negativos 
	jogos.b = subset(jogos, sinal=="N")
	#nao retirados 
	jogos.c = subset(jogos, sinal=="-")

	resul.a = c(dim(subset(jogos.a,hdif>0 & dif>0))[1],
			dim(subset(jogos.a,hdif<0 & dif<0))[1],
			dim(subset(jogos.a,hdif<=0 & dif>0))[1],
			dim(subset(jogos.a,hdif>=0 & dif<0))[1])	
	resul.b = c(dim(subset(jogos.b,hdif>0 & dif>0))[1],
			dim(subset(jogos.b,hdif<0 & dif<0))[1],
			dim(subset(jogos.b,hdif<=0 & dif>0))[1],
			dim(subset(jogos.b,hdif>=0 & dif<0))[1])
	resul.c = c(dim(subset(jogos.c,hdif>0 & dif>0))[1],
			dim(subset(jogos.c,hdif<0 & dif<0))[1],
			dim(subset(jogos.c,hdif<=0 & dif>0))[1],
			dim(subset(jogos.c,hdif>=0 & dif<0))[1])
	resul = rbind(resul.a,resul.b,resul.c)

	if(i==1){
	tab1 = resul
	}else{
		tab1 = rbind(tab1,resul)
	}
}

anos = rep(anos,3)[order(rep(anos,3))]
time.vencedor = rep(c("Removido+","Não Removido","Removido-"),3)
tab1 = data.frame(cbind(anos,time.vencedor,tab1,rowSums(tab1)))
for(i in 3:7) tab1[,i] = as.numeric(as.character(tab1[,i]))

names(tab1) = c("anos","time.vencedor" ,"home.best", "away.best", "away.under", "home.under","total")
tab2 = tab1

tab1$home.best.p  = paste(round(tab1$home.best*100/tab1$total,2),"%",sep="")
tab1$away.best.p  = paste(round(tab1$away.best*100/tab1$total,2),"%",sep="")
tab1$home.under.p = paste(round(tab1$home.under*100/tab1$total,2),"%",sep="")
tab1$away.under.p = paste(round(tab1$away.under*100/tab1$total,2),"%",sep="")

tab1 = tab1[,c(1,2,8:11,7)]

require(xtable)

print(xtable(tab1,align="lcc|cc|cc|c",digits=0,caption="Amount of matches that each group won"),
	include.rownames=F,caption.placement="top")


tab2$home.best.p  = tab2$home.best*100/tab2$total
tab2$away.best.p  = tab2$away.best*100/tab2$total
tab2$away.under.p = tab2$away.under*100/tab2$total
tab2$home.under.p = tab2$home.under*100/tab2$total
tab3 = tab2[,c(1,2,8:11)]

tab3[,2] = as.character(tab3[,2])
tab4 = data.frame(time.vencedor = c(tab3[,2],tab3[,2],tab3[,2],tab3[,2]),
	perc = c(tab3[,3],tab3[,4],tab3[,5],tab3[,6]),
	grupo = c(rep("home.best",dim(tab3)[1]),
			rep("away.best",dim(tab3)[1]),
			rep("home.under",dim(tab3)[1]),
			rep("away.under",dim(tab3)[1])))

write.table(tab2,"tabela2_comremocao.csv",row.names=F,sep=";")





