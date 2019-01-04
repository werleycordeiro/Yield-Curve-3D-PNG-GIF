library(Quandl)
library(xts)
data <- Quandl("USTREASURY/YIELD")
bondsUS<-as.matrix(data[,which(names(data)=="1 MO"):which(names(data)=="30 YR")])
bondsUS<-bondsUS[3:501,]
bondsUS<- xts(bondsUS, order.by = data[3:501,1])
bondsUS<-bondsUS[,-2]
# write.csv(bondsUS, file="bonds.csv")
bondsUSm <- apply.monthly(xts(bondsUS), mean)
Maturidade<-t(t(bondsUSm))
z<-Maturidade

#-----------#
# Imagem 3D #
#-----------#
col.pal<-colorRampPalette(c("midnightblue","blue","cyan3","cyan2","cyan1","cyan"))
colors<-col.pal(100)
# height of facets
z.facet.center <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)])/4
# Range of the facet center on a 100-scale (number of colors)
z.facet.range<-cut(z.facet.center, 100)
par(bg = "white")
persp(z=z,x=seq(2017, 2019, length.out = nrow(z)),y=c(1,3,6,12,24,36,60,84,120,240,360),col=colors[z.facet.range],xlab ="Data", ylab ="Maturidade", zlab = "Taxa",theta=30,ticktype="detailed",phi=15,expand=0.4,nticks=2)
