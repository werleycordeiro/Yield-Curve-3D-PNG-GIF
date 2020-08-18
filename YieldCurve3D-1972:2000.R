data<-read.csv("https://www.dropbox.com/s/inpnlugzkddp42q/bonds.csv?dl=1",header = TRUE, sep = ";")
library(xts)
data <- as.matrix(data[,which(names(data)=="M3"):which(names(data)=="M120")])
datas	<- seq(as.Date("1972/1/1"), by = "month", length.out = 348)
juros	<- xts(data, order.by = datas)

Maturidade<-c(3,6,9,12,15,18,21,24,30,36,48,60,72,84,96,108,120)
z<-t(t(juros))


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
persp(z=z,x=seq(1970, 2000, length.out = nrow(z)),y=Maturidade,col=colors[z.facet.range],xlab ="Data", ylab ="Maturidade", zlab = "Taxa",theta=40,ticktype="detailed",phi=15,expand=0.4,nticks=5)

#-----#
# GIF #
#-----#

library(animation)
saveGIF(for(i in 1:90){ par(bg = "white"); persp(z=z,x=seq(1970, 2000, length.out = nrow(z)),y=c(3,6,9,12,15,18,21,24,30,36,48,60,72,84,96,108,120),col=colors[z.facet.range],xlab ="Data", ylab ="Maturidade", zlab = "Taxa",theta=i-40,ticktype="detailed",phi=15,expand=0.4,nticks=5) }, interval = 0.2)
