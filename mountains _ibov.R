# Run

font_import()
loadfonts(device = "win")

# library
library(extrafont)
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(tidyverse)

setwd(" ... ")
data <- read.csv("Bovespa_Historical_Data.csv", header=TRUE)
data0 <- data[,c(1,7)]
data0[,2] <- gsub("%","",data0[,2])
mat <- matrix(NA,12,27)
month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

for(i in 1:12){
	tmp <- rev(as.numeric(data0[grepl(month[i], ignore.case = TRUE, data0[,1]),2]))
	mat[i, 1:length(tmp)] <- tmp
}

rownames(mat) <- month
colnames(mat) <- as.character(1995:2021)
mat

# Transform matrix 'mat'

month2 <- c("January","February","March","April","May","June","July","August","September","October","November","December")
for(i in 1:12){


	if(i==1){

		tmp <- mat[i,]
		tmp0 <- cbind(rep(month2[i], 27), tmp)		

	}else{

		tmp1 <- cbind(rep(month2[i], 27), mat[i,])
		tmp0 <- rbind(tmp0, tmp1)

	}

}

tmp0 <- data.frame(tmp0[,1],as.numeric(tmp0[,2]))
colnames(tmp0) <- c("Month","Returns")
tmp0 <- as_tibble(tmp0)

# Plot

gghistmap <- ggplot(tmp0, aes(x = `Returns`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(option = "H", direction = -1) +
  labs(title = 'Monthly Returns of IBOV from 1995 to 2021') +
  theme(axis.title.y = element_text(size=40, vjust=0.5)) +
  theme(axis.title.x = element_text(size=40, vjust=-0.5)) +
  theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
gghistmap

# Save

width = 16
height = 16
ggsave(filename = "Hist_IBOV.pdf", gghistmap,scale = 2, width = width, height = height, device='pdf')

