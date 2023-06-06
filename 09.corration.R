load(file = "Remove the high-level raw data.Rdata")

cor_exp=cbind(rawdata_new,completeclinicaldata)
cor_exp=cor_exp[,-(34:36)]
cor_exp=cor_exp[,-48]


as_numeric_data_frame<- function(.dta,colname=''){
  if (length(colname)==0) colname=names(.dta)
  if (length(colname) != sum(colname %in% names(.dta))) stop('Col Name not match');
  .dta[,colname] <-  lapply( .dta[,colname], as.numeric)
  .dta
}


cor_exp=as_numeric_data_frame(.dta=cor_exp,colname=colnames(cor_exp))
colnames(cor_exp)
cor_exp=cor_exp[,c(30,22,8,2,33,27,6,18,3,17,28,14,19,10,5,34:47)]
cor_exp=cor_exp[1:241,]
cor_exp1=round(cor(cor_exp,use ="na.or.complete",method = c("spearman")),3)

library(Hmisc)
cortest <- rcorr(as.matrix(cor_exp), type = "spearman")
View(cortest$r)
cortest$r=round(cortest$r,2)
View(cortest$P)
cortest$P=round(cortest$P,3)
write.csv(cortest$r,file = "COR.csv")
write.csv(cortest$P,file = "CORP.csv")



library(ggplot2)
library(ggtree)
library(aplot)
library(tidyr)
library(magrittr)
library(ggcor)
library(ggplot2)


pdf(file="cor_matr.pdf",width = 23,height = 15)
quickcor(cor_exp, cluster = F,type = "upper",cor.test = TRUE,method = "spearman") +
  geom_colour(data = get_data(type = "upper")) +
  geom_mark(data = get_data( type = "upper"),size=2.5,color="black",fontface=2)+
  scale_fill_gradientn(colours = c("#77C034","white" ,"#C388FE"))+
  geom_panel_grid(colour = "white",size = 0.3)
dev.off()



