# Еременко Николай Максимович -для дневных ночных за лето 2019 года, постройте 
# регрессионную зависимость скорости сокотечения (переменная Flux) для деревьев вида Betula pendula

data = read.csv("data.csv")
summary(data)

data_filtered_1 = data[data$doy > 151 & data$doy < 244,]
data_filtered_2 = data_filtered_1[data_filtered_1$Species == "Betula pendula" ,]


library(dplyr)

data_filtered_2=select(data_filtered_2,-c("id", "Species","age_group_index","time", "antrop_load", 
                                          "in_site_antrop_load") )
library(ggplot2)

library(ggcorrplot)

corr = cor(data_filtered_2,use = "na.or.complete")^2

ggcorrplot(corr,
           tl.cex=4
)
flux_corr=corr[ ,"Flux"]
flux_corr=flux_corr[flux_corr>0.1]
flux_corr

formula8 = Flux ~ dTa+u+E1+G1
formula9 = Flux ~ dTa+u+E1+G1 + dTa:u:E1:G1

model8 = lm(data=data_filtered_2, formula8)
model9 = lm(data=data_filtered_2, formula9)

summary(model8)
summary(model9)