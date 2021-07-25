#2021.07.25 sun #Team_Complete

#00.
library(tibble)
library(readxl)

#01.
weather_raw <- read_excel("c:/R_project/Team_Complete/data/weather1819.xlsx") %>% as_tibble()
weather <- weather_raw

#02.
for(i in 2:13){
  weather[[i]] <- scale(weather[[i]])
}

#03. 
weather_pca <- prcomp(weather[,-1])

#04.
summary(weather_pca)

#05. 
screeplot(weather_pca,type="lines", main="scree plot")

#06. 
biplot(weather_pca)

#07.
weather_pca_data <- cbind(weather[,1],weather_pca$x[,1:2])

#08.
write.csv(weather_pca_data, "c:/R_project/Team_Complete/data/weather_final_pca.csv")

#09.
weather_pca$rotation
