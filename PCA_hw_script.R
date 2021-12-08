library(readxl)
library(vegan)
library(dplyr)
library(ggplot2)
library(grid)
library(ggpubr)
library(factoextra)
library(knitr)
library(magrittr)
library(caret)
library(clusterSim)
clusterSim
norm2

setwd('.')

reader <-  function(f_ext, PATH) {
  lst_files <- list.files(PATH, pattern = paste('*.', f_ext, sep = ''))
  df <-  read.csv(paste(PATH, lst_files[1], sep = ''))
  for (i in lst_files[-1]) df <- merge(df, read.csv(paste(PATH, i, sep = '')))
  return(df)
}

df <- reader('csv', PATH= './/Data//')

df_ver1 <- dplyr::select(df, -c(material))

# colnames(df_ver1)

# ?createDataPartition

#
train_size <- sample(seq_len(nrow(df_ver1)), size = floor(0.75 * nrow(df_ver1)))  

train_data <- df_ver1[train_size, ] 
test_data <- df_ver1[-train_size, ]
#

#train_data 

### 11111 var

## find mean and sd column-wise of training data
train_data_mean <- apply(train_data,2,mean)
train_data_sd <- apply(train_data,2,sd)

## centered
?sweep
sweep(train_data, 2L, train_data_mean) # using the default "-" to subtract mean column-wise   
## centered AND scaled
norm2.test_data <- sweep(sweep(test_data, 2L, train_data_mean), 2, train_data_sd, "/")

### 22222 var

#normParam <- preProcess(train_data)
#norm.test_data <- predict(normParam, test_data)

### 33333 var

glimpse(train_data)

preproc1 <- preProcess(train_data[,c(2:168)], method=c("range"))

norm_one.train_data <- predict(preproc1, train_data[,c(2:168)])

glimpse(norm_one.train_data)

# sec
preproc2 <- preProcess(test_data[,c(2:168)], method=c("range"))

norm_one.test_data <- predict(preproc1, test_data[,c(2:168)])

glimpse(norm_one.test_data)

### 44444 var

#norm_two.test_data <- as.data.frame(scale(train_data[,c(2:168)]))

#glimpse(norm_two.test_data)

###

nwdf = cbind(train_data$critical_temp, norm_one.train_data)


model <- lm(nwdf$`train_data$critical_temp`~., data = nwdf)
summary(model)

###

pca <- prcomp(norm_one.train_data)

pcr_tot <- pca$x
#pca2 <- pca$x[,2]
#pca3 <- pca$x[,3]
#pca4 <- pca$x[,4]
#pca5 <- pca$x[,5]
#pca6 <- pca$x[,6]

pca_total <- data.frame(cbind(train_data$critical_temp, pcr_tot))

model_pca <- lm(pca_total$V1 ~., data = pca_total)
# pca1 + pca2 + pca3 + pca4 + pca5 + pca6
summary(model_pca)

### w/o standard#######################################################

model_wos <- lm(train_data$critical_temp~., data = train_data)
summary(model_wos)

### w/o standard

train_data_wotv <- subset(train_data, select = -c(1))

pca_wos <- prcomp(train_data_wotv)

summary(pca_wos)

pca11 <- pca$x
#pca22 <- pca$x[,2]
#pca33 <- pca$x[,3]
#pca44 <- pca$x[,4]
#pca55 <- pca$x[,5]
#pca66 <- pca$x[,6]

pca_total_wos <- data.frame(cbind(train_data$critical_temp, pca11, pca22, pca33, pca44, pca55, pca66))

model_pca_wos <- lm(scale(pca_total_wos$V1)~., data = pca_total_wos)

summary(model_pca_wos)

###############################################################################

train_data.tv <- train_data$critical_temp
train_data$critical_temp <- NULL

train_data.norm <- data.Normalization(train_data, type="n1", normalization="column")
train_data.tv.norm <- data.Normalization(train_data.tv, type="n1", normalization="column")

#model_new <- lm(train_data.norm$~., data = nwdf)

train_data.norm <- dplyr::select(train_data.norm,-c(He, Ne, Ar, Kr, Xe, Pm, Po, At, Rn))

train_data.pca <- prcomp(train_data.norm, center=TRUE, scale.=TRUE)

pcs <- as.data.frame(train_data.pca$x)

train_data.pca_total <- cbind(train_data.tv.norm, pcs)

plot(train_data.tv.norm, pcs$PC1)

lmodel <- lm(train_data.tv.norm ~ ., data = train_data.pca_total)
summary(lmodel)
###
pca11 <- train_data.pca$x[,1]
pca22 <- train_data.pca$x[,2]
pca33 <- train_data.pca$x[,3]
pca44 <- train_data.pca$x[,4]
pca55 <- train_data.pca$x[,5]
pca66 <- train_data.pca$x[,6]
train_data.pca_total_cut <- data.frame(cbind(train_data.tv.norm, pca11, pca22, pca33, pca44, pca55, pca66))
lmodel_cut <- lm(train_data.tv.norm ~ ., data = train_data.pca_total_cut)
summary(lmodel_cut)
