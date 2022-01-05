
#setwd("C:/Users/user/Desktop/WorkingDIR")
#getwd()

##set the personal library
#.libPaths("C:/Users/user/Desktop/WorkingDIR/MyLibrary")
#.libPaths() ## Press tab for options

# Install packages
#install.packages("raster")
#install.packages("tree")
#install.packages("rgeos")
#install.packages("rgdal")
#install.packages("maptools")


#read package
library(raster)
library(tree)
library(rgeos)
library(rgdal)
library(maptools)

# install and read maultiple packages code
#packages <- c("rgeos", "rgdal", "maptools")

#ipak <- function(pkg){
#  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#  if (length(new.pkg)) 
#    install.packages(new.pkg, dependencies = TRUE)
#  sapply(pkg, require, character.only = TRUE)
#}

#ipak(packages)


# Read Training data 1st technique: prepare data in R environment

# load all the data
setwd("C:/Users/sebas/Universidad/Seminario I/Proyecto final")

Firms2018 = raster("Dec2018Firms.tif")
Temperatura2018= raster("Dec2018Temperatura.tif")
Nvdi2018= raster("Dec2018NVDI.tif")
#Elevacion2018= raster("Elevation.tif")

#BAI1 <- setValues(BAI, 1:ncell(BAI))
#hist(BAI1)
################Gráficas#################### 

plot(Firms2018,main="Incendios Diciembre 2018 Valparaíso")
plot(Temperatura2018,main="Temperatura Valparaiso diciembre 2018")
plot(Nvdi2018,main="Nvdi diciembre de 2018 en Valparaiso")
#plot(Elevacion2018,main="Elevacion Valparaiso año 2018")

#Al correr este códgo se dejan de ver bn las imágenes
Firms2018 <- setValues(Firms2018, 1:ncell(Firms2018))
hist(Firms2018)
summary(Firms2018)
head(Firms2018)

Temperatura2018 <- setValues(Temperatura2018, 1:ncell(Temperatura2018))
hist(Temperatura2018)
summary(Temperatura2018)
head(Temperatura2018)

Nvdi2018 <- setValues(Nvdi2018, 1:ncell(Nvdi2018))
hist(Nvdi2018)
summary(Nvdi2018)
head(Nvdi2018)

#Elevacion2018 <- setValues(Elevacion2018, 1:ncell(Elevacion2018))
#hist(Elevacion2018)
#summary(Elevacion2018)
#head(Elevacion2018)

#Training_LR= raster("burndateT0102.tif")


############################################################
# if you have diffrent extent, then try to Resample them, correr 1 a 1 
Firms2018_re <- resample(Firms2018,Nvdi2018, resample='bilinear') 
Temperatura2018_re<- resample(Temperatura2018,Nvdi2018, resample='bilinear')

# create resampled Rasters
dir.create("C:/Users/sebas/Universidad/Seminario I/Proyecto final/Resample")  

writeRaster(Firms2018_re,"C:/Users/sebas/Universidad/Seminario I/Proyecto final/Resample/Firms2018_re.tif", overwrite=TRUE)
writeRaster(Temperatura2018_re,"C:/Users/sebas/Universidad/Seminario I/Proyecto final/Resample/Temperatura2018_re.tif", overwrite=TRUE)
writeRaster(Nvdi2018,"C:/Users/sebas/Universidad/Seminario I/Proyecto final/Resample/Nvdi2018.tif", overwrite=TRUE)

#Reading data
list.files("C:/Users/sebas/Universidad/Seminario I/Proyecto final/Resample/")
Training=raster("C:/Users/sebas/Universidad/Seminario I/Proyecto final/Resample/Firms2018_re.tif")
hist(Training)  #Plot histogram to see the shape of data distribution 
#plot(Training, add=TRUE)

summary(Firms2018_re)
plot(Firms2018_re)

## stack multiple raster files
OHRasters_List= list.files(path = "C:/Users/sebas/Universidad/Seminario I/Proyecto final/Resample/",pattern = "tif$", full.names = TRUE)

Rasters_stacked=stack(OHRasters_List) ################hasta aquì
names(Rasters_stacked)
head(Rasters_stacked)   ## our stack variable, we see NA values, #### 
summary(Rasters_stacked)

##that need to be removed or replaced by -9999###

value_table=getValues(Rasters_stacked)
head(value_table, n=10)


###Ahora hay que cargar las variables re sample
setwd("C:/Users/sebas/Universidad/Seminario I/Proyecto final/Resample")

Firms2018_re = raster("Firms2018_re.tif")
Temperatura2018_re= raster("Temperatura2018_re.tif")

#value_table=na.omit(value_table)#####PARA NOSOTROS LOS NA SON CEROS
#####PARA NOSOTROS LOS NA SON CEROS, por lo que hay que reemplazar valores
#Generando la función:
replace_missings <- function(x, replacement) {
  is_miss <- is.na(x)
  x[is_miss] <- replacement
  
  message(sum(is_miss), " missings replaced by the value ", replacement)
  x
}
Firms2018_reSNA<-replace_missings(Firms2018_re, replacement = 0)
summary(Firms2018_reSNA)

#Convirtiendo los valores mayores a 1 en 1 para BAI

Firms2018_reSNA[Firms2018_reSNA > 0] <- 1
Firms2018_reSNA

#Quitando NA en Temperatura:

Temperatura2018_reSNA<-replace_missings(Temperatura2018_re, replacement = 0)
summary(Temperatura2018_reSNA)

#Convirtiendo los valores mayores a 1 en 1 para FIRMS

Temperatura2018_reSNA[Temperatura2018_reSNA > 0] <- 1
Temperatura2018_reSNA

#Quitando NA en el resto de las variables:

Nvdi2018SNA<-replace_missings(Nvdi2018, replacement = 0)
summary(Nvdi2018SNA)

##Guardando los nuevos rasters##
dir.create("C:/Users/sebas/Universidad/Seminario I/Proyecto final/ResampleSna")  

writeRaster(Firms2018_reSNA,"C:/Users/sebas/Universidad/Seminario I/Proyecto final/ResampleSna/Firms2018_reSNA.tif", overwrite=TRUE)
writeRaster(Temperatura2018_reSNA,"C:/Users/sebas/Universidad/Seminario I/Proyecto final/ResampleSna/Temperatura2018_reSNA.tif", overwrite=TRUE)
writeRaster(Nvdi2018SNA,"C:/Users/sebas/Universidad/Seminario I/Proyecto final/ResampleSna/Nvdi2018SNA.tif", overwrite=TRUE)

OHRasters_ListSNA= list.files(path = "C:/Users/sebas/Universidad/Seminario I/Proyecto final/ResampleSna",pattern = "tif$", full.names = TRUE)
Rasters_stackedSNA=stack(OHRasters_ListSNA) 
names(Rasters_stackedSNA)
head(Rasters_stackedSNA)  
summary(Rasters_stackedSNA)

memory.limit(size=80000000)##Aumentar la ram utilizada

value_tableSNA=getValues(Rasters_stackedSNA)
head(value_tableSNA, n=10)
value_tableSNA=as.data.frame(Rasters_stackedSNA)
attach(value_tableSNA)

Training_data=value_tableSNA
colnames(Training_data)[colnames(Training_data) == 'Firms2018_reSNA'] <- 'Training_points' #chage column name
attach(Training_data)
#head(Training_points)

Training_data=as.data.frame(Training_data)
head(Training_data)


##################################################
############ARBOL DE DECISIÓN##########
################################################

dim(Training_data)
summary(Training_data)
str(Training_data)


# Now let us run the model
colnames(Training_data)<-c("Training_points","NDVI","Temp")
head(Training_data)
attach(Training_data)
#summary(Reflactancia)

DTree_model1=tree(as.factor(Training_points) ~ .,Training_data)# using all the 5 variables of 150 observations # # adding as.factor(x), model deals with factor not numeric
summary(DTree_model1)
plot(DTree_model1)
text(DTree_model1, pretty=0)

# check the model performance
Performance_predict=predict(DTree_model1,Training_data, type ="class")
mean(Performance_predict!=Training_data$Training_points)
mean(Performance_predict==Training_data$Training_points)

#table(Training_data[,5],Performance_predict) # check the prediction accuracy, 
# 73 out of 75 for (0) and 63 out of 75 for (1) columns=prediction value



# check model accuracy 
Accuracy_predict=predict(DTree_model1,Testing_data) ### SHOULD USE SAME COULUMN EXCEPT TESTING DATA COL
mean(Accuracy_predict!=Testing_data$Testing_points) # Mean= not classified/classified
mean(Accuracy_predict==Testing_data$Testing_points) # Mean= not classified/classified

## now to start with pruning
# we need to do cross validation to check howmany levels we have to prune
cv_tree=cv.tree(DTree_model1, FUN = prune.misclass)
names(cv_tree) # give the size of tree
plot(cv_tree$size,
     cv_tree$dev,
     type="b") # x=size, y=dev

# now we should prune the tree at the lowest value of Dev (lowest error, i.e above of that is causing the error)
### pruning the tree
pruned_model=prune.misclass(DTree_model1, best=5)
plot(pruned_model)
text(pruned_model, pretty=0)


# check the model performance
tree_predict_PP=predict(pruned_model,Training_data, type ="class")
mean(tree_predict_PP!=Training_data$Training_points)
table(Training_data[,5],tree_predict_PP) # check the prediction accuracy, 
# 73 out of 75 for (0) and 63 out of 75 for (1) columns=prediction value

# check model accuracy 
Accuracy_predict_P=predict(pruned_model,Testing_data) ### SHOULD USE SAME COULUMN EXCEPT TESTING DATA COL

mean(tree_predict_PP==Testing_data$Testing_points)

#### D O N E ####