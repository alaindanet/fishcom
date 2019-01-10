################################################################################
#              Compare my metaweb with the one of Bonnafe et al.               #
################################################################################

library(tidyverse)
library('magrittr')
library(data.table)
##############################
#  Comparision of raw files  #
##############################
#Done in clean_fish_op_data.R

## set working directory
setwd(getwd())

## load data and species list
# bib_data
ff <- read.table("../bonnafe_work/data/bib_data/F-F.csv",sep=";",header=T)
odsF <- as.matrix(read.table("../bonnafe_work/data/bib_data/ODSF.csv",sep=";",header=T))
odsR <- as.matrix(read.table("../bonnafe_work/data/bib_data/ODSR.csv",sep=";",header=T))
### The same, see clean_pred_ods_data.R

## raw_data
fish_length <- fread("../bonnafe_work/data/raw_data/data_clean.txt",
  select = c("Species", "Length")) %>%
  as.tibble()
colnames(fish_length) <- str_to_lower(colnames(fish_length))
#fish_length <- read.table("../bonnafe_work/data/raw_data/data_clean.txt", sep="\t", header=T)
#data(fish_length)
size_quantiles <- read.table("../bonnafe_work/data/bib_data/size_quantiles.csv",sep=";",header=T)
### The same, see clean_fish_op_data.R

## global variables
# species
speciesVect <- as.vector(unique(ff[,"species_code"]))
# resources
resourceList <- odsR[,"species_code"]

## replace max in ODS table by maximum observed size in raw data
odsF[odsF[,"size_max"]=="max","size_max"] <- size_quantiles[pmatch(paste(odsF[odsF[,"size_max"]=="max","species_code"]),size_quantiles[,"X"]),"X100."]
#odsF[odsF[,"size_max"]=="max","size_max"] <- Inf

#####################################
## MATRIX CONSTRUCTION (OPTION 14) ##
#####################################

## quantile method + (xmax+xmin)/2 + (ymin+ymax)/2
# (i) split species in n quantiles
# (ii) construct matrix with n*52 columns (row and colnames labeled as speciesCode_maxLength)
# (iii) fill the matrix using ff file
# a2 = 0.03 and b2 = 0.455

## (i) split species in n quantiles

## define number of classes
nSizeClasses <- 9

## define species vector
speciesColVect <- unlist(lapply(speciesVect,FUN=rep,times=nSizeClasses))

## determine max size for each trophic species
sizeMaxVect <- NULL
i<-1
for(i in 1:length(speciesVect))
{
  quantile_local <- round(quantile(x=fish_length$length[fish_length$species==speciesVect[i]],probs=seq(0,1,1/nSizeClasses)))
  # quantile_local <- round(seq(0,max(fish_length$Length[fish_length$species==speciesVect[i]]),max(fish_length$Length[fish_length$species==speciesVect[i]])/nSizeClasses)) ## precentile version
  sizeMaxVect <- c(sizeMaxVect,quantile_local[-1]) # store upper quantiles = remove min
}

## create vect of min size
# shift sizeMaxVect with lag +1 => remove last value and add 1 at beginning
sizeMinVect <- c(1,sizeMaxVect[-length(sizeMaxVect)])
# replace by 1 min size of first stage of trophic species (which currently equals max size of lag-1 trophic species)
sizeMinVect[match(speciesVect,speciesColVect)] <- 1
# check
head(cbind(speciesColVect,sizeMinVect,sizeMaxVect))
tail(cbind(speciesColVect,sizeMinVect,sizeMaxVect))
# OK

## bind speciesVect, minSizevect, and maxSizeVect to create sizeClasses (= trophic species)
sizeClasses <- paste(speciesColVect,sizeMinVect,sizeMaxVect,sep="_")
length(sizeClasses) # 3*50=150 for 3 sizeClasses per species
# OK

## (ii) construct ff_matrix with 150+7=157 columns (row and colnames labeled as speciesCode_minLength_maxLength)
## initiate matrices
# ff matrix
ff_matrix <- matrix(rep(0,(length(sizeClasses))^2),ncol=length(sizeClasses))
rownames(ff_matrix) <- colnames(ff_matrix) <- sizeClasses
# fr matrix
fr_matrix <- matrix(rep(0,(length(sizeClasses)*length(resourceList))),ncol=length(sizeClasses))


## (iii) fill the fish-fish matrix using ff file
j<-7
for(j in 1:length(sizeClasses))
{
  ## output iterators
  print("=======================")
  print(j/length(sizeClasses)*100)
  
  ## get predator code
  predatorspecies <- unlist(strsplit(x=sizeClasses[j],"_"))[1]
  
  ## extract predation window parameters
  # a1 <- 0
  # a2 <- 0.03
  # b1 <- 0
  # b2 <- 0.455
  
  ## extract predation window parameters
  a1 <- ff$par_1_min[ff$species_code==predatorspecies]
  a2 <- ff$par_2_min[ff$species_code==predatorspecies]
  b1 <- ff$par_1_max[ff$species_code==predatorspecies]
  b2 <- ff$par_2_max[ff$species_code==predatorspecies]
  
  ## get min and max predator size 
  minPredatorSize <- as.numeric(unlist(strsplit(x=sizeClasses[j],"_"))[2])
  maxPredatorSize <- as.numeric(unlist(strsplit(x=sizeClasses[j],"_"))[3])
  
  ## calculate theoretical min and max prey size
  thMinPreySize <- a1 + a2*((minPredatorSize+maxPredatorSize)/2)
  thMaxPreySize <- b1 + b2*((minPredatorSize+maxPredatorSize)/2) 
  # prey range consummed by average sized individual of class
  
  ## get min and max prey size from all other trophic species
  minPreySizeVect <- matrix(as.numeric(unlist(strsplit(x=sizeClasses,"_"))),ncol=3,byrow=T)[,2]
  maxPreySizeVect <- matrix(as.numeric(unlist(strsplit(x=sizeClasses,"_"))),ncol=3,byrow=T)[,3]
  
  ## get the piscivory index of predator
  # check if predator has at least one piscivore stage
  sizeAtPiscivory <- NA
  if(sum((odsF[odsF[,"species_code"]==predatorspecies,"fish"]==1)*1)>0)
  {
    # we get size at switch to piscivory 
    sizeAtPiscivory <- as.numeric(odsF[odsF[,"species_code"]==predatorspecies & odsF[,"fish"]==1,"size_min"])[1]
    # AND compare it to min and max predator size
    piscivoryIndex <- (maxPredatorSize > sizeAtPiscivory)*1 # fonction porte
    
    ## update thMinPreySize if  minPredatorSize < sizeAtPiscivory <= maxPredatorSize
    # because only part of the class is piscivorous
    if(minPredatorSize < sizeAtPiscivory & sizeAtPiscivory <= maxPredatorSize)
    {
      # thMinPreySize <- a1 + a2*sizeAtPiscivory # minPreySize = smallest prey of smallest piscivorous predator of sizeClass
      # thMaxPreySize <- b1 + b2*sizeAtPiscivory # avoid overlap problem
      # sizeAtPisc is the size of smallest predator
      # to avoid overlap problem we calculate max prey according to that size
    }
    
  }else
  {
    piscivoryIndex <- 0
  }
  
  ## ff interactions
  ## determine whether the min, max prey size of trophic species are or around in the predation window
  # mean prey size in predation window
  ff_matrix[,j] <-  ((thMinPreySize <= ((minPreySizeVect+maxPreySizeVect)/2) & ((minPreySizeVect+maxPreySizeVect)/2) <= thMaxPreySize))*piscivoryIndex
  
  ## fr interactions
  ## ontogenetic feeding stage (OFS) subsetting
  # extract OFS in odsF if either min or max OFS size is contained in, or around of, ]minPredatorSize,maxPredatorSize] predator size
  # case 1 - OFS min size in ]minPredatorSize,maxPredatorSize]
  minOfsInSizeClass <- ((minPredatorSize < as.numeric(odsF[,"size_min"])) & (as.numeric(odsF[,"size_min"]) < maxPredatorSize))
  # case 2 - OFS max size in ]minPredatorSize,maxPredatorSize]
  maxOfsInSizeClass <- ((minPredatorSize < as.numeric(odsF[,"size_max"])) & (as.numeric(odsF[,"size_max"]) <= maxPredatorSize))
  # case 3 - OFS min and max size around ]minPredatorSize,maxPredatorSize]
  minMaxOfsAroundSizeClass <- ((as.numeric(odsF[,"size_min"]) < minPredatorSize) & (maxPredatorSize <= as.numeric(odsF[,"size_max"])))
  minMaxSizeClassAroundOfs <- ((as.numeric(odsF[,"size_min"]) > minPredatorSize) & (maxPredatorSize >= as.numeric(odsF[,"size_max"])))
  # sum subsetted rows
  fr_matrix[,j] <- (apply(matrix(as.numeric(odsF[odsF[,"species_code"] == predatorspecies & (minOfsInSizeClass | maxOfsInSizeClass | minMaxOfsAroundSizeClass | minMaxSizeClassAroundOfs), resourceList]),ncol=length(resourceList),byrow=F),2,FUN=sum)>0)*1
  
  ## checking
  print(paste("Predator:",sizeClasses[j],sep=" "))
  print(paste("SizeAtPisc:",sizeAtPiscivory,sep=" "))
  print(paste("Piscivory index:",piscivoryIndex,sep=" "))
  print(paste("minPreySize:",thMinPreySize,sep=" "))
  print(paste("maxPreySize:",thMaxPreySize,sep=" "))
  print("Preys:")
  print(head(ff_matrix[,j]))
  print("Resources:")
  print(odsF[odsF[,"species_code"] == predatorspecies & (minOfsInSizeClass | maxOfsInSizeClass | minMaxOfsAroundSizeClass), c("size_min","size_max",resourceList)])
  print(fr_matrix[,j])
}

## check
# ff_matrix[1:10,1:10]
# ff_matrix[140:150,140:150]
dim(ff_matrix)[1] == dim(ff_matrix)[2]

## (iv) the fish-resource resource-resource and resource fish matrices (fr_matrix and rr_matrix and rf_matrix)
# fr_matrix
colnames(fr_matrix) <- c(colnames(ff_matrix))
# rr_matrix
rr_matrix <- t(odsR[,resourceList])
colnames(rr_matrix) <- odsR[,"species_code"]
# rf_matrix
rf_matrix <- matrix(0,ncol=dim(rr_matrix)[2],nrow=dim(ff_matrix)[1])

## (v) combine the ff fr rr matrices
ff_fr_rf_rr_matrix <- rbind(cbind(ff_matrix,rf_matrix),cbind(fr_matrix,rr_matrix))
colnames(ff_fr_rf_rr_matrix) <- c(sizeClasses,paste(resourceList,"_0_0",sep="")) # to facilitate size handling
head(ff_fr_rf_rr_matrix)
# check that matrix is square
dim(ff_fr_rf_rr_matrix)[1]==dim(ff_fr_rf_rr_matrix)[2]

#
###

#################################
## TERMINATION

## write the matrix 
write.table(ff_fr_rf_rr_matrix,paste("./output/AccMat_","quant_",nSizeClasses,"_PWvar_partOverlap",".csv",sep=""),sep=";",row.names=FALSE,col.names=TRUE)
matrix_to_rep <- read_csv2("../bonnafe_work/data/bib_data/output/AccMat_quant_9_PWvar_partOverlap-alain.csv") %>% as.matrix
new_mat  <- sapply(ff_fr_rf_rr_matrix, as.numeric)
names(new_mat) <- NULL 
all.equal(new_mat, as.numeric(matrix_to_rep))

###############################################################
#  Test differences between my metaweb and the one of Willem  #
###############################################################

# Mat generated with modification in Fish resources rules
mat_alain <- read_csv2(paste("./output/AccMat_","quant_9_PWvar_partOverlap","-alain.csv",sep=""))
col_to_rm <- str_detect(colnames(mat_alain), "OBL")
mat_alain_2 <- mat_alain[- which(col_to_rm), - which(col_to_rm)] %>% as.matrix(.)
## Generate my metaweb:
data(fish_length)
data(fish_diet_shift)
data(resource_diet_shift)
data(pred_win)
metaweb <- build_metaweb(fish_length, species, length, pred_win,
  fish_diet_shift, size_min, size_max, fish,
  resource_diet_shift, class_method = "quantile",
  nb_class = 9, pred_win_method = "midpoint", fish_resource_method = "willem", na.rm = TRUE, replace_min_by_one = TRUE)
filter(metaweb$size_class, species == "BRO")
colnames(metaweb$metaweb)
colnames(mat_alain_2)
# Order matrix:
order_species_to_rep <- str_extract_all(colnames(mat_alain_2), "[A-Za-z]+", simplify = TRUE) %>% as.vector
col_species <- str_extract_all(colnames(metaweb$metaweb), "[A-Za-z]+", simplify = TRUE) %>% as.vector
metaweb2 <- metaweb$metaweb[order(match(col_species, order_species_to_rep)), order(match(col_species, order_species_to_rep))]
colnames(mat_alain_2) <- colnames(metaweb2)
rownames(mat_alain_2) <- colnames(metaweb2)

all.equal(metaweb2, mat_alain_2)
test <- ifelse(metaweb2 == mat_alain_2, 0, 1)

length(metaweb2[which(test ==1)])
length(metaweb2[which(test ==1)]) / length(metaweb2) * 100

get_error <- function(x){
  row_position <- which(test==1, arr.ind = TRUE)[x, 1]
  col_position <- which(test==1, arr.ind = TRUE)[x, 2]

  # Predator and prey
  me <- metaweb2[row_position, col_position]
  predator <- colnames(metaweb2)[col_position]
  prey <- rownames(metaweb2)[row_position]
  if (me) {
    cat(predator, "eats", prey, "in my metaweb but not in willem's one", "\n")
  } else {
    cat(predator, "does not eat", prey, "in my metaweb but does in willem's one", "\n")
  }
}

for (i in 1:4) {
  get_error(i)
}

filter(metaweb$size_class, species %in% c("BRO", "ANG"), class_id ==1)
filter(fish_diet_shift, species %in% c("BRO", "ANG"))
filter(metaweb$th_prey_size, species %in% c("BRO", "ANG"), class_id ==1)
filter(metaweb$piscivory_index, species %in% c("BRO", "ANG"), class_id ==1)

metaweb$metaweb["zoopl", "BRO_1"]
mat_alain[, "BRO_1_104"]

## write the matrix for nw
# remove size min in size class
colnames(ff_fr_rf_rr_matrix) <- rownames(ff_fr_rf_rr_matrix) <- paste(matrix(unlist(strsplit(x=colnames(ff_fr_rf_rr_matrix),split = "_")),ncol=3,byrow=T)[,1],matrix(unlist(strsplit(x=colnames(ff_fr_rf_rr_matrix),split = "_")),ncol=3,byrow=T)[,3],sep="_") # c(paste(odsF[,"species_code"],odsF[,"stage"],sep="_"),odsR[,"species_code"]) # for stage
# write n trophic species at beginning of file
write(nrow(ff_fr_rf_rr_matrix),paste("./output/AccMat_","quant_",nSizeClasses,"_PWvar_partOverlap",".txt",sep=""))
write.table(ff_fr_rf_rr_matrix,paste("./output/AccMat_","quant_",nSizeClasses,"_PWvar_partOverlap",".txt",sep=""),sep="\t",row.names=TRUE,col.names=FALSE,quote=FALSE,append=TRUE)
