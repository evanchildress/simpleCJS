parallel=T

require(snow)
library(getWBData)
library(R2jags)
library(lubridate)
library(reshape2)
library(data.table)
simNum <- 1

source("constructCJSwAbundance.R")



modelFile<-'~/simpleCJS/model.txt'
##################

source('./callCjsExtreme.R')

#writeLines(text=paste(date(),directory,afterAdapt - beforeAdapt,done - beforeJags), con='../latest_directory')
#writeLines(text=paste(date(),directory,afterAdapt - beforeAdapt,done - beforeJags,"[", comment,"]"), con='./info.txt')
#getwd()

#save(d, out, file = fileOutName)
directory <- tempfile( pattern="output-", tmpdir ='.', fileext='-Cjs')
dir.create(directory)
fileOutName<-"cjsMcmc.rds"
saveRDS(out,file=file.path(directory,fileOutName))
saveRDS(out,file="results/bntYoyCjsMcmc.rds")