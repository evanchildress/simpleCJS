simNum <- 1
rDataName <- 'dMDataOutbkt2002_2014.RData'
dataStore<-getwd()

##########
comment<-paste(" model",simNum, " MS Sim "  )

home <- getwd()

#setwd( home )
directory <- tempfile( pattern="output-", tmpdir ='.', fileext='-simpleCJS')
dir.create(directory)

bugsName <- paste0('./simpleCJS','.txt')

file.copy(from='./callSimpleCJS.R', to=paste(directory,'callSimpleCJS.R',sep='/'))
file.copy(from=bugsName , to=paste(directory,bugsName ,sep='/'))
#file.copy(from='./analyzeSimpleCJS.R', to=paste(directory,'analyzeSimpleCJS.R',sep='/'))
file.copy(from='./run.R', to=paste(directory,'run.R',sep='/'))
##################
#file.copy(from=paste(rDataName,sep=''), to=paste(directory,rDataName,sep='/'))
##################

fileOutName <- "outMSRiver.RData" 

#load(paste(home,'/',rDataName,simNum,'.RData',sep=''))

####################
load(file.path(dataStore,rDataName))
#load('/home/ben/allSpp/fdDATA.RData')
####################
#print(parms)

writeLines(text = '')
cat("Working directory for this run is: ", directory, "\n", sep = '')
cat("Output filename for this run is: ", fileOutName, "\n", sep = '')
cat("Comment: '", comment, "\n", sep = '')
writeLines(text = '')

source('./callSimpleCJS.R')

#writeLines(text=paste(date(),directory,afterAdapt - beforeAdapt,done - beforeJags), con='../latest_directory')
writeLines(text=paste(date(),directory,afterAdapt - beforeAdapt,done - beforeJags,"[", comment,"]"), con='./info.txt')
getwd()

save(d, out, file = fileOutName)

################################################################
