
setwd('C:/Users/Psychiatry Lab/Documents/Biobank/data')
library(plyr)

################################################################################
#ATTENTION!!!!!!!!!!!!!!!!!!!!!!
#BEFORE YOU RUN THIS, MAKE SURE TO DELETE ALL ROWS BEFORE THE VARIABLE NAME ROW
################################################################################

#Put data sheet names here, without .csv in the name
datasheets <- scan(what='character')
joined_data_export_10042017     
data_export_12_13_2017_12_34_09  

#Read each file into a data frame with the same name as the .csv file
for (i in datasheets)
{
	assign(
		i, read.csv(paste(i,'.csv',sep=''), header=T, na.strings=c('999','NA'), stringsAsFactors=F)
		#i, read.csv(paste(i,'.csv',sep=''), header=T, na.strings=c('999','NA'),skip=1, stringsAsFactors=F)
	) 
}

#Parse the text list of data frame names as a list of data frames
data_list <- eval( 
			parse( 
				text=paste(
					"list(", paste(datasheets, collapse=','), ")" 
					)
				)
			)

            
#Combine all data frames
dat <- rbind.fill(data_list)

write.csv(dat,'joined_data_export_20171218.csv',row.names=F)
