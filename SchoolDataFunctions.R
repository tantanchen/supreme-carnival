#REMEMBER TO SET WORKING DIRECTORY
setwd('Gtown Money')

 #cleaning
  library(readxl)
  library(tools)
#melting
  library(reshape2)
#plotting
  library(ggplot2)

#Generate years
func.genYears <- function(yearData) {
  years = c()
  j = 1
  for(i in yearData){
    if(substr(i,1,1) == "9") {
      years[j] = paste0(19,i)
    } else {
      years[j] = paste0(20,i)
    }
    j <- j+1
  }
  
  #Return
  years
}

#Function to get data only for Brown County
func.getBC <- function(file) {
 
  
  #process data
  pre <- read_xlsx(file)
  
  #label for dependent variable
  label <- file_path_sans_ext(substring(file,6))

  #finishing processing
  pre <- pre[ which(pre$COUNTY == 'Brown'), ]
  drop <- names(pre) %in% c('COUNTY','IRN',"FY12")
  to_func <- pre[!drop]
  
  data <- func.formatBC(to_func)
  colnames(data) <- c('School',label,'Year')
  
  #plot
  func.plot(data)
  
  #change wd for subsequent iterations
  setwd('..')

  #Return
  data
}

#Get data in correct format
func.formatBC <- function(data) {
 
  
  #Melt
  bc_melt <- melt(data, id=c('DISTRICT'))
  
  #Sort
  colnames(bc_melt) <- c('School','FY','Spending')
  bc_data <- data.frame(lapply(bc_melt[with(bc_melt,order(School)),],gsub,pattern='FY',replacement=''))
  
  #Generate years and drop FY
  bc_data$Year <- func.genYears(bc_data$FY)
  drop <- names(bc_data) %in% c('FY')
  bc_data <- bc_data[!drop]
  
  #Ensure variable is continuous
  bc_data$Spending <- as.numeric(levels(bc_data$Spending))[bc_data$Spending]
  
  #Return
  bc_data
}

#Line grpah of spending per school by year
func.plot <- function(data, ...) {
  
  
  #set wd to save graphs to "Charts" folder
  setwd('Charts')
  
  #ymax
  ymax <- round(max(data[2])+50, digits=-2)

  #for saving file
  file <- paste0(names(data)[2],'.png')
  png(filename=file)

  #plot
  print(ggplot(data, aes_string('Year', names(data)[2])) + geom_line(aes(colour=School, group=School)) +
    geom_point() + ylim(0,ymax) + xlab("Fiscal Year") +
    theme(axis.text.x = element_text(angle=45, hjust=1)))
  
  dev.off()
}

#Iterate through files
files <- list.files(path="Data", pattern="*.xlsx", full.names = TRUE, recursive=FALSE)
for(file in files) {
  print(file)
  func.getBC(file)
}
