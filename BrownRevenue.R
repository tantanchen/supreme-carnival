library(ggplot2)
library(readxl)
library(plyr)


PerPupilLocalRevenue <- read_xlsx(path = "PerPupilLocalRevenue.xlsx", 
                                  sheet = 1 , 
                                  col_types = c(rep("text",3), rep("numeric", 18)))
PerPupilFederalRevenue <- read_xlsx("PerPupilFederalRevenue.xlsx", 
                                    col_types = c(rep("text",3), rep("numeric", 18)))

LocalRevenue2012 <- subset(PerPupilLocalRevenue, select = c("IRN", "DISTRICT", "COUNTY", "FY12"))
LocalRevenue2012 <- subset(LocalRevenue2012, COUNTY == "Brown")

FederalRevenue2012 <- subset(PerPupilFederalRevenue, select = c("IRN", "DISTRICT", "COUNTY", "FY12"))
FederalRevenue2012 <- subset(FederalRevenue2012, COUNTY == "Brown")



LocalRevenue2012$Type <- "Local"
FederalRevenue2012$Type <- "Federal"

CombinedData <- rbind(LocalRevenue2012, FederalRevenue2012)

County2012 <- ddply(CombinedData, 
                        c("DISTRICT", "Type"), 
                        summarize,
                        FY12 = sum(FY12, na.rm = TRUE))

print(ggplot(County2012, aes(x = Type, y = FY12, fill = DISTRICT)) + 
                         geom_bar(stat = "identity") + coord_flip())
