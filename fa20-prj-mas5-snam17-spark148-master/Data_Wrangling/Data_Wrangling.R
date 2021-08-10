library(tidyverse)
library(XML)
library(readxl)
library(rvest)
#Helper functions
char_numeric <- function(dat,from,to,target){
  sapply(target, function(i) as.numeric(str_replace_all(dat[,i],from,to)))
}

# Topic 1: Early Vote data in Texas by County in 2020 Presidential Election
## 1.1 Early Vote data by Year: 1996 to 2020
path <- paste0("./Raw_data/1.Early_Vote/Early_Vote_By_Year/",list.files("./Raw_data/1.Early_Vote/Early_Vote_By_Year"))
html_files <- lapply(path, function(i) readLines(i, encoding="UTF-8"))
EV_tables <- lapply(html_files,readHTMLTable, header=TRUE)
# Remove Total number in each dataset
for(i in 1:length(EV_tables)){
  if(i != 6){
    EV_tables[[i]][[1]] <- (EV_tables[[i]][[1]])[1:15,]
  }
  EV_tables[[i]][[1]] <- (EV_tables[[i]][[1]])
}
EV_tables[[5]][[1]] <- (EV_tables[[5]][[1]])[,-7] #remove unnecessary column: 
EV_tables[[6]][[1]] <- (EV_tables[[6]][[1]])[,-ncol(EV_tables[[6]][[1]])] #remove unnecessary column
##Integrate Early Vote data
EV_Total <- NULL
for(i in 1:length(EV_tables)){
  cur_dat <- EV_tables[[i]][[1]]
  cur_dat[,c(2,3,4,6,7)] <- char_numeric(cur_dat,",","",c(2,3,4,6,7))
  cur_dat[,c(5,8)] <-  char_numeric(cur_dat,"%","",c(5,8))
  cur_dat$year <- c(seq(from=2000,to=2020,by=4),1996)[i]
  cur_dat <- cur_dat[,-3] # number of votes on the last date.
  names(cur_dat) <- c("County","Registered_Voters","Cumulative_In_Person","Cumulative_In_Person_Percent","Cumulative_By_Mail",
                      "Cumulative_In_Person_By_Mail","Cumulative_Percent_Early_Vote","Year")
  cur_dat$"Cumulative_By_Mail_Percent" <- round(cur_dat$"Cumulative_By_Mail" / cur_dat$"Registered_Voters" * 100,2)
  cur_dat <- cur_dat[,c(1,2,3,4,5,9,6,7,8)]
  if(i == 1){
    EV_Total = cur_dat
  }
  else{
    EV_Total <- rbind(EV_Total, cur_dat)
  }
}
EV_Total <- EV_Total %>% arrange(Year) %>% drop_na()
#Voter Registration by Year from 1996 to 2020
path <- paste0("./Raw_data/5.Voter_Registration/",list.files("./Raw_data/5.Voter_Registration/"))
html_files <- lapply(path, function(i) readLines(i, encoding="UTF-8"))
VR_tables <- lapply(html_files,readHTMLTable, header=TRUE)
# Remove Total number in each dataset
for(i in 1:length(VR_tables)){
  VR_tables[[i]][[1]] <- (VR_tables[[i]][[1]])[-nrow(VR_tables[[i]][[1]]),]
}
VR_tables[[1]][[1]] <- (VR_tables[[1]][[1]])[,-2] #remove unnecessary column: 
VR_tables[[7]][[1]] <- (VR_tables[[7]][[1]])[,-2] #remove unnecessary column
VR_tables[[1]][[1]] <- (VR_tables[[1]][[1]])[,c(1,2,5,3,4)] #reorder columns
VR_tables[[2]][[1]] <- (VR_tables[[2]][[1]])[,c(1,2,5,3,4)] #reorder columns
VR_tables[[7]][[1]] <- (VR_tables[[7]][[1]])[,c(1,2,5,3,4)] #reorder columns
VR_Total <- NULL
for(i in 1:length(VR_tables)){
  cur_dat <- VR_tables[[i]][[1]]
  cur_dat[,c(3,4,5)] <- char_numeric(cur_dat,",","",c(3,4,5))
  cur_dat$year <- c(seq(from=2000,to=2020,by=4),1996)[i]
  names(cur_dat) <- c("County","Precints","Total_Voters","Suspense_Voters","Non_Suspense_Voters","Year")
  if(i == 1){
    VR_Total = cur_dat
  }
  else{
    VR_Total <- rbind(VR_Total, cur_dat)
  }
}
VR_Total <- VR_Total %>% arrange(Year) %>% drop_na()  
# Combining Early Vote + Voter Registration
EV_Total$County <- tolower(EV_Total$County)
VR_Total$County <- tolower(VR_Total$County)
VR_Total[1710,]$County <- "san augustine"
dat_final <- left_join(EV_Total,VR_Total,by=c("County","Year"))
dat_final <- dat_final[,-2]
dat_final$Precints <- as.character(dat_final$Precints)
dat_final$Precints <- as.numeric(str_replace_all(dat_final$Precints,",",""))
names(dat_final)[10] <- "Registered_Voters"
dat_final <- dat_final[,c(1,9:12,2:8)]
dat_final$County <- str_to_title(dat_final$County)
dat_final$Cumulative_In_Person_Percent <- round(dat_final$Cumulative_In_Person/dat_final$Non_Suspense_Voters * 100,2)
dat_final$Cumulative_By_Mail_Percent <- round(dat_final$Cumulative_By_Mail/dat_final$Non_Suspense_Voters * 100,2)
dat_final$Cumulative_Percent_Early_Vote <- round((dat_final$Cumulative_In_Person+dat_final$Cumulative_By_Mail)/dat_final$Non_Suspense_Voters * 100,2)
write_csv(dat_final,"./Final_data/1.Early_Vote_Data.csv") 


## 1.2 Early Vote by Day in 2020
path <- paste0("./Raw_data/1.Early_Vote/Early_Vote_By_Day/",list.files("./Raw_data/1.Early_Vote/Early_Vote_By_Day"))
html_files <- lapply(path, function(i) readLines(i, encoding="UTF-8"))
EV_tables <- lapply(html_files,readHTMLTable, header=TRUE)
# Remove Total number in each dataset
for(i in 1:length(EV_tables)){
  EV_tables[[i]][[1]] <- (EV_tables[[i]][[1]])[,c(1,2,3,6)] # remove total and last column
  names(EV_tables[[i]][[1]]) <- c("County","Registered_Voters","In_person","Cumulative_Mail")
}
period <- seq.Date(as.Date("2020-10-04"),as.Date("2020-10-30"),by=1)
cur_dat <- EV_tables[[1]][[1]]
cur_dat[,c(2,3,4)] <- char_numeric(cur_dat,",","",c(2,3,4))
cur_dat$date <- period[1]
names(cur_dat)[4] <- "Mail"
EV_Total <- EV_tables[[1]][[1]] <- cur_dat
for(i in 2:length(EV_tables)){
  cur_dat <- EV_tables[[i]][[1]]
  cur_dat[,c(2,3,4)] <- char_numeric(cur_dat,",","",c(2,3,4))
  cur_dat[,4] <- cur_dat[,4] - c(char_numeric((EV_tables[[(i-1)]][[1]]),",","",4))
  names(cur_dat)[4] <- "Mail"
  cur_dat$date <- period[i]
  EV_Total <- rbind(EV_Total, cur_dat)
}
VR_Total$County <- str_to_title(VR_Total$County)
VR_2020 <- VR_Total %>% filter(Year==2020) %>% select(1,5)
EV_copy <- EV_Total
EV_copy$County <- str_to_title(as.character(EV_copy$County))
EV_VR <- left_join(EV_copy,VR_2020,by="County")
EV_Total <- (EV_VR[,-c(2)])[,c(4,1,5,2,3)]
EV_Total <- EV_Total%>% drop_na() %>% mutate("In_Person_Percentage"=round((In_person / Non_Suspense_Voters * 100),2),
                                             "Mail_Percentage"=round((Mail / Non_Suspense_Voters * 100),2))
EV_Total <- EV_Total[,c(1,2,3,4,6,5,7)]
write_csv(EV_Total,"./Final_data/1.Early_Vote_Data_By_Day.csv")

# Topic 2. Result of election in TX from 1976 - 2016
General_election_data <- as.tibble(read.csv("./Raw_data/2.General_Election_Data/1976_2016_president_election_by_state.csv"))
General_Summary <- General_election_data %>% filter(state_po == "TX") %>% group_by(year) %>% 
  filter(candidatevotes == max(candidatevotes) | candidatevotes == sort(candidatevotes,decreasing=TRUE)[2]) %>% select(-c(2,4,5,6,7,10,13,14))
General_Summary$Result <- c("winner","runners-up") 
General_Summary <- General_Summary %>% select(-c(2))
write_csv(General_Summary,"./Final_data/2.General_Election_Data.csv")

# Topic 3. Demographic data in Texas by County

## 3.1 2018
dat <- read.csv("./Raw_data/3.Demographic/cc-est2019-alldata-48.csv")
dat$CTYNAME <- sub(" .*", "", as.character((dat$CTYNAME))) #remove "County" in the CTYNAME (County Name) column
dat_11 <- dat %>% filter(YEAR == 11) %>% select(-c(1,2,3,4,6)) %>% group_by(CTYNAME) %>% filter(!(AGEGRP %in% c(1,2,3,4)))# 2018 population data
dat_11_sub <- dat_11 %>%  filter(AGEGRP !=0) %>% select(1:17,28,29,52,53)
dat_11_total <- dat_11_sub %>% select(-c(2)) %>% group_by(CTYNAME) %>% summarise_all(.funs=sum)
write_csv(dat_11_total,"./Final_data/3.2018_Demographic_Data_By_County_Total.csv")
write_csv(dat_11_sub,"./Final_data/3.2018_Demographic_Data_By_County.csv")
## 3.2 2020
dat_12 <- dat %>% filter(YEAR == 12) %>% select(-c(1,2,3,4,6)) %>% group_by(CTYNAME) %>% filter(!(AGEGRP %in% c(1,2,3,4)))# 2019 population data
dat_12_sub <- dat_12 %>%  filter(AGEGRP !=0) %>% select(1:17,28,29,52,53)
chg_perct_matrix <- (dat_12_sub[,c(3:21)] - dat_11_sub[,c(3:21)]) /  dat_11_sub[,c(3:21)]
chg_perct_matrix[!sapply(chg_perct_matrix,is.finite)] <- 0 # handle Inf, Nan => set to 1.
chg_perct_matrix  <- (chg_perct_matrix + 1)
dat_13_est <- round(dat_12_sub[,c(3:21)] * chg_perct_matrix)
dat_13_est$TOT_POP <- rowSums(dat_13_est[,4:15])
dat_13_est$TOT_MALE <- dat_13_est$TOT_POP - dat_13_est$TOT_FEMALE
dat_13_sub <- cbind(as.data.frame(dat_12_sub[,1:2]),dat_13_est)
dat_13_total <- dat_13_sub %>% select(-c(2)) %>% group_by(CTYNAME) %>% summarise_all(.funs=sum)
write_csv(dat_13_total,"./Final_data/3.2020_Demographic_Data_By_County_Total.csv")
write_csv(dat_13_sub,"./Final_data/3.2020_Demographic_Data_By_County.csv")

# Topic 4. 2018 General Election (Governor/Senator) in Texas
path <- paste0("./Raw_data/4.2018_Election_Data/",list.files("./Raw_data/4.2018_Election_Data/"))
html_files <- lapply(path, function(i) readLines(i, encoding="UTF-8"))
GS_tables <- lapply(html_files,readHTMLTable, header=TRUE)
for(i in 1:length(GS_tables)){
  GS_tables[[i]][[1]] <- (GS_tables[[i]][[1]])[-1,]
  names(GS_tables[[i]][[1]]) <- NULL
}
GS_tables[[1]][[1]]$race <- "Governor"
GS_tables[[2]][[1]]$race <- "Senator"
##Integrate 2018 election data
GS_Total <- NULL
for(i in 1:length(GS_tables)){
  cur_dat <- GS_tables[[i]][[1]]
  cur_dat[,c(2:6)] <- char_numeric(cur_dat,",","",2:6)
  cur_dat[,c(7)] <-  char_numeric(cur_dat,"%","",7)
  names(cur_dat) <- c("County","REP","DEM","LIB","Total_Votes","Registered_Voters","Turn_Out","Race")
  if(i == 1){
    GS_Total = cur_dat
  }
  else{
    GS_Total <- rbind(GS_Total, cur_dat) 
  }
}
GS_Total <- GS_Total %>% drop_na()
write_csv(GS_Total,"./Final_data/4.2018_Election_Data.csv")