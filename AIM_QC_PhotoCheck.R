####
##This tool is developed to help analyze the photos within a folder directory
### Assumptions:
##  Example naming convention: "CAAIM NCDO_Twin Peaks 111_Soil1_20180612.jpg"
##  - Naming convention is parsed with underscores: "_"
##  - Naming convention starts with Project Name, i.e: "CAAIM NCDO"
##  - Project [designation] is followed by PlotID, i.e: "Twin Peaks 111"
##  - PlotID is followed by Photo Type [description], i.e: "Soil1"
##  - Photo_type is followed by Date [and file_format], i.e: "20180612.jpg"  
##       *currently, file format is not split from date
##       *Date format is YYYYMMDD
###
### This tool performs the following functions: 
## 1. print the number and a list of unique plot IDs present
## 2. Count the number of soil pit photos and cross reference with the
##    list of unique plot IDs to identify missing photos
## 3. Print the number and a list of any missing soil pit photos
## 4. Repeat this process for transect start 1,2, and 3 based on the codes provided in the
##    section below titled "Select Naming Conventions"
####

###
## Dependent Packages
###
library(dplyr)
library(stringr)
library(tidyverse)

### Select Naming Conventions
### Wildcards are programmed around each code
### Thus, "T1S" will also pick up additional photos such as: "T1S 2" or "T1S 3" 
###   As long as underscores are NOT used within the Photo Type naming convention i.e: "T1S_4" would not work
#Variables to modify based on the naming convention utilized
folder_path = "//Blm/dfs/ca/el/pub/public/Monitoring/AIM/Terrestrial/Final Data/CAAIM NCDO 2018/Base AIM Final Data/Base AIM Data/Plot Photos"
    #Folder path must use forward slashes - "/" and go to the folder containing the data
    #This is not currently recursive
file_format = ".jpg" ## ".png"
SoilCode = "Soil1"
Transect1_Start = "T1S" ##Standard is "T1"
Transect2_Start = "T2S" ##Standard is "T2"
Transect3_Start = "T3S" ##Standard is "T3"


#Import Directory to inventory
photo_file_list = list.files(folder_path,pattern = paste("*",file_format,sep = ""),ignore.case = TRUE)
#Create a dataframe from the list of files
Plot_Photo_List_DF = data.frame(photo_file_list)
#Parse the name parts out
Plot_Photo_Parsed <- data.frame(do.call('rbind', strsplit(as.character(Plot_Photo_List_DF$photo_file_list),'_',fixed=TRUE)))
#Change the names to Project, plotID, Photo_Type, and Date
colnames(Plot_Photo_Parsed) <- c("Project", "PlotID","Photo_Type","Date")                 
#Dataframe of the unique plot IDs present in the folder
unique_plot_ID_DF = as.data.frame(unique(Plot_Photo_Parsed[,2]))
#Updated Plot ID field to "PlotID"
colnames(unique_plot_ID_DF) <- "PlotID"
#print how many unique PlotIDs there are withi this folder - compare to plot tracking excel 
    

 
#Create a soil pit photo dataframe
SoilPit_Photos = Plot_Photo_Parsed[grep(SoilCode,Plot_Photo_Parsed$Photo_Type),]
#Join the soil pit data to the unique plot IDs so we can see if anything is missing
PlotID_SoilPit_Join = merge(x = unique_plot_ID_DF,y = SoilPit_Photos,by = "PlotID", all.x = TRUE)
#Dataframe of plot IDs that do not have at least one soil pit photo
PlotIDs_Missing_SoilPit = as.data.frame(PlotID_SoilPit_Join[is.na(PlotID_SoilPit_Join$Photo_Type),])


#Create a transect 1 start photo dataframe
Transect1_Start_Photos = Plot_Photo_Parsed[grep(Transect1_Start,Plot_Photo_Parsed$Photo_Type),]
#Join transect 1 to the unique plotIDs to see if any plot transect 1 photos are missing
PlotID_Transect1_Join = merge(x = unique_plot_ID_DF,y = Transect1_Start_Photos,by = "PlotID", all.x = TRUE)
#Dataframe of plot IDs that do not have at least one soil pit photo
PlotIDs_Missing_Transect1 = as.data.frame(PlotID_Transect1_Join[is.na(PlotID_Transect1_Join$Photo_Type),])


#Create a transect 2 start photo dataframe
Transect2_Start_Photos = Plot_Photo_Parsed[grep(Transect2_Start,Plot_Photo_Parsed$Photo_Type),]
#Join transect 2 to the unique plotIDs to see if any plot transect 2 photos are missing
PlotID_Transect2_Join = merge(x = unique_plot_ID_DF,y = Transect2_Start_Photos,by = "PlotID", all.x = TRUE)
#Dataframe of plot IDs that do not have at least one soil pit photo
PlotIDs_Missing_Transect2 = as.data.frame(PlotID_Transect2_Join[is.na(PlotID_Transect2_Join$Photo_Type),])

#Create a transect 3 start photo dataframe
Transect3_Start_Photos = Plot_Photo_Parsed[grep(Transect3_Start,Plot_Photo_Parsed$Photo_Type),]
#Join transect 3 to the unique plotIDs to see if any plot transect 3 photos are missing
PlotID_Transect3_Join = merge(x = unique_plot_ID_DF,y = Transect3_Start_Photos,by = "PlotID", all.x = TRUE)
#Dataframe of plot IDs that do not have at least one soil pit photo
PlotIDs_Missing_Transect3 = as.data.frame(PlotID_Transect3_Join[is.na(PlotID_Transect3_Join$Photo_Type),])

##Start Reporting Out
#Print a list of the number of unique plot IDs
print(unique_plot_ID_DF[,"PlotID", drop = F])  
#Print the number of unique plot IDs
print(paste("There are", count(unique_plot_ID_DF), "unique Plot IDs within this folder"))


#If any plots missing soil pit photos, print number of missing soil pit photos and list.
#If not, print that they are all accounted for
if (nrow(PlotIDs_Missing_SoilPit) > 0 ){
  print(paste(count(PlotIDs_Missing_SoilPit), "unique Plot IDs are missing soil pit photos in this folder:"))
  PlotIDs_Missing_SoilPit[,"PlotID", drop=FALSE]    
} else {
  print("It would appear that all soil pit photos have been accounted for")
}

#If any plots missing transect 1 start photos, print number of missing transect 1 start photos and list them.
#If not, print that they are all accounted for
if (nrow(PlotIDs_Missing_Transect1) > 0 ){
  print(paste(count(PlotIDs_Missing_Transect1), "unique Plot IDs are missing transect 1 photos in this folder:"))
  PlotIDs_Missing_Transect1[,"PlotID", drop=FALSE]   
} else {
  print("It would appear that all Transect 1 photos have been accounted for")
}

#If any plots missing transect 2 start photos, print number of missing transect 2 start photos and list them.
#If not, print that they are all accounted for
if (nrow(PlotIDs_Missing_Transect2) > 0 ){
  print(paste(count(PlotIDs_Missing_Transect2), "unique Plot IDs are missing transect 2 photos in this folder:"))
  PlotIDs_Missing_Transect2[,"PlotID", drop=FALSE]   
} else {
  print("It would appear that all Transect 2 photos have been accounted for")
}

#If any plots missing transect 3 start photos, print number of missing transect 3 start photos and list them.
#If not, print that they are all accounted for
if (nrow(PlotIDs_Missing_Transect3) > 0 ){
  print(paste(count(PlotIDs_Missing_Transect3), "unique Plot IDs are missing transect 3 photos in this folder:"))
  PlotIDs_Missing_Transect3[,"PlotID", drop=FALSE]   
} else {
  print("It would appear that all Transect 3 photos have been accounted for")
}

print("analysis complete")
