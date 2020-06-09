#Data Exploration Course 
#
#Merrimack College - Fall 2019

#Preliminary R code to read, setup and do some exploratory data analysis and conduct kmeans cluster analysis

#set the working directory to start with in your local machine
setwd ("~/Documents/Merrimack_College_Faculty/Data_Exploration/Data_Exploration_Fall_2019/Module 5/")
#Check that you are in the right directory
getwd()
#To list all the files in your current directory
list.files("~/Documents/Merrimack_College_Faculty/Data_Exploration/Data_Exploration_Fall_2019/Module 5/", recursive= TRUE)

#Invoke the requried library packages
library(data.table)
library(lubridate)
library(bit64)
library(dplyr)
library(stringr)
library(anytime)
library(tidyverse)
library(ggplot2)
library(broom)
library(janitor)
library(dlookr)
library(purrr)
library(psych)
library(openxlsx)
library(expss)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

file_path_cust <- "~/Documents/Merrimack_College_Faculty/Data_Exploration/Data_Exploration_Fall_2019/Module 5/"

#You can run code from the run button in the top right hand side or use Cmd-Return (Mac) to run the code line.
#You can also check for keyboard shortcuts in the Tools menu under keyboard shortcuts or option-shift-k will also take you there
#Read in the customer data
customer_data <- fread("~/Documents/Merrimack_College_Faculty/Data_Exploration/Data_Exploration_Fall_2019/Module 5/Customer_Dataset_File.csv", stringsAsFactors=FALSE)

#To get the names of all the variable columns in the data set
names(customer_data)
#  [1] "CustomerID"          "Region"              "TownSize"            "Gender"              "Age"                
#  [6] "EducationYears"      "JobCategory"         "UnionMember"         "EmploymentLength"    "Retired"            
# [11] "HHIncome"            "DebtToIncomeRatio"   "CreditDebt"          "OtherDebt"           "LoanDefault"        
# [16] "MaritalStatus"       "HouseholdSize"       "NumberPets"          "NumberCats"          "NumberDogs"         
# [21] "NumberBirds"         "HomeOwner"           "CarsOwned"           "CarOwnership"        "CarBrand"           
# [26] "CarValue"            "CommuteTime"         "PoliticalPartyMem"   "Votes"               "CreditCard"         
# [31] "CardTenure"          "CardItemsMonthly"    "CardSpendMonth"      "ActiveLifestyle"     "PhoneCoTenure"      
# [36] "VoiceLastMonth"      "VoiceOverTenure"     "EquipmentRental"     "EquipmentLastMonth"  "EquipmentOverTenure"
# [41] "CallingCard"         "WirelessData"        "DataLastMonth"       "DataOverTenure"      "Multiline"          
# [46] "VM"                  "Pager"               "Internet"            "CallerID"            "CallWait"           
# [51] "CallForward"         "ThreeWayCalling"     "EBilling"            "TVWatchingHours"     "OwnsPC"             
# [56] "OwnsMobileDevice"    "OwnsGameSystem"      "OwnsFax"             "NewsSubscriber"     

#What is the dimension of the data set - number of rows and columns
dim(customer_data)
# [1] 5000   59

str(customer_data)
# Classes ‘data.table’ and 'data.frame':	5000 obs. of  59 variables:
#  $ CustomerID         : chr  "3964-QJWTRG-NPN" "0648-AIPJSP-UVM" "5195-TLUDJE-HVO" "4459-VLPQUH-3OL" ...
#  $ Region             : int  1 5 3 4 2 4 2 3 2 2 ...
#  $ TownSize           : num  2 5 4 3 2 4 5 4 3 2 ...
#  $ Gender             : chr  "Female" "Male" "Female" "Male" ...
#  $ Age                : int  20 22 67 23 26 64 52 44 66 47 ...
#  $ EducationYears     : int  15 17 14 16 16 17 14 16 12 11 ...
#  $ JobCategory        : chr  "Professional" "Sales" "Sales" "Sales" ...
#  $ UnionMember        : chr  "Yes" "No" "No" "No" ...
#  $ EmploymentLength   : int  0 0 16 0 1 22 10 11 15 19 ...
#  $ Retired            : chr  "No" "No" "No" "No" ...
#  $ HHIncome           : chr  " $31,000.00 " " $15,000.00 " " $35,000.00 " " $20,000.00 " ...
#  $ DebtToIncomeRatio  : num  11.1 18.6 9.9 5.7 1.7 5.6 1.9 14.4 2.6 4.1 ...
#  $ CreditDebt         : num  1.2009 1.222 0.9286 0.0228 0.2147 ...
#  $ OtherDebt          : num  2.24 1.568 2.536 1.117 0.176 ...
#  $ LoanDefault        : chr  "Yes" "Yes" "No" "Yes" ...
#  $ MaritalStatus      : chr  "Unmarried" "Unmarried" "Married" "Married" ...
#  $ HouseholdSize      : int  3 2 3 5 4 1 1 2 1 2 ...
#  $ NumberPets         : int  0 6 3 0 0 11 2 10 1 1 ...
#  $ NumberCats         : int  0 0 2 0 0 1 0 0 1 1 ...
#  $ NumberDogs         : int  0 0 1 0 0 1 2 2 0 0 ...
#  $ NumberBirds        : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ HomeOwner          : int  0 1 1 1 0 1 0 1 1 1 ...
#  $ CarsOwned          : int  2 2 3 3 1 0 2 1 1 4 ...
#  $ CarOwnership       : chr  "Own" "Own" "Own" "Own" ...
#  $ CarBrand           : chr  "Domestic" "Foreign" "Foreign" "Foreign" ...
#  $ CarValue           : chr  " $14,300.00 " " $6,800.00 " " $18,800.00 " " $8,700.00 " ...
#  $ CommuteTime        : num  22 29 24 38 32 23 32 31 25 29 ...
#  $ PoliticalPartyMem  : chr  "Yes" "Yes" "Yes" "No" ...
#  $ Votes              : chr  "Yes" "No" "No" "No" ...
#  $ CreditCard         : chr  "Mast" "Visa" "Visa" "Visa" ...
#  $ CardTenure         : int  2 4 35 5 8 18 3 25 26 2 ...
#  $ CardItemsMonthly   : int  5 5 9 17 8 11 20 6 12 11 ...
#  $ CardSpendMonth     : chr  "$816.60" "$426.00" " $1,842.20 " " $3,409.90 " ...
#  $ ActiveLifestyle    : chr  "No" "Yes" "No" "Yes" ...
#  $ PhoneCoTenure      : int  5 39 65 36 21 28 15 46 53 3 ...
#  $ VoiceLastMonth     : chr  "$19.50" "$26.70" "$85.20" "$18.00" ...
#  $ VoiceOverTenure    : chr  "$34.40" "$330.60" " $1,858.35 " "$199.45" ...
#  $ EquipmentRental    : chr  "Yes" "Yes" "No" "No" ...
#  $ EquipmentLastMonth : chr  "$29.50" "$54.85" "$-" "$-" ...
#  $ EquipmentOverTenure: chr  "$126.10" " $1,975.00 " "$-" "$-" ...
#  $ CallingCard        : chr  "Yes" "Yes" "Yes" "Yes" ...
#  $ WirelessData       : chr  "No" "Yes" "No" "No" ...
#  $ DataLastMonth      : chr  "$-" "$45.65" "$-" "$-" ...
#  $ DataOverTenure     : chr  "$-" " $1,683.55 " "$-" "$-" ...
#  $ Multiline          : chr  "Yes" "Yes" "Yes" "Yes" ...
#  $ VM                 : chr  "Yes" "Yes" "No" "No" ...
#  $ Pager              : chr  "Yes" "Yes" "No" "No" ...
#  $ Internet           : chr  "No" "4" "No" "2" ...
#  $ CallerID           : chr  "No" "Yes" "No" "No" ...
#  $ CallWait           : chr  "Yes" "No" "No" "No" ...
#  $ CallForward        : chr  "Yes" "Yes" "No" "No" ...
#  $ ThreeWayCalling    : chr  "Yes" "No" "No" "No" ...
#  $ EBilling           : chr  "No" "Yes" "No" "Yes" ...
#  $ TVWatchingHours    : int  13 18 21 26 27 21 19 13 25 21 ...
#  $ OwnsPC             : chr  "No" "Yes" "No" "Yes" ...
#  $ OwnsMobileDevice   : chr  "Yes" "Yes" "No" "Yes" ...
#  $ OwnsGameSystem     : chr  "Yes" "Yes" "No" "Yes" ...
#  $ OwnsFax            : chr  "No" "Yes" "No" "No" ...
#  $ NewsSubscriber     : chr  "No" "Yes" "Yes" "Yes" ...

#What are the difference data types in the data set?
sapply(customer_data, class)
    #      CustomerID              Region            TownSize              Gender                 Age      EducationYears 
    #     "character"           "integer"           "numeric"         "character"           "integer"           "integer" 
    #     JobCategory         UnionMember    EmploymentLength             Retired            HHIncome   DebtToIncomeRatio 
    #     "character"         "character"           "integer"         "character"         "character"           "numeric" 
    #      CreditDebt           OtherDebt         LoanDefault       MaritalStatus       HouseholdSize          NumberPets 
    #       "numeric"           "numeric"         "character"         "character"           "integer"           "integer" 
    #      NumberCats          NumberDogs         NumberBirds           HomeOwner           CarsOwned        CarOwnership 
    #       "integer"           "integer"           "integer"           "integer"           "integer"         "character" 
    #        CarBrand            CarValue         CommuteTime   PoliticalPartyMem               Votes          CreditCard 
    #     "character"         "character"           "numeric"         "character"         "character"         "character" 
    #      CardTenure    CardItemsMonthly      CardSpendMonth     ActiveLifestyle       PhoneCoTenure      VoiceLastMonth 
    #       "integer"           "integer"         "character"         "character"           "integer"         "character" 
    # VoiceOverTenure     EquipmentRental  EquipmentLastMonth EquipmentOverTenure         CallingCard        WirelessData 
    #     "character"         "character"         "character"         "character"         "character"         "character" 
    #   DataLastMonth      DataOverTenure           Multiline                  VM               Pager            Internet 
    #     "character"         "character"         "character"         "character"         "character"         "character" 
    #        CallerID            CallWait         CallForward     ThreeWayCalling            EBilling     TVWatchingHours 
    #     "character"         "character"         "character"         "character"         "character"           "integer" 
    #          OwnsPC    OwnsMobileDevice      OwnsGameSystem             OwnsFax      NewsSubscriber 
    #     "character"         "character"         "character"         "character"         "character" 

#What is the extent of missing values in the dataset?
sapply(customer_data, function(x) sum(is.na(x))/length(x))*100

#How many missing observations do we have in the data set?
#There are 5000*59 = 295000 cells in the data set of which
sum(is.na(customer_data))
#[1] 80

#Gives you a "heads up" on the first five observations in your dataset
head(customer_data)
#         CustomerID Region TownSize Gender Age EducationYears  JobCategory UnionMember EmploymentLength Retired      HHIncome
# 1: 3964-QJWTRG-NPN      1        2 Female  20             15 Professional         Yes                0      No   $31,000.00 
# 2: 0648-AIPJSP-UVM      5        5   Male  22             17        Sales          No                0      No   $15,000.00 
# 3: 5195-TLUDJE-HVO      3        4 Female  67             14        Sales          No               16      No   $35,000.00 
# 4: 4459-VLPQUH-3OL      4        3   Male  23             16        Sales          No                0      No   $20,000.00 
# 5: 8158-SMTQFB-CNO      2        2   Male  26             16        Sales          No                1      No   $23,000.00 
# 6: 9662-FUSYIM-1IV      4        4   Male  64             17      Service          No               22      No  $107,000.00 
#    DebtToIncomeRatio CreditDebt OtherDebt LoanDefault MaritalStatus HouseholdSize NumberPets NumberCats NumberDogs NumberBirds
# 1:              11.1   1.200909  2.240091         Yes     Unmarried             3          0          0          0           0
# 2:              18.6   1.222020  1.567980         Yes     Unmarried             2          6          0          0           0
# 3:               9.9   0.928620  2.536380          No       Married             3          3          2          1           0
# 4:               5.7   0.022800  1.117200         Yes       Married             5          0          0          0           0
# 5:               1.7   0.214659  0.176341          No       Married             4          0          0          0           0
# 6:               5.6   1.060584  4.931416          No     Unmarried             1         11          1          1           0
#    HomeOwner CarsOwned CarOwnership CarBrand     CarValue CommuteTime PoliticalPartyMem Votes CreditCard CardTenure
# 1:         0         2          Own Domestic  $14,300.00           22               Yes   Yes       Mast          2
# 2:         1         2          Own  Foreign   $6,800.00           29               Yes    No       Visa          4
# 3:         1         3          Own  Foreign  $18,800.00           24               Yes    No       Visa         35
# 4:         1         3          Own  Foreign   $8,700.00           38                No    No       Visa          5
# 5:         0         1        Lease  Foreign  $10,600.00           32                No    No       Disc          8
# 6:         1         0           -1       -1  $(1,000.00)          23                No    No       Visa         18
#    CardItemsMonthly CardSpendMonth ActiveLifestyle PhoneCoTenure VoiceLastMonth VoiceOverTenure EquipmentRental
# 1:                5        $816.60              No             5         $19.50          $34.40             Yes
# 2:                5        $426.00             Yes            39         $26.70         $330.60             Yes
# 3:                9     $1,842.20               No            65         $85.20      $1,858.35               No
# 4:               17     $3,409.90              Yes            36         $18.00         $199.45              No
# 5:                8     $2,551.00              Yes            21          $9.15          $74.10              No
# 6:               11     $2,282.70               No            28         $24.30         $264.90             Yes
#    EquipmentLastMonth EquipmentOverTenure CallingCard WirelessData DataLastMonth DataOverTenure Multiline  VM Pager Internet
# 1:             $29.50             $126.10         Yes           No            $-             $-       Yes Yes   Yes       No
# 2:             $54.85          $1,975.00          Yes          Yes        $45.65     $1,683.55        Yes Yes   Yes        4
# 3:                 $-                  $-         Yes           No            $-             $-       Yes  No    No       No
# 4:                 $-                  $-         Yes           No            $-             $-       Yes  No    No        2
# 5:                 $-                  $-         Yes          Yes        $19.05        $410.80        No Yes    No        3
# 6:             $35.50             $970.95         Yes           No            $-             $-        No  No   Yes       No
#    CallerID CallWait CallForward ThreeWayCalling EBilling TVWatchingHours OwnsPC OwnsMobileDevice OwnsGameSystem OwnsFax
# 1:       No      Yes         Yes             Yes       No              13     No              Yes            Yes      No
# 2:      Yes       No         Yes              No      Yes              18    Yes              Yes            Yes     Yes
# 3:       No       No          No              No       No              21     No               No             No      No
# 4:       No       No          No              No      Yes              26    Yes              Yes            Yes      No
# 5:      Yes      Yes         Yes             Yes       No              27    Yes               No            Yes      No
# 6:      Yes      Yes         Yes             Yes       No              21     No               No             No      No
#    NewsSubscriber
# 1:             No
# 2:            Yes
# 3:            Yes
# 4:            Yes
# 5:             No
# 6:             No
# tail(name of dataframe) will give you the last 5 observations in your dataset
tail(customer_data)

#Provides you with a summary of all the fields in the data set. 
#This command is not recommended if you have thouands of variables as 
summary(customer_data)

#Provides you with a glimpse of the dataset
glimpse(customer_data)

#I typically use the command tabyl from the janitor package to get and understanding of the frequency distribution of variabels
tabyl(customer_data$HouseholdSize)
# customer_data$HouseholdSize    n percent valid_percent
#                            1 2032  0.4064   0.407051282
#                            2 1466  0.2932   0.293669872
#                            3  552  0.1104   0.110576923
#                            4  520  0.1040   0.104166667
#                            5  287  0.0574   0.057491987
#                            6   97  0.0194   0.019431090
#                            7   29  0.0058   0.005809295
#                            8    7  0.0014   0.001402244
#                            9    2  0.0004   0.000400641
#                           NA    8  0.0016            NA

#customer_data <- setDT(customer_data)
class(customer_data)

table(customer_data$Gender, customer_data$CarsOwned)

tabyl(customer_data$EducationYears)
class(customer_data$EducationYears)
#Recoding using dplyr
customer_data <- mutate(customer_data, educ_years_rec = case_when(
 EducationYears %in%  6:10      ~ 1,
 EducationYears %in%  11:15     ~ 2,
 EducationYears %in%  16:19     ~ 3,
 EducationYears %in%  20:21     ~ 4,
 EducationYears %in%  22:23     ~ 5
 )
)  
tabyl(customer_data$educ_years_rec)
# customer_data$educ_years_rec    n percent
#                             1  584  0.1168
#                             2 2465  0.4930
#                             3 1597  0.3194
#                             4  317  0.0634
#                             5   37  0.0074

customer_data <- mutate(customer_data, educ_years_rec_ch = case_when(
 EducationYears %in%  6:10      ~ '6-10 years of school',
 EducationYears %in%  11:15     ~ '11-15 years of school',
 EducationYears %in%  16:19     ~ '16-19 years of school',
 EducationYears %in%  20:21     ~ '20-21 years of school',
 EducationYears %in%  22:23     ~ '22-23 years of school'
 )
)  

tabyl(customer_data$educ_years_rec_ch)

tabyl(customer_data$NumberPets)

diag_customer_data <- diagnose(customer_data)
#fwrite function writes out the csv file to your working directory
fwrite(diag_customer_data, paste0(file_path_cust,"diag_customer_data.csv"))


#Based on the output from the diagnose() work, here are some columns that have missing values:
# variables	    types	 missing_count	missing_percent	unique_count	unique_rate
# HouseholdSize	integer	8	            0.16	            10	        0.002
# NumberPets	  integer	6	            0.12	            21        	0.0042
# NumberCats	  integer	7	            0.14	             8	        0.0016
# NumberDogs	  integer	8	            0.16	             8	        0.0016
# NumberBirds	  integer	34	          0.68	             7        	0.0014
# HomeOwner	    integer	13	          0.26	             3	        6.00E-04


#Selecting only numeric columns
names_num_cust_data <- names(select_if(customer_data, is.numeric))
names_num_cust_data

num_cust_data <- select_if(customer_data, is.numeric)
dim(num_cust_data)

names_char_cust_data <- names(select_if(customer_data, is.character))
names_char_cust_data

#knitr::kable does not display on the R output
df_prop_1 <- prop.table(table(customer_data$Gender, customer_data$CarsOwned), margin=2)
knitr::kable(df_prop_1, caption = "Column Proportions of gender by cars owned")

knitr::kable(prop.table(table(customer_data$ActiveLifestyle, customer_data$LoanDefault), margin=2), caption = "Column Proportions of Active Life Style by Loan default ")

knitr::kable(prop.table(table(customer_data$Gender, customer_data$CarsOwned), margin=2), align = 'c', caption = 'Table of proportion of Gender by number of cars owned')

#Replacing missing values with means
customer_data$HouseholdSize[is.na(customer_data$HouseholdSize)] = mean(customer_data$HouseholdSize, na.rm=TRUE)
tabyl(customer_data$HouseholdSize)
tabyl(customer_data$LoanDefault)
tabyl(customer_data$HomeOwner)


customer_data$NumberPets[is.na(customer_data$NumberPets)] = mean(customer_data$NumberPets, na.rm=TRUE)
customer_data$NumberCats[is.na(customer_data$NumberCats)] = mean(customer_data$NumberCats, na.rm=TRUE)
customer_data$NumberDogs[is.na(customer_data$NumberDogs)] = mean(customer_data$NumberDogs, na.rm=TRUE)
customer_data$NumberBirds[is.na(customer_data$NumberBirds)] = mean(customer_data$NumberBirds, na.rm=TRUE)

mean(customer_data$DebtToIncomeRatio, na.rm = FALSE)
#9.95416

tabyl(customer_data$HHIncome)

class(customer_data$HHIncome)

knitr::kable(prop.table(table(customer_data$Gender, customer_data$CarsOwned), margin=2), align = 'c', caption = 'Table of proportion of Gender by number of cars owned') %>% 

#HHIncome has a $ sign and a comma and therefore needs to be converted to a numeric vector
#Create a new variable 
customer_data$HHIncome_2 = as.integer(gsub("[\\$,]", "", customer_data$HHIncome))
class(customer_data$HHIncome_2)
mean(customer_data$HHIncome_2)
tabyl(customer_data$HHIncome_2)

#Recode HHIncome into a 6 level variable
customer_data <- mutate(customer_data, HHIncome_rec = case_when(
 HHIncome_2 %in%  0:24000          ~ 1,
 HHIncome_2 %in%  24001:50000      ~ 2,
 HHIncome_2 %in%  50001:75000      ~ 3,
 HHIncome_2 %in%  75001:100000     ~ 4,
 HHIncome_2 %in%  100001:150001    ~ 5,
 HHIncome_2 >     150001           ~ 6
 )
)  

tabyl(customer_data$HHIncome_rec)
 # customer_data$HHIncome_rec    n percent
 #                          1 1330  0.2660
 #                          2 1835  0.3670
 #                          3  818  0.1636
 #                          4  436  0.0872
 #                          5  345  0.0690
 #                          6  236  0.0472

tabyl(customer_data$DebtToIncomeRatio)


debt_inc_above_10 <- customer_data %>%
  filter(DebtToIncomeRatio > 20)

#Displays only the highest debt to income ratios > 20
(tabyl(debt_inc_above_10$DebtToIncomeRatio))

# #Example from tabyl vignette
# percent_above_165_cm <- humans %>%
#   group_by(gender) %>%
#   summarise(pct_above_165_cm = mean(height > 165, na.rm = TRUE))

tabyl(customer_data$LoanDefault)
customer_data$LoanDefault_num <- ifelse(customer_data$LoanDefault == "No", 1, 2)
tabyl(customer_data$LoanDefault_num)
prop.table(table(customer_data$LoanDefault, customer_data$DebtToIncomeRatio), margin=2)   #By columns
tabyl(customer_data$HomeOwner)
customer_data$HomeOwner[is.na(customer_data$HomeOwner)]  <-  0

#Recoding the missing values of Gender to Female
#customer_data$Gender <- ifelse((customer_data$Gender == "" | customer_data$Gender == "Female"), "Female", "Male")

#Convert all missing values for gender to female - arbitrary decision
customer_data$Gender[is.na(customer_data$Gender)]  <-  "Female"
tabyl(customer_data$Gender)
customer_data$Gender_num <- ifelse(customer_data$Gender == "Female", 1, 2)
tabyl(customer_data$Gender_num)


#Variables for conducting  a segmentation with K Mean
# EducationYears
# EmploymentLength
# HHIncome_rec
# DebtToIncomeRatio
# LoanDefault_num
# HouseholdSize
# HomeOwner
# CardTenure
# Gender_num

#Creating a new debt to income ratio variable by multiplying by 10 to facilitate 
#bucketing since the right hand side vector must match the left hand side vector

customer_data$DebtToIncomeRatio_2 <- as.integer(10*(customer_data$DebtToIncomeRatio))

customer_data <- mutate(customer_data, DebtToIncomeRatio_rec = case_when(
DebtToIncomeRatio_2 %in%  0:20       ~ 1,
DebtToIncomeRatio_2 %in%  21:40      ~ 2,
DebtToIncomeRatio_2 %in%  41:60      ~ 3,
DebtToIncomeRatio_2 %in%  61:80      ~ 4,
DebtToIncomeRatio_2 %in%  81:100     ~ 5,
DebtToIncomeRatio_2 %in%  101:130    ~ 6,
DebtToIncomeRatio_2 >     130         ~ 7
 )
)  

customer_data <- mutate(customer_data, DebtToIncomeRatio_rec3 = case_when(
DebtToIncomeRatio %in%  0:2.09       ~ 1.0,
DebtToIncomeRatio %in%  2.1:4.09      ~ 2.0,
DebtToIncomeRatio %in%  4.1:6.09      ~ 3.0,
DebtToIncomeRatio %in%  6.1:8.09      ~ 4.0,
DebtToIncomeRatio %in%  8.1:10.09     ~ 5.0,
DebtToIncomeRatio %in%  10.1:13.0    ~ 6.0,
DebtToIncomeRatio >     13.0        ~ 7.0
 )
) 

tabyl(customer_data$DebtToIncomeRatio_rec3)

customer_data <- mutate(customer_data, DebtToIncomeRatio_rec2 = case_when(
DebtToIncomeRatio_2 %in%  0:20       ~ "A",
DebtToIncomeRatio_2 %in%  21:40      ~ "B",
DebtToIncomeRatio_2 %in%  41:60      ~ "C",
DebtToIncomeRatio_2 %in%  61:80      ~ "D",
DebtToIncomeRatio_2 %in%  81:100     ~ "E",
DebtToIncomeRatio_2 %in%  101:130    ~ "F",
DebtToIncomeRatio_2 >     130        ~ "G"
 )
)  

tabyl(customer_data$DebtToIncomeRatio_rec2)

customer_data$DebtToIncomeRatio_rec2 <- NULL
customer_data$DebtToIncomeRatio_rec3 <- NULL


tabyl(customer_data$DebtToIncomeRatio_rec)

cust_data.df.num <- subset(customer_data, select = c("EducationYears",
                                               "EmploymentLength",
                                               "HHIncome_2",
                                               "DebtToIncomeRatio",
                                               "LoanDefault_num",
                                               "HouseholdSize",
                                               "HomeOwner",
                                               "CardTenure",
                                               "Gender_num"))

#To check if the column exists in my dataset
c("LoanDefault_num") %in% colnames(customer_data)


#An anonymous function(x) then converts all of a group's data to 
#numeric(as.numeric) and computes its mean() for each of the groups levels

seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}


#Example of using the seg.summ function to examine how the measures vary across the Gender variable which
#can be construed as a crude segmentation based on gender. 

names(cust_data.df.num)
seg.summ(cust_data.df.num, customer_data$Gender)

names(cust_data.df.num)

#Examples of creating cross tabulations to examine the distribution of variables against each other
#These are what you may have to compute to examine segment solutions
t_test1 <- cust_data.df.num %>% 
  tabyl(HouseholdSize, HomeOwner, show_missing_levels = FALSE) %>% 
  adorn_totals("row") %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title("combined")
t_test1

t_test2 <- cust_data.df.num %>% 
  tabyl(HomeOwner, Gender_num, show_missing_levels = FALSE) %>% 
  adorn_totals("row") %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title("combined")
t_test2

#Using tabyl from the janitor package
customer_data %>%
  tabyl(Gender, LoanDefault) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") 


# Steps in conducting segmentation or clustering
# 1.  Transform the data if required for each clustering method. Most clustering methods will only accept
# numeric values. kmeans procedure accepts all numeric data.
# 2.  Compute a distance matrix if needed. kmeans does not require a distance matrix but you should scale the data
# so that scale effects do not change the segment interpretation
# 3.  Apply the clustering method to the data object and save its result to another object. kmeans require that 
# you specify the number of clusters that you want to examine. 
# 4.  Examine the solution in the model object with regard to the underlying data and consider whether it 
# answers the business question. This can be done using the seg.summ function created earlier where you can
# examine the The most difficult part of the segmentation process is step 4; i.e., establishing whether q
# proposed segmantin solution answers a business need. The cluster solution is largely a vector of allegedly 
# "true" segment assignments for each observation. It is up to the analyst whether that tells a meaningful story
# to your data. The simple function seg.summ provided provides an inspection of the high level 
# differences between groups. This simple function provides the mean of each group and allows the analyst to 
# inspect cluster solutions efficiently. It also treat categorical variables as numbers which is not advisable but 
# provides a quick answer to determine if there is anything interesting occurring ina a solution. Other solutions
# to use could be tabyl function fron the janitor package and the expss package. 
# Examples to use the Tabyl function are shown earlier


#Check that all the data is numeric and there are no missing values
summary(cust_data.df.num)
sapply(cust_data.df.num, class)
sum(is.na(cust_data.df.num))

#Rescale the data before input into kmeans for creating clusters
cust_data.df.num_2 <- as.data.frame(scale(cust_data.df.num))
class(cust_data.df.num_2)


cust_data.kmeans_seg_5 <- kmeans(cust_data.df.num_2, centers = 5, nstart = 25)
cust_data.kmeans_seg_5

str(cust_data.kmeans_seg_5)
#cluster is a named variable in a kmeans solution

#cluster is a named variable in a kmeans solution
seg.summ(cust_data.df.num_2, cust_data.kmeans_seg_5$cluster)

#Another way to examine the distribution of income across the 5 cluster solutions
plot(cust_data.df.num$HHIncome_2 ~cust_data.kmeans_seg_5$cluster, ylab = "Income", xlab = "Cluster")

# We can also view our results by using fviz_cluster. 
# This provides a nice illustration of the clusters. 
# If there are more than two dimensions (variables) the 
# fviz_cluster procedure will perform principal component analysis (PCA) 
# and plot the data points according to the first two principal components
# that explain the majority of the variance.

#Graphical display of the 5 segment solution
sol_5 <-fviz_cluster(cust_data.kmeans_seg_5, cust_data.df.num_2)
sol_5
# We can also use standard pair wise scatter plots to illustrate th
# clusters compared to the original variables

#Not a good display of the segment differences and needs more work
cust_data.df.num_2 %>% 
  as_tibble() %>% 
  mutate(cluster = cust_data.kmeans_seg_5$cluster) %>% 
  ggplot(aes(HHIncome_2, LoanDefault_num, color = factor(cluster))) +
  geom_point()

# For kmeans the number of cluster (k) must be set before starting the algorithm.
# It is often advantageous to to use several different clustering solutions with different
# values of k and examine the differences in the solution results.
# We therefore invoke the same kmeans algorithm for 5, 6 and 7 cluster and examine the results

cust_data.kmeans_seg_6 <- kmeans(cust_data.df.num_2, centers = 6, nstart = 25)
cust_data.kmeans_seg_7 <- kmeans(cust_data.df.num_2, centers = 7, nstart = 25)

#Graphical display of the 6 and 7 segment solution

# Use fviz plots to compare across the different cluster solutions
sol_6 <- fviz_cluster(cust_data.kmeans_seg_6, cust_data.df.num_2)
sol_6
sol_7 <- fviz_cluster(cust_data.kmeans_seg_7, cust_data.df.num_2)
sol_7

# Determining optimal clusters. There are no good solutions for determining the optimal clusters.
# It is a process of judgment looking at the plots, how well they are separated and also the profile
# for each cluster solutions using profiling variables that were not used to drive the clustering solution
# The three most popular graphical approaches for determing optimal clusters are:
#   1. Elbow method
#   2. Silhoutte method
#   3. Gap statistics

# Elbow method
# Recall that, the basic idea behind cluster partitioning methods, such as k-means clustering, 
# is to define clusters such that the total intra-cluster variation (known as total within-cluster 
# variation or total within-cluster sum of square) is minimized.
# The total within-cluster sum of square (wss) measures the compactness of the clustering 
# and we want it to be as small as possible. Thus, we can use the following algorithm 
# to define the optimal clusters:
# Hence we can compute clustering algorithm (e.g., k-means clustering) for different values of k. 
# For instance, by varying k from 1 to 10 clusters
# For each k, calculate the total within-cluster sum of square (wss)
# Plot the curve of wss according to the number of clusters k.
# The location of a bend (knee) in the plot is generally considered as an indicator 
# of the appropriate number of clusters.

set.seed(1234)
#Function to compute total within cluster sum of squares 
wss <- function(k) {
  kmeans(cust_data.df.num_2, k, nstart = 10)$tot.withinss
}

#Compute and plot the within sum of squares (wss) for k = 1 to k = 10
k.values <- 1:10

#Extract wss for 2 - 10 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within clusters sum of squares")

set.seed(1234)
fviz_nbclust(cust_data.df.num_2, kmeans, method = "wss")

#ALL SUBSEQUENT CODE IS PROVIDED FOR THOSE WHO HAVE THE INTEREST TO PURSUE KMEANS CLUSTER FURTHER

# Average Silhouette Method
# Silhouette refers to a method of interpretation and validation of consistency within clusters of data. 
# The technique provides a succinct graphical representation of how well each object has been classified.
# The silhouette value is a measure of how similar an object is to its own cluster (cohesion) 
# compared to other clusters (separation). The silhouette ranges from −1 to +1, where a 
# high value indicates that the object is well matched to its own cluster and poorly
# matched to neighboring clusters. If most objects have a high value, then the 
# clustering configuration is appropriate. If many points have a low or negative value, 
# then the clustering configuration may have too many or too few clusters.
# The silhouette can be calculated with any distance metric, such as the 
# Euclidean distance or the Manhattan distance.

# The average silhouette approach measures the quality of a clustering by determining how 
# well each object lies in its cluster. A high average silhouette width indicates a good
# clustering. The average silhouette method computes the average silhoutte of observations 
# for different values of k. The optimal number of clusters k is one that maximizes the 
# average silhoutte over a range of possible values of k. 
# To compute the average silhoutte width, the silhoutte function in the cluster package is used.
# The following code computes the average silhouette values.
#Function to compute average silhouette for k clusters

avg_sil <- function(k) {
  km.res <- kmeans(cust_data.df.num_2, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(cust_data.df.num_2))
  mean(ss[, 3])
}

#Compute and plot wss for k = 2 to k = 10
k.values <-  2: 10

#Visually check the distribution of income according to segment
#box
#Extract average silhouette for 2 - 10 clusters
avg_sil_values <-  map_dbl(k.values, avg_sil)

# Based on the plots - does the 7 segment solution look optimal??
plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within clusters sum of squares")
                           
# Similar to the elbow method, this process to compute the “average silhoutte method” 
# has been wrapped up in a single function (fviz_nbclust):

fviz_nbclust(cust_data.df.num_2, kmeans, method = "silhouette")

# Gap Statistic Method
# The gap statistic has been published by R. Tibshirani, G. Walther, and T. Hastie 
# (Standford University, 2001). The approach can be applied to any clustering method 
# (i.e. K-means clustering, hierarchical clustering). The gap statistic
# compares the total intracluster variation for different values of k with their 
# expected values under null reference distribution of the data 
# (i.e. a distribution with no obvious clustering).
# The reference dataset is generated using Monte Carlo simulations 
# of the sampling process.
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(cust_data.df.num_2, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 10)
# Print the result
print(gap_stat, method = "firstmax")

#We can visualize the results with fviz_gap_stat function

fviz_gap_stat(gap_stat)




