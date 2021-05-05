####Dicking around with UAR RA data for figures]
library(tidyverse)
library(here)
RA <- read.csv(here("UAR_RapidAssessmentforStructures_RInput_2020_ER_SG.csv"))
RA <- read.csv(file.choose())
x <- RA %>%
  mutate(YAC = Year-2013, #gets number of years after 2013 the strucutre is
         failed = ifelse((Integrity == 4 | Integrity == 3), 1, 0))   # | is OR operator; Integrity == 3 | if we want to do "impaired" instead of failed


# x1 <- x %>%
#   filter(!is.na(Integrity)) %>%
#   group_by(Structure_Type, Year) %>%
#   summarize(failedsum = sum(failed),
#             total_structures = n())
#once here, combine total str_num and do math stuff
#this is the same as grouping by structure typem Year and doing summarise (n = n())
# x2 <- x %>%
#   count(Structure_Type, Year, fir_imp_yr_integrity)
# x3 <- x2 %>%
#   group_by(Structure_Type, Year) %>%
#   summarize(Percent = fir_imp_yr_integrity / sum(n))
  #mutate(Percent = fir_imp_yr_integrity / )

# x1 <- x %>%
#   
#   count(Structure_Type, Year)
  #summarise(Integrity_percent = sum(100 * (failed / length(unique(RA$Num)))))


  #plot for when each structure failed . denominator is total number of structures  
x1 <- x %>%
  group_by(Year, Structure_Type, Integrity) %>%
  summarise(percent_failed = sum(100 * (failed / length(unique(RA$Num))), na.rm = TRUE)) #gives the percentage of structures that failed in a given year
x1 %>%  
  ggplot(aes(x= Year, y = percent_failed)) +
  geom_bar(stat = "identity",position = "dodge",aes(fill = Structure_Type)) +
  theme_classic() + 
  labs(title="New Structure Failures (3 or 4) by Year") +
  scale_fill_grey()
  
  
#scatter of the same plot
x %>%
  group_by(YAC, Structure_Type) %>%
  summarise(percent_failed = sum(100 * (failed / length(unique(RA$Num))), na.rm = TRUE)) %>% #gives the percentage of structures that failed in a given year
  ggplot(aes(x= YAC, y = percent_failed)) +
  geom_point(aes(color = Structure_Type)) +
  theme_classic() +
  labs(title="New Structure Failures (4) by Year After Completion")



#plot for when each structure failed. Normalized by # of the same type of structure. so denomitor changes between strtucure types
x %>%
  group_by(Structure_Type, YAC) %>%
  summarise(sum_failed = sum(failed, na.rm = TRUE), rel_failed = (100*sum_failed/length(unique(Num)))) %>% #rel_failed is sum of (new) 4 diagnoses in integrity per year, divided by total number of structures of that same type that year. NA's are counted as a non-failed structure. Which makes sense, because usually, if they're an NA, it means that they've been diagnosed as failed in previous years.An idea is to do the same for failures, but som years are diagnosed 3 over the years, and are only NA after they become 4s. So if the goal is going for "New" impairments, then the df is gonna look a little different 
  ggplot(aes(x = YAC, y = rel_failed)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = Structure_Type)) +
  labs(title="New Structure Failures (4) by Year Normalized by Structure type") +
  theme_classic()

###same plot but dif method, where I use add_tally to find total number of structure numbers
# x %>%
#   group_by(Structure_Type, YAC) %>%
#   add_tally(name = "Total_structure") %>% #add tally adds a new column and assumes you've already grouped_by the things you want to count
#   summarise(sum_failed = sum(failed, na.rm = TRUE), Total_structure = Total_structure, rel_failed = (100*sum_failed/Total_structure)) %>% #rel_failed is sum of (new) 4 diagnoses in integrity per year, divided by total number of structures of that same type that year. NA's are counted as a non-failed structure. Which makes sense, because usually, if they're an NA, it means that they've been diagnosed as failed in previous years.An idea is to do the same for failures, but som years are diagnosed 3 over the years, and are only NA after they become 4s. So if the goal is going for "New" impairments, then the df is gonna look a little different 
#   ggplot(aes(x = YAC, y = rel_failed)) + 
#   geom_bar(stat = "identity", position = "dodge", aes(fill = Structure_Type)) +
#   labs(title="New Structure Failures (4) by Year Normalized by Structure type") +
#   theme_classic()


# First year impariment function ------------------------------------------

find_first_year_of_impairment_integrity <- function (RA_data) {
  RA_data$fir_imp_yr_integrity <- 0 ##needs to be outside the for loop
  for (str_num in unique(RA_data$Num)) {
    #print(str_num)
    #finds how many rows the structure was iagnosed with 3 or 4
    impaired_rows <- which((RA_data$Integrity == 3 | RA_data$Integrity == 4) & RA_data$Num == str_num) #makes vector of which rows a specific structure was impaired or failed
    #print(length(impaired_rows))
    
    #if the length of that vector is 0 then it 
    if (length(impaired_rows > 0)) {
      #print(c("This structure was impaired or failed"))
      minval <- min(impaired_rows)
      #print(minval)
      RA_data[minval, c("fir_imp_yr_integrity")] <- 1
      print(paste("first impaired year: ", RA_data$Year[minval], "entered for", str_num, "in row ", minval))
      
      
    }
    else {
      print(paste(str_num, "structure wasnt impaired ever"))
      # RA_data[minval, c("fir_imp_yr")] <- 0
    }
    
  }
  return (RA_data)
}

find_first_year_of_impairment_erosion <- function (RA_data) {
  RA_data$fir_imp_yr_erosion <- 0 ##needs to be outside the for loop
  for (str_num in unique(RA_data$Num)) {
    #print(str_num)
    #finds how many rows the structure was iagnosed with 3 or 4
    impaired_rows <- which((RA_data$Erosion == 3 | RA_data$Erosion == 4 | RA_data$Erosion == 5) & RA_data$Num == str_num) #makes vector of which rows a specific structure was impaired or failed
    #print(length(impaired_rows))
    
    #if the length of that vector is 0 then it 
    if (length(impaired_rows > 0)) {
      #print(c("This structure was impaired or failed"))
      minval <- min(impaired_rows)
      #print(minval)
      RA_data[minval, c("fir_imp_yr_erosion")] <- 1
      print(paste("first impaired erosion year: ", RA_data$Year[minval], "entered for structure ", str_num, "in row ", minval))
      
      
    }
    else {
      print(paste(str_num, "structure wasnt impaired by erosion ever"))
      # RA_data[minval, c("fir_imp_yr")] <- 0
    }
    
  }
  return (RA_data)
}

find_first_year_of_impairment_deposition <- function (RA_data) {
  RA_data$fir_imp_yr_deposition <- 0 ##needs to be outside the for loop
  for (str_num in unique(RA_data$Num)) {
    #print(str_num)
    #finds how many rows the structure was iagnosed with 3 or 4
    impaired_rows <- which((RA_data$Deposition == 3 | RA_data$Deposition == 4| RA_data$Deposition == 5) & RA_data$Num == str_num) #makes vector of which rows a specific structure was impaired or failed
    #print(length(impaired_rows))
    
    #if the length of that vector is 0 then it 
    if (length(impaired_rows > 0)) {
      #print(c("This structure was impaired or failed"))
      minval <- min(impaired_rows)
      #print(minval)
      RA_data[minval, c("fir_imp_yr_deposition")] <- 1
      print(paste("first impaired Deposition year: ", RA_data$Year[minval], "entered for", str_num, "in row ", minval))
      
      
    }
    else {
      print(paste(str_num, "structure wasnt impaired Deposition ever"))
      # RA_data[minval, c("fir_imp_yr")] <- 0
    }
    
  }
  return (RA_data)
}

find_first_year_of_impairment_Rootwad <- function (RA_data) {
  RA_data$fir_imp_yr_rootwad <- 0 ##needs to be outside the for loop
  for (str_num in unique(RA_data$Num)) {
    #print(str_num)
    #finds how many rows the structure was iagnosed with 3 or 4
    impaired_rows <- which((RA_data$Rootwad_Performance == 3 | RA_data$Rootwad_Performance == 4 | RA_data$Rootwad_Performance == 5) & RA_data$Num == str_num) #makes vector of which rows a specific structure was impaired or failed
    #print(length(impaired_rows))
    
    #if the length of that vector is 0 then it 
    if (length(impaired_rows > 0)) {
      #print(c("This structure was impaired or failed"))
      minval <- min(impaired_rows)
      #print(minval)
      RA_data[minval, c("fir_imp_yr_rootwad")] <- 1
      print(paste("first impaired Rootwad year: ", RA_data$Year[minval], "entered for", str_num, "in row ", minval))
      
      
    }
    else {
      print(paste(str_num, "structure wasnt impaired Rootwad ever"))
      # RA_data[minval, c("fir_imp_yr")] <- 0
    }
    
  }
  return (RA_data)
}

##DOESN"T work but is possibly worth exploring more to replace/modify the above function to be cleaner
# for (str_num in x$Num) {
#   impaired_rows <- which((x$Integrity == 3 | x$Integrity == 4) & x$Num == str_num) #makes vector of which rows a specific structure was impaired or failed
#   print(length(impaired_rows))
#   x3 <- x %>%
#     mutate(first_impaired_yr = case_when(
#       length(impaired_rows) == 0 | (x[str_num,c("Num")] != min(impaired_rows)) ~ 0,
#       length(impaired_rows) > 0 & (x[str_num,c("Num")]== min(impaired_rows)) ~ 1
#     ))
#   
# }


y <- find_first_year_of_impairment_integrity(x)
y <- find_first_year_of_impairment_deposition(y)
y <- find_first_year_of_impairment_erosion(y)
y <- find_first_year_of_impairment_Rootwad(y)

# y1 <- y %>%
#   filter(Structure_Type == "log vane")

#another way to find total structures given a 3 or 4 during sometime during assessment
#this gives the total number of log vanes issued a 3 or 4 at some point; repeat values (ex, >1 year issued a 3) aren't counted since I'm counting the row created by my function
# y %>%
#   group_by(Year) %>%
#   count(fir_imp_yr_integrity == 1)

#names(y)["fir_imp_year_integrity"]

# Summary of ratings by year ----------------------------------------------


###integrity: gives first years a structure was impaired (3 or 4)

#| fir_imp_yr_deposition == TRUE | fir_imp_yr_erosion == TRUE | fir_imp_yr_rootwad == TRUE
  # count(fir_imp_yr_integrity == 1 | fir_imp_yr_deposition == 1 |
  #         fir_imp_yr_erosion == 1 |
  #         fir_imp_yr_rootwad == 1)

# y1 <- y %>%
#   group_by(Year) %>%
#   count(fir_imp_yr_integrity == 1,
#         fir_imp_yr_deposition == 1,
#         fir_imp_yr_erosion == 1,
#         fir_imp_yr_rootwad == 1)
# ,
# fir_imp_yr_deposition == 1,
# fir_imp_yr_erosion == 1,
# fir_imp_yr_rootwad == 1

integrity1 <- y %>%
  filter(`fir_imp_yr_integrity` == TRUE ) %>%
  group_by(Year) %>%
  count(fir_imp_yr_integrity == 1) %>%
  mutate(Percent = round(100 * (n / length(unique(y$Num))),2)) #adding this row will get a percentage of all structures


integrity2 <- integrity1 %>%
  select(-n) %>%
  pivot_wider(names_from = Year, values_from = Percent)

###Deposition
deposition1 <- y %>%
  filter(fir_imp_yr_deposition == TRUE ) %>%
  group_by(Year) %>%
  count(fir_imp_yr_deposition == 1) %>%
  mutate(Percent = round(100 * (n / length(unique(y$Num))),2)) #adding this row will get a percentage of all structures

deposition2 <- deposition1 %>%
  select(-n) %>%
  pivot_wider(names_from = Year, values_from = Percent)

###erosion
erosion1 <- y %>%
  filter(fir_imp_yr_erosion == TRUE ) %>%
  group_by(Year) %>%
  count(fir_imp_yr_erosion == 1) %>%
  mutate(Percent = round(100 * (n / length(unique(y$Num))),2)) #adding this row will get a percentage of all structures

erosion2 <- erosion1 %>%
  select(-n) %>%
  pivot_wider(names_from = Year, values_from = Percent)

###rootwad
rtwads <- y %>%
  filter(!is.na(Rootwad_Performance))
rtwads_structures <- length(unique(rtwads$Num))


rootwad1 <- y %>%
  filter(fir_imp_yr_rootwad == TRUE ) %>%
  group_by(Year) %>%
  count(fir_imp_yr_rootwad == 1) %>%
  mutate(Percent = round(100 * (n / rtwads_structures),2)) #adding this row will get a percentage of all structures



rootwad2 <- rootwad1 %>%
  select(-n) %>%
  pivot_wider(names_from = Year, values_from = Percent)

#binding rows
x <- bind_rows(integrity2, deposition2, erosion2, rootwad2)
x <- x %>%
  mutate(Ratings = case_when(`fir_imp_yr_integrity == 1`== TRUE ~ "Integrity",
                   `fir_imp_yr_deposition == 1`== TRUE ~ "Deposition",
                   `fir_imp_yr_erosion == 1`== TRUE ~ "Erosion",
                   `fir_imp_yr_rootwad == 1`== TRUE ~ "Rootwad"))

x1 <- x %>%
  select(Ratings, `2014`,`2015`, `2016`,`2017`,`2018`,`2020`)

x1[is.na(x1)] <- 0
x1 <- x1 %>%
  mutate(notes = "ratings >= 3")

# Intact Structures -------------------------------------------------------
##simplest way to gt just the numbers for totals: haven't converted to table form yet
x1 <- x %>%
  filter(!is.na(Integrity) | !is.na(Deposition) | !is.na(Erosion)) %>%
  group_by(Structure_Type, Num) %>%
  summarise(max_int_score = max(Integrity),
            max_dep_score = max(Deposition),
            max_ero_score = max(Erosion)) 

x2 <- x1 %>%
  mutate(int_intact = ifelse(max_int_score < 3, 1,0),
         dep_intact = ifelse(max_dep_score < 3, 1,0),
         ero_intact = ifelse(max_ero_score < 3, 1,0)
  ) %>%
  group_by(Structure_Type) %>%
  summarize(unique_str = n(),
            intact_integrity = sum(int_intact) / unique_str,
            intact_dep = sum(dep_intact) / unique_str,
            intact_ero = sum(ero_intact) / unique_str,
  )



#rootwad
x1 <- x %>%
  filter(!is.na(Rootwad_Performance)) %>%
  group_by(Structure_Type, Num) %>%
  summarize(max_roo_score = max(Rootwad_Performance)) %>%
  mutate(roo_intact = ifelse(max_roo_score < 3, 1, 0)) %>%
  group_by(Structure_Type) %>%
  summarize(unique_str = n(),
            intact_roo = sum(roo_intact) / unique_str)

#convoluted way like a dummy
###Structures still intact: integrity
still_intact_integrity <- y %>%
  filter(!is.na(Integrity)) %>%
  mutate(still_intact = case_when(
    Integrity == 1 | Integrity == 2 ~ 1,
    Integrity == 3 | Integrity == 4 ~ 0
  )) %>%
  count(Year, still_intact) 

x4 <- still_intact_integrity %>%
  group_by(Year) %>%
  summarize(total_structures = sum(n)) %>%
  ungroup()

still_intact_integrity2 <- left_join(still_intact_integrity, x4, by = "Year")
  

#mutate(Percent = round(100*(n / length(unique(RA$Num))),2))
#final [ivot wider table to later join]
still_intact_integrity3 <- still_intact_integrity2 %>%
  mutate(Percent = round(100*(n/total_structures),2)) %>%
  filter(still_intact == 1) %>%
  select(-n,-total_structures) %>%
  pivot_wider(names_from = Year,values_from = Percent)

###Depositon
still_intact_Deposition <- y %>%
  filter(!is.na(Deposition)) %>%
  mutate(still_intact = case_when(
    Deposition == 0 | Deposition == 1 | Deposition == 2 ~ 1,
    Deposition == 3 | Deposition == 4 | Deposition == 5 ~ 0
  )) %>%
  count(Year, still_intact) 

x4 <- still_intact_Deposition %>%
  group_by(Year) %>%
  summarize(total_structures = sum(n)) %>%
  ungroup()

still_intact_Deposition2 <- left_join(still_intact_Deposition, x4, by = "Year")


#final pivot wider table to later join
still_intact_Deposition3 <- still_intact_Deposition2 %>%
  mutate(Percent = round(100*(n/total_structures),2)) %>%
  filter(still_intact == 1) %>%
  select(-n,-total_structures) %>%
  pivot_wider(names_from = Year,values_from = Percent)

###Depositon
still_intact_Erosion <- y %>%
  filter(!is.na(Erosion)) %>%
  mutate(still_intact = case_when(
    Erosion == 0 | Erosion == 1 | Erosion == 2 ~ 1,
    Erosion == 3 | Erosion == 4 | Erosion == 5 ~ 0
  )) %>%
  count(Year, still_intact) 

x4 <- still_intact_Erosion %>%
  group_by(Year) %>%
  summarize(total_structures = sum(n)) %>%
  ungroup()

still_intact_Erosion2 <- left_join(still_intact_Erosion, x4, by = "Year")


#final pivot wider table to later join
still_intact_Erosion3 <- still_intact_Erosion2 %>%
  mutate(Percent = round(100*(n/total_structures),2)) %>%
  filter(still_intact == 1) %>%
  select(-n,-total_structures) %>%
  pivot_wider(names_from = Year,values_from = Percent)

###Rootwad
still_intact_rootwad <- y %>%
  filter(!is.na(Rootwad_Performance)) %>%
  mutate(still_intact = case_when(
    Rootwad_Performance == 0 | Rootwad_Performance == 1 | Rootwad_Performance == 2 ~ 1,
    Rootwad_Performance == 3 | Rootwad_Performance == 4 | Rootwad_Performance == 5 ~ 0
  )) %>%
  count(Year, still_intact) 

#don't need this part; theres 22 structures measured every year for wood toe
# x4 <- still_intact_rootwad %>%
#   group_by(Year) %>%
#   summarize(total_structures = sum(n)) %>%
#   ungroup()
# 
# still_intact_rootwad2 <- left_join(still_intact_rootwad, x4, by = "Year")


#final pivot wider table to later join
#case here where rootwad got a 4 then dropped back down; structure 75
still_intact_rootwad3 <- still_intact_rootwad2 %>%
  mutate(Percent = round(100*(n/rtwads_structures),2)) %>%
  filter(still_intact == 1) %>%
  select(-n,-total_structures) %>%
  pivot_wider(names_from = Year,values_from = Percent)


#binding rows
x <- bind_rows(still_intact_integrity3, still_intact_Deposition3, still_intact_Erosion3, still_intact_rootwad3)
x$still_intact[1] <- "Integrity"
x$still_intact[2] <- "Deposition"
x$still_intact[3] <- "Erosion"
x$still_intact[4] <- "Rootwad"
names(x)[1] <- "Rating"



# x <- x %>%
#   mutate(Ratings = case_when(`fir_imp_yr_integrity == 1`== TRUE ~ "Integrity",
#                              `fir_imp_yr_deposition == 1`== TRUE ~ "Deposition",
#                              `fir_imp_yr_erosion == 1`== TRUE ~ "Erosion",
#                              `fir_imp_yr_rootwad == 1`== TRUE ~ "Rootwad"))

x1 <- x %>%
  select(Ratings, `2014`,`2015`, `2016`,`2017`,`2018`,`2020`)

x1[is.na(x1)] <- 0
x1 <- x1 %>%
  mutate(notes = "ratings >= 3")

RA %>%
  filter(!is.na(Integrity)) %>%
  count(Structure_Type, Year)

# Summary of ratings by structure type ------------------------------------
## intact
y <- RA
still_intact_integrity_structures <- y %>%
  filter(!is.na(Integrity)) %>%
  mutate(still_intact = case_when(
    Integrity == 1 | Integrity == 2 ~ 1,
    Integrity == 3 | Integrity == 4 ~ 0
  )) %>%
  count(Year, still_intact, Structure_Type) 
#gives how many structures of each type there were in a given year
x4 <- still_intact_integrity_structures %>%
  group_by(Year, Structure_Type) %>%
  summarize(total_structures = sum(n)) %>%
  ungroup()

still_intact_integrity_structures2 <- left_join(still_intact_integrity_structures, x4, by = c("Year", "Structure_Type"))


#mutate(Percent = round(100*(n / length(unique(RA$Num))),2))
#final [ivot wider table to later join]
still_intact_integrity_structures3 <- still_intact_integrity_structures2 %>%
  mutate(Percent = round(100*(n/total_structures),2)) %>%
  filter(still_intact == 1) 

still_intact_integrity_structures3 <- still_intact_integrity_structures3 %>%
  select(-n,-total_structures) %>%
  pivot_wider(names_from = Year,values_from = Percent)

##Deposition

still_intact_Deposition_structures <- y %>%
  filter(!is.na(Deposition)) %>%
  mutate(still_intact = case_when(
    Deposition == 0 | Deposition == 1 | Deposition == 2 ~ 1,
    Deposition == 3 | Deposition == 4 | Deposition == 5 ~ 0
  )) %>%
  count(Year, still_intact, Structure_Type) 

x4 <- still_intact_Deposition_structures %>%
  group_by(Year, Structure_Type) %>%
  summarize(total_structures = sum(n)) %>%
  ungroup()

still_intact_Deposition_structures2 <- left_join(still_intact_Deposition_structures, x4, by = c("Year", "Structure_Type"))


#mutate(Percent = round(100*(n / length(unique(RA$Num))),2))
#final [ivot wider table to later join]
still_intact_Deposition_structures3 <- still_intact_Deposition_structures2 %>%
  mutate(Percent = round(100*(n/total_structures),2)) %>%
  filter(still_intact == 1) %>%
  select(-n,-total_structures) %>%
  pivot_wider(names_from = Year,values_from = Percent)

##Erosion

still_intact_Erosion_structures <- y %>%
  filter(!is.na(Erosion)) %>%
  mutate(still_intact = case_when(
    Erosion == 0 | Erosion == 1 | Erosion == 2 ~ 1,
    Erosion == 3 | Erosion == 4 | Erosion == 5 ~ 0
  )) %>%
  count(Year, still_intact, Structure_Type) 

x4 <- still_intact_Erosion_structures %>%
  group_by(Year, Structure_Type) %>%
  summarize(total_structures = sum(n)) %>%
  ungroup()

still_intact_Erosion_structures2 <- left_join(still_intact_Erosion_structures, x4, by = c("Year", "Structure_Type"))


#mutate(Percent = round(100*(n / length(unique(RA$Num))),2))
#final [ivot wider table to later join]
still_intact_Erosion_structures3 <- still_intact_Erosion_structures2 %>%
  mutate(Percent = round(100*(n/total_structures),2)) %>%
  filter(still_intact == 1) %>%
  select(-n,-total_structures) %>%
  pivot_wider(names_from = Year,values_from = Percent)

#rootwad
still_intact_Rootwad_structures <- y %>%
  filter(!is.na(Rootwad_Performance)) %>%
  mutate(still_intact = case_when(
    Rootwad_Performance == 0 | Rootwad_Performance == 1 | Rootwad_Performance == 2 ~ 1,
    Rootwad_Performance == 3 | Rootwad_Performance == 4 | Rootwad_Performance == 5 ~ 0
  )) %>%
  count(Year, still_intact, Structure_Type) 

# x4 <- still_intact_Rootwad_structures %>%
#   group_by(Year, Structure_Type) %>%
#   summarize(total_structures = sum(n)) %>%
#   ungroup()
# 
# still_intact_Rootwad_structures2 <- left_join(still_intact_Rootwad_structures, x4, by = c("Year", "Structure_Type"))


#mutate(Percent = round(100*(n / length(unique(RA$Num))),2))
#final [ivot wider table to later join]
still_intact_Rootwad_structures3 <- still_intact_Rootwad_structures %>%
  mutate(Percent = round(100*(n/rtwads_structures),2)) %>%
  filter(still_intact == 1) %>%
  select(-n,) %>%
  pivot_wider(names_from = Year,values_from = Percent)


## Summary column


#INTEGIRTY SUMMARY COLUMN
# x <- RA %>%
#   mutate(failed = ifelse((Integrity >= 3), 1, 0))   # | is OR operator; Integrity == 3 | if we want to do "impaired" instead of failed

x <- RA

x <- find_first_year_of_impairment_integrity(x)
x1 <- x %>%
  group_by(Year, Structure_Type) %>%
  summarise(total_str_num = length(unique(Num)),
            total_impaired = sum(fir_imp_yr_integrity))
#checking to make sure we have 137 total structures
test1 <- x1 %>%
  ungroup() %>%
  select(-Year,-total_impaired) %>%
  distinct()
sum(test1$total_str_num)

# group_by(Structure_Type) %>%
# summarize(sum1 = sum(total_str_num))
integrity_structures_sum <- x1 %>%
  group_by(Structure_Type) %>%
  summarize(total_allyrs = sum(total_impaired),
            total_intact_structures = (total_str_num - total_allyrs),
            Percent_Intact = round(100*(total_intact_structures / total_str_num),2)) %>%
  distinct() %>%
  select(-total_allyrs, -total_intact_structures)

still_intact_integrity_structures4 <- left_join(still_intact_integrity_structures3, integrity_structures_sum, by = "Structure_Type")

still_intact_integrity_structures4 <- still_intact_integrity_structures4 %>%
  select(-still_intact)


#DEPOSITION SUMMARY COLUMN
x <- RA

x <- find_first_year_of_impairment_deposition(x)
x1 <- x %>%
  group_by(Year, Structure_Type) %>%
  summarise(total_str_num = length(unique(Num)),
            total_impaired = sum(fir_imp_yr_deposition))

deposition_structures_sum <- x1 %>%
  group_by(Structure_Type) %>%
  summarize(total_allyrs = sum(total_impaired),
            total_intact_structures = (total_str_num - total_allyrs),
            percentage = round(100*(total_intact_structures / total_str_num),2)) %>%
  distinct() %>%
  select(-total_allyrs, -total_intact_structures)

still_intact_Deposition_structures4 <- left_join(still_intact_Deposition_structures3, deposition_structures_sum, by = "Structure_Type")

still_intact_Deposition_structures4 <- still_intact_Deposition_structures4 %>%
  select(-still_intact)

#EROSION SUMMARY COLUMN
x <- RA

x <- find_first_year_of_impairment_erosion(x)
x1 <- x %>%
  group_by(Year, Structure_Type) %>%
  summarise(total_str_num = length(unique(Num)),
            total_impaired = sum(fir_imp_yr_erosion))

erosion_structures_sum <- x1 %>%
  group_by(Structure_Type) %>%
  summarize(total_allyrs = sum(total_impaired),
            total_intact_structures = (total_str_num - total_allyrs),
            percentage = round(100*(total_intact_structures / total_str_num),2)) %>%
  distinct() %>%
  select(-total_allyrs, -total_intact_structures)

still_intact_Erosion_structures4 <- left_join(still_intact_Erosion_structures3, erosion_structures_sum, by = "Structure_Type")

still_intact_Erosion_structures4 <- still_intact_Erosion_structures4 %>%
  select(-still_intact)

#ROOTWAD SUMMARY COLUMN
x <- find_first_year_of_impairment_Rootwad(x)
x1 <- x %>%
  group_by(Year, Structure_Type) %>%
  summarise(total_str_num = length(unique(Num)),
            total_impaired = sum(fir_imp_yr_rootwad))

rootwad_structures_sum <- x1 %>%
  group_by(Structure_Type) %>%
  summarize(total_allyrs = sum(total_impaired),
            total_intact_structures = (total_str_num - total_allyrs),
            percentage = round(100*(total_intact_structures / total_str_num),2)) %>%
  distinct() %>%
  select(-total_allyrs, -total_intact_structures)

still_intact_Rootwad_structures4 <- left_join(still_intact_Rootwad_structures3, rootwad_structures_sum, by = "Structure_Type")

still_intact_Rootwad_structures4 <- still_intact_Rootwad_structures4 %>%
  select(-still_intact)


##Failures ----------
##total structures that failed at a given time

#integrity
x <- find_first_year_of_impairment_integrity(x)
x1 <- x %>%
  group_by(Year, Structure_Type) %>%
  summarise(total_str_num = length(unique(Num)),
            total_impaired = sum(fir_imp_yr_integrity))



x2 <- x1 %>%
  mutate(Percent = round(100*(total_impaired/total_str_num),2))

x3 <- x2 %>%
  select(-total_str_num, -total_impaired) %>%
  pivot_wider(names_from = Year, values_from = Percent)
x3 <- x3 %>%
  mutate(Notes = "ratings >= 3")

##Depostion
x <- RA
x <- find_first_year_of_impairment_deposition(x)
x1 <- x %>%
  group_by(Year, Structure_Type) %>%
  summarise(total_str_num = length(unique(Num)),
            total_impaired = sum(fir_imp_yr_deposition))

x2 <- x1 %>%
  mutate(Percent = round(100*(total_impaired/total_str_num),2))

x3 <- x2 %>%
  select(-total_str_num, -total_impaired) %>%
  pivot_wider(names_from = Year, values_from = Percent)
x3 <- x3 %>%
  mutate(Notes = "ratings >= 3")

###Erosoin
x <- RA
x <- find_first_year_of_impairment_erosion(x)
x1 <- x %>%
  group_by(Year, Structure_Type) %>%
  summarise(total_str_num = length(unique(Num)),
            total_impaired = sum(fir_imp_yr_erosion))

x2 <- x1 %>%
  mutate(Percent = round(100*(total_impaired/total_str_num),2))

x3 <- x2 %>%
  select(-total_str_num, -total_impaired) %>%
  pivot_wider(names_from = Year, values_from = Percent)
x3 <- x3 %>%
  mutate(Notes = "ratings >= 3")

##Rootwads
x <- RA
rtwads <- x %>%
  filter(!is.na(Rootwad_Performance))
rtwads_structures <- length(unique(rtwads$Num))

x <- find_first_year_of_impairment_Rootwad(rtwads)


x1 <- x %>%
  group_by(Year, Structure_Type) %>%
  summarise(total_str_num = length(unique(Num)),
            total_impaired = sum(fir_imp_yr_rootwad))

x2 <- x1 %>%
  mutate(Percent = round(100*(total_impaired/total_str_num),2))

x3 <- x2 %>%
  select(-total_str_num, -total_impaired) %>%
  pivot_wider(names_from = Year, values_from = Percent)
x3 <- x3 %>%
  mutate(Notes = "ratings >= 3")
#this 
length(unique(y1$Num))

###max scores across years
x <- RA
max_structure_scores <- x %>%
  filter(!is.na(Integrity)) %>%
  group_by(Structure_Type, Year) %>%
  summarise(max_int_score = max(Integrity))

max_structure_scores %>%
  ggplot(aes(x = Year, y = max_int_score, fill = Structure_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  labs(title = "Max Integrity Scores for Structures Across Years", x = "Year", y = "Max Integrity Score")

###Integrity andfucntion of all structures
x <- RA
# x1 <- x %>%
#   group_by(Integrity, Year) %>%
#   summarise(total = sum())

x1 <- x %>%
  filter(!is.na(Integrity)) %>%
  count(Integrity, Year)
  #mutate(Percent = round(100*(n / length(unique(RA$Num))),2))
x2 <- x1 %>%
  #select(-n) %>%
  pivot_wider(names_from = Year,values_from = n)

x2[is.na(x2)] <- 0

##Depositon 
x1 <- x %>%
  filter(!is.na(Deposition)) %>%
  count(Deposition, Year)
  #mutate(Percent = round(100*(n / length(unique(RA$Num))),2))
x2 <- x1 %>%
  #select(-n) %>%
  pivot_wider(names_from = Year,values_from = n)

x2[is.na(x2)] <- 0

##Erosion 
x1 <- x %>%
  filter(!is.na(Erosion)) %>%
  count(Erosion, Year)
  #mutate(Percent = round(100*(n / length(unique(RA$Num))),2))
x2 <- x1 %>%
  #select(-n) %>%
  pivot_wider(names_from = Year,values_from = n)

x2[is.na(x2)] <- 0

# rootwads
x1 <- x %>%
  filter(!is.na(Rootwad_Performance)) %>%
  count(Rootwad_Performance, Year)
  #mutate(Percent = round(100*(n / rtwads_structures),2))
x2 <- x1 %>%
  #select(-n) %>%
  pivot_wider(names_from = Year,values_from = n)

x2[is.na(x2)] <- 0

##

# str_num = 68
# impaired_rows <- which((x$Integrity == 3 | x$Integrity == 4) & x$Num == str_num) #makes vector of which rows a specific structure was impaired or failed
# if (length(impaired_rows == 0)) {
#   print(c("True"))
# }
#bar plot for when structures were diagnosed with 3's or 4's for the first time. denomitor is total number of structures  
y %>%
  group_by(Year, Structure_Type) %>%
  summarise(new_impaired = sum(fir_imp_yr), perc_new_impaired = (100*(new_impaired/(length(unique(y$Num)))))) %>%
  ggplot(aes(x = Year, y = perc_new_impaired)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = Structure_Type)) +
  theme_classic() +
  labs(title = "New Structure 3s and 4s by Year")

#bar plot for when structures were diagnosed with 3's or 4's for the first time. denomitor is total number of that type of structure  
#difference in code is y$Num (which will divide by the total number of structures ever) vs Num, which when used in the summarise function, just totalls the number of structures within each year, of each type, since that's what they're grouped by
y %>%
  group_by(Year, Structure_Type) %>%
  summarise(new_impaired = sum(fir_imp_yr), total_of_that_type = length(unique(Num)), perc_new_impaired = (100*new_impaired/length(unique(Num)))) %>%
  ggplot(aes(x = Year, y = perc_new_impaired)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = Structure_Type)) +
  theme_classic() +
  labs(title = "New Structure 3s and 4s by Year")
  

# Max Structure Scores ----------------------------------------------------
x <- RA
x1 <- x %>%
  filter(!is.na(Integrity) | !is.na(Deposition) | !is.na(Erosion)) %>%
  select(Num, Structure_Type, Year, Integrity)
###INTEGRITY
###total summarized for all structures
total <- x1 %>%
  group_by(Structure_Type, Num) %>%
  summarize(max_int = max(Integrity))

total2 <- total %>%
  ungroup() %>%
  count(max_int) %>%
  rename(Integrity = max_int,
         Max = n) 

#year by year
x2 <- x1 %>%
  pivot_wider(names_from = Year, values_from = Integrity) %>%
  rowwise() %>%
  mutate(max_int = max(c(`2014`,`2015`,`2016`,`2017`,`2018`,`2020`)))


x2 <- x %>%
  filter(!is.na(Integrity) | !is.na(Deposition) | !is.na(Erosion)) %>%
  count(Integrity, Year) %>%
  pivot_wider(names_from = Year, values_from = n)

# x3 <- x %>%
#   filter(!is.na(Integrity) | !is.na(Deposition) | !is.na(Erosion)) %>%
#   count(Integrity) 

all_structures_max_summarized <- left_join(x2, total2, by = "Integrity")
all_structures_max_summarized[is.na(all_structures_max_summarized)] <- 0


###DEPOSITOIN

x <- RA
x1 <- x %>%
  filter(!is.na(Integrity) | !is.na(Deposition) | !is.na(Erosion)) %>%
  select(Num, Structure_Type, Year, Deposition)
###total summarized for all structures
total <- x1 %>%
  group_by(Structure_Type, Num) %>%
  summarize(max_dep = max(Deposition))

total2 <- total %>%
  ungroup() %>%
  count(max_dep) %>%
  rename(Deposition = max_dep,
         Max = n) 

#year by year
# x2 <- x1 %>%
#   pivot_wider(names_from = Year, values_from = Deposition) %>%
#   rowwise() %>%
#   mutate(max_int = max(c(`2014`,`2015`,`2016`,`2017`,`2018`,`2020`)))


x2 <- x %>%
  filter(!is.na(Integrity) | !is.na(Deposition) | !is.na(Erosion)) %>%
  count(Deposition, Year) %>%
  pivot_wider(names_from = Year, values_from = n)

# x3 <- x %>%
#   filter(!is.na(Integrity) | !is.na(Deposition) | !is.na(Erosion)) %>%
#   count(Integrity) 

all_structures_max_summarized <- left_join(x2, total2, by = "Deposition")
all_structures_max_summarized[is.na(all_structures_max_summarized)] <- 0

###Erosion

x <- RA
x1 <- x %>%
  filter(!is.na(Integrity) | !is.na(Deposition) | !is.na(Erosion)) %>%
  select(Num, Structure_Type, Year, Erosion)
###total summarized for all structures
total <- x1 %>%
  group_by(Structure_Type, Num) %>%
  summarize(max_ero = max(Erosion))

total2 <- total %>%
  ungroup() %>%
  count(max_ero) %>%
  rename(Erosion = max_ero,
         Max = n) 

#year by year
# x2 <- x1 %>%
#   pivot_wider(names_from = Year, values_from = Erosion) %>%
#   rowwise() %>%
#   mutate(max_int = max(c(`2014`,`2015`,`2016`,`2017`,`2018`,`2020`)))


x2 <- x %>%
  filter(!is.na(Integrity) | !is.na(Deposition) | !is.na(Erosion)) %>%
  count(Erosion, Year) %>%
  pivot_wider(names_from = Year, values_from = n)

# x3 <- x %>%
#   filter(!is.na(Integrity) | !is.na(Deposition) | !is.na(Erosion)) %>%
#   count(Integrity) 

all_structures_max_summarized <- left_join(x2, total2, by = "Erosion")
all_structures_max_summarized[is.na(all_structures_max_summarized)] <- 0


###Rootwad

x <- RA
x1 <- x %>%
  filter(!is.na(Rootwad_Performance)) %>%
  select(Num, Structure_Type, Year, Rootwad_Performance)
###total summarized for all structures
total <- x1 %>%
  group_by(Structure_Type, Num) %>%
  summarize(max_roo = max(Rootwad_Performance))

total2 <- total %>%
  ungroup() %>%
  count(max_roo) %>%
  rename(Rootwad_Performance = max_roo,
         Max = n) 

#year by year
# x2 <- x1 %>%
#   pivot_wider(names_from = Year, values_from = Rootwad_Performance) %>%
#   rowwise() %>%
#   mutate(max_int = max(c(`2014`,`2015`,`2016`,`2017`,`2018`,`2020`)))


x2 <- x %>%
  filter(!is.na(Rootwad_Performance)) %>%
  count(Rootwad_Performance, Year) %>%
  pivot_wider(names_from = Year, values_from = n)

# x3 <- x %>%
#   filter(!is.na(Integrity) | !is.na(Deposition) | !is.na(Erosion)) %>%
#   count(Integrity) 

all_structures_max_summarized <- left_join(x2, total2, by = "Rootwad_Performance")
all_structures_max_summarized[is.na(all_structures_max_summarized)] <- 0

# RA Table making ---------------------------------------------------------

  
###Table making
x <- RA %>%
  mutate(YAC = Year-2013, #gets number of years after 2013 the structure is
         impaired_or_damaged = ifelse((Integrity == 4 | Integrity == 3), 1, 0))

x1 <- x %>%
  filter(Year <= 2016, x$impaired_or_damaged == 1)
x2 <- RA %>%
  filter( Integrity == 3 |Integrity == 4)
length(unique(x2$Num))
  
length(unique(x1$Num)) #total number of structure given a 3 or 4 at some point in time during assesment: 36. (20 of which were given 4)

#gets the status of all structures in 2020: 
x1 <- x %>%
  filter(Year == 2020) %>%
  select(Integrity) 
  
x2 <- data.frame(table(unlist(x1)))
names(x2)[1] <- "Integrity Score in 2020"
names(x2)[2] <- "Total Number of Structures"

x2$Percent <- round(100*(x2$`Total Number of Structures`/length(unique(RA$Num))),2)

  # group_by(Integrity) %>%
  # summarise(total = tally(Integrity))
####Visualizing CLD's
RA %>%
  ggplot(aes(x = Structure_Type, y = Integrity)) +
  geom_boxplot() +
  theme_classic()

###Assumptions testing
#normal distribution
#a normal distribution means yo uget a P value >.05
shapiro.test(RA$Integrity)
#equal variance
#variances that are equal will get a p value > .05
#can't do that on categorical variables
bartlett.test(RA$Structure_Type~RA$Integrity)

# Log Vane ----------------------------------------------------------------

#need to know % of log vanes that scored 3 or above in riffle vs pools
#also break down log vane failures (integiry >= 3) by year for riffle vs pool
###Log Vane
LV <- read.csv(file.choose())
#figure without removed structures
ggplot(data=LV, aes(x=as.factor(Year), y=as.numeric(LV_Angle), colour=as.factor(Integrity)), fill="gray")+
  stat_boxplot(geom = "errorbar")+
  stat_boxplot(geom = "boxplot")+
  scale_color_manual(limits=c("1","2","3","4"), labels=c("Intact", "Damaged","Impaired","Failed"), values= c("1"="chartreuse4", "2"="goldenrod1","3"="darkorange2","4"="firebrick3"))+
  labs(title="Log Vane Angle - Integrity", y="Departure Angle(?)", x="Year")+
  plot_theme


#gets table of all structures with structres that weren't removed;
#removes 3 strctures by this method
max_scores <- LV %>%
  filter(Integrity != "Removed") %>%
  group_by(Num, LV_Angle, LV_Slope,Associated_Residual_Pool,Morphology_Pre) %>%
  summarise(max_int_score = max(Integrity))
  #filter(max_int_score != "Removed")

#18 total structures given 3's or 4's at some point
max_scores %>%
  group_by(max_int_score) %>%
  count(max_int_score)

max_scores %>%
  ggplot(aes(x = max_int_score, y = LV_Slope, color = Morphology_Pre)) +
  geom_boxplot() +
  theme_classic() +
  scale_colour_grey() +
  labs(title = "Max Integrity Score vs LV Slope")+
  xlab("Maximum Integrity Score") +
  ylab("Log Vane Slope")

max_scores %>%
  ggplot(aes(x = max_int_score, fill = Morphology_Pre)) +
  geom_histogram(stat = "count", position = "dodge") +
  theme_classic() +
  scale_colour_grey() +
  labs(title = "Morpholgy_Pre Max Integrity Scores")+
  xlab("Maximum Integrity Score") +
  ylab("Count")

#gives morphology Pre failure and success over all years of monitoring
max_scores %>%
  mutate(failed = case_when(
    max_int_score == 3 | max_int_score == 4 ~ TRUE,
    max_int_score == 1 | max_int_score == 2 ~ FALSE
  )) %>%
  group_by(Morphology_Pre, failed) %>%
  summarize(percent = 100* (n()/length(unique(max_scores$Num))))
# # just raw numbers now
# max_scores %>%
#   mutate(failed = case_when(
#     max_int_score == 3 | max_int_score == 4 ~ TRUE,
#     max_int_score == 1 | max_int_score == 2 ~ FALSE
#   )) %>%
#   group_by(Morphology_Pre, failed) %>%
#   summarize(percent = 100* (n()/length(unique(max_scores$Num))))

length(unique(LV$Num))


#breaking down impairments by year: 
#if  a structure is diagnosed as 4, it stays that way
#LV includes removed structures
LV <- find_first_year_of_impairment(LV)


y1 <- LV %>%
  group_by(Year, Morphology_Pre) %>%
  summarise(new_impaired = sum(fir_imp_yr_integrity), perc_new_impaired = round((100*(new_impaired/(length(unique(LV$Num))))),2))

y1 %>%
  ggplot(aes(x = Year, y = perc_new_impaired)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = Morphology_Pre)) +
  theme_classic() +
  labs(title = "New Structure 3s and 4s by Year", y = "% Newly Impaired", x = "Year")

y1 %>%
  select(-new_impaired) %>%
  pivot_wider(names_from = Year, values_from = perc_new_impaired)
#y2 <- data.frame(table(unlist(y1)))
# names(y1)[3] <- "Number"
# names(y1)[4] <- "Percent"
# 
# y1$Percent <- round(y1$Percent,2)


##Need to find first year when the LV was assessed wiht a 4

#morphology_pre and Integrity scores
#library(broom)
anova1 <- aov(max_scores$max_int_score~max_scores$Morphology_Pre)
summary(anova1)
#p value not below .05; integrity scores were not affected by departure angle
anova1 <- aov(max_scores$max_int_score~max_scores$LV_Angle)
summary(anova1)

#p value below .05; integrity scores were affected by LV slope
anova2 <- aov(max_scores$max_int_score~max_scores$LV_Slope)
summary(anova2)

#chi sq for 2 qualitatvie variables (integrity score and pre-existing morphllogy)
#as far as I can tell here, there was no significatn difference between pre-existing morphology and max integrity score given
x <- max_scores %>%
  filter(Morphology_Pre != "")
#makes contingency table of 2 qualitative variables strutcure type and integrity
#can also be done with xtabs() function
x.contingencytable<-table(x$Morphology_Pre,x$max_int_score)  
#error of "Chi-squared approximation may be incorrect" means that the smallest expected frequencies is lower than 5
#gather some levels (especially those with a small number of observations) to increase the number of observations in the subgroups, or
#use the Fisher's exact test

chi_1 <- chisq.test(x.contingencytable)
res2<-residuals(chi_1, "pearson") #by looking at residuals, we can tell where the differences are
# #tells where the observed frequencies deviate from xpected if the variables were independent; therefore showing where the variables influence each other
#look for extreme values >1.96 or <-1.96 (likely to be associated with that score vs not likely to be assoicated with that score)

#can't use anova for this because pre existing morphology was a qualitative variable
# anova3 <- aov(x$max_int_score~x$Morphology_Pre)
# summary(anova3)

#p value below .05; max integrity score was affected by morphology_pre, 
#for all structures that had a designation at leaast/had an associated pool
x <- max_scores %>%
  mutate(has_pool = (!is.na(Associated_Residual_Pool)))
anova3 <- aov(x$max_int_score~x$has_pool)
summary(anova3)

x1 <- x %>%
  filter(has_pool == FALSE)

test <- x1$Num

##actually I don't think we can use kruskal wallce now because indpendent variable has to be categorocial
#so i can do kruskal wallace for structure_type and max integriy score
###kruskal wallace test: non-parametric (doesn't make assumptions about distribution) version of ANOVA
#dependent variable: continuous but not normally distributed, or Ordinal; in this case, max intregrity score is ordinal
#independent variable: categorical
#ANOVA assumes normal distribution and equal variances
#anova is used with categorical data with ocnitnuous data
#test normal distribution with shapiro.test(variable) 
#if p value is greater than .05, the distribution is normal. run this for all variables  involved
#test equal variance with bartlett.test(variable~variable); asks "are the variances of these two variables the same?
#if p value is greater than .05,the variances are the same
#if these fail, can't do anova. do kruskal.test(dependent.variable~independent.variable) 
#p value less than .05 means they're signnificant
#then use pariwise.wilcox.test() to see where the differences are

#with a normal dataframe of LV, suggests that slope affects integirty scfore
kruskal.test(LV$Integrity~LV$LV_Slope)
#with a dataframe with just the max score a log vane was given, suggests that slope did not affect the max integrity score a LV was given
kruskal.test(max_scores$max_int_score~max_scores$LV_Slope)
#can't run anova like this because the dependent variable needs to be continous and independent variable needs to be categorical
x1 <- aov(max_scores$max_int_score~max_scores$LV_Slope)
summary(x1)
#says there are difs (p = .01) but we need tukey hsd to tell us where differences are
#tukey won't work unless independent variable is factor; needs 
TukeyHSD(x1)
#departure angle: as far as I can tell, no differnece in max integirty score vs departure angle
kruskal.test(max_scores$max_int_score~max_scores$LV_Angle)
#just doing an anova to see; no difference measured there either (p value > .05)
x1 <- aov(max_scores$max_int_score~max_scores$LV_Angle)
summary(x1)
#boxplot(as.numeric(max_scores$LV_Angle), as.numeric(max_scores$max_int_score))

###RPD
RPD_data <- read.csv(file,choose())
RPD1<-read.csv("UAR_ResidualPoolDepthAnalysis_2020_ver2.csv", na.strings = c("", " "))

#testing normal distribution
shapiro.test(RPD_data$RPD)
#this is easily seen in histogram
RPD_data %>%
  ggplot(aes(x = RPD)) +
  geom_histogram()

###but is we fitler out 0 values that are assigned when a pool was a riffle previously...
RPD_data2 <- RPD_data %>%
  filter(RPD > 0)
#it still fails the test ?\_(???)_/?
shapiro.test(RPD_data2$RPD)
#but its histogram looks a lot better

RPD1<-read.csv("test1.csv", na.strings = c("", " "))

#this part was filling in cells so I did't have to do it manually inexcel 

x <- read.csv(file.choose(), na.strings = ""," ")

# x <- RPD1 %>%
#   fill(Pool_Num.1,Pool_Type.1,Structure_Type, RA_Num,Morphology_Pre)
x <- fill(RPD1,4:7)

write.csv(x,"test2.csv")
