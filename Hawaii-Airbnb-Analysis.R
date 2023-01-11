
#                                           US AIRBNB ANALYSIS (with HAWAII CASE STUDY)
#                                     -----------------------------------------------------



#installing the required packages
install.packages('tidyverse') # collection of packages for data manipulation, exploration, and visualization(dplyr for transforming data)
#install.packages('lubridate') ----> used to parse, extract, update, and perform algebraic operations on date and time objects.
#install.packages('reshape') ----> allows to restructure and aggregate data between wide and long formats
install.packages('fitdistrplus')
install.packages('maps')
install.packages('mapproj')
install.packages('cowplot')
install.packages('ggpubr')
install.packages('fastDummies')
install.packages('asbio')
install.packages('BSDA')
install.packages('corrplot')

#1. Loading the packages using the library() function if they are already installed

library (BSDA) 
library(asbio)
library(tidyverse)
library(lubridate) 
library(reshape2)
library(ggplot2)
library(fitdistrplus)
library(maps)
library(dplyr)
library(mapproj)
library(cowplot)
library(ggpubr)
library(fastDummies)
library(corrplot)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#2. Set the working directory to location of the dataset
setwd('C:/Users/aishw/OneDrive/Documents/IE 6200 Lab Assignments')
getwd()

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#3.Importing the AirBnB dataset
airbnb_df<- read.table('AirBnB-US.csv', # the dataset to be imported
                       header = TRUE,   # the dataset contains names of its columns called as header names
                       fill= TRUE,
                       sep = ',')       # the field separator character ',' , values on each line of the file are separated by this character
# Checking the structure of the dataset
str(airbnb_df) 

head(airbnb_df)
tail(airbnb_df)

# DATA CLEANING
#----------------
airbnb_US_data= dplyr::select(airbnb_df, id, host_id, neighbourhood_group, neighbourhood, latitude, longitude, room_type,
                         minimum_nights, number_of_reviews, reviews_per_month, price, calculated_host_listings_count,
                         availability_365, city)


airbnb_clean_df<- dplyr::filter(airbnb_US_data, price <= 750)
head(airbnb_clean_df)

airbnb_clean_df$number_of_reviews <- ifelse(is.na(airbnb_clean_df$number_of_reviews),0,airbnb_clean_df$number_of_reviews)
airbnb_clean_df$id <- ifelse(is.na(airbnb_clean_df$id),0,airbnb_clean_df$id)
#airbnb_clean_df$name <- ifelse(is.na(airbnb_clean_df$name),0,airbnb_clean_df$name)
airbnb_clean_df$host_id <- ifelse(is.na(airbnb_clean_df$host_id),0,airbnb_clean_df$host_id)
#airbnb_clean_df$host_name <- ifelse(is.na(airbnb_clean_df$host_name),0,airbnb_clean_df$host_name)
airbnb_clean_df$neighbourhood_group <- ifelse(is.na(airbnb_clean_df$neighbourhood_group),0,airbnb_clean_df$neighbourhood_group)
airbnb_clean_df$neighbourhood <- ifelse(is.na(airbnb_clean_df$neighbourhood),0,airbnb_clean_df$neighbourhood)
airbnb_clean_df$latitude <- ifelse(is.na(airbnb_clean_df$latitude),0,airbnb_clean_df$latitude)
airbnb_clean_df$longitude <- ifelse(is.na(airbnb_clean_df$longitude),0,airbnb_clean_df$longitude)
airbnb_clean_df$room_type <- ifelse(is.na(airbnb_clean_df$room_type),0,airbnb_clean_df$room_type)
airbnb_clean_df$price <- ifelse(is.na(airbnb_clean_df$price),0,airbnb_clean_df$price)
airbnb_clean_df$minimum_nights <- ifelse(is.na(airbnb_clean_df$minimum_nights),0,airbnb_clean_df$minimum_nights)
#airbnb_clean_df$last_review <- ifelse(is.na(airbnb_clean_df$last_review),0,airbnb_clean_df$last_review)
airbnb_clean_df$reviews_per_month <- ifelse(is.na(airbnb_clean_df$reviews_per_month),0,airbnb_clean_df$reviews_per_month)
airbnb_clean_df$calculated_host_listings_count <- ifelse(is.na(airbnb_clean_df$calculated_host_listings_count),0,airbnb_clean_df$calculated_host_listings_count)
airbnb_clean_df$availability_365 <- ifelse(is.na(airbnb_clean_df$availability_365),0,airbnb_clean_df$availability_365)
airbnb_clean_df$city <- ifelse(is.na(airbnb_clean_df$city),0,airbnb_clean_df$city)

summary(airbnb_clean_df)

listings <- unique(airbnb_clean_df$id)
length(listings)

airbnb_clean_df$number_of_reviews <- as.numeric(airbnb_clean_df$number_of_reviews)
airbnb_clean_df$price <- as.numeric(airbnb_clean_df$price) ## Changed to numeric
airbnb_clean_df$availability_365 <- as.numeric(airbnb_clean_df$availability_365)
airbnb_clean_df$latitude <- as.numeric(airbnb_clean_df$latitude)
airbnb_clean_df$longitude <- as.numeric(airbnb_clean_df$longitude)
airbnb_clean_df$minimum_nights <- as.numeric(airbnb_clean_df$minimum_nights)
airbnb_clean_df$reviews_per_month <- as.numeric(airbnb_clean_df$reviews_per_month)

# DESCRIPTIVE STATISTICS FOR US CITIES
# ------------------------------------

airbnb_US= dplyr::select(airbnb_clean_df, id, host_id, neighbourhood_group,neighbourhood, latitude, longitude, room_type,
                             minimum_nights, number_of_reviews, reviews_per_month, price, calculated_host_listings_count,
                             availability_365, city)

airbnb_boston <- dplyr::filter(airbnb_US, city == 'Boston' & price <= 750)
airbnb_chicago <- dplyr::filter(airbnb_US, city == 'Chicago' & price <= 750)
airbnb_NYC <- dplyr::filter(airbnb_US, city == 'New York City' & price <= 750)
airbnb_LA <- dplyr::filter(airbnb_US, city == 'Los Angeles' & price <= 750)
airbnb_DC <- dplyr::filter(airbnb_US, city == 'Washington D.C.' & price <= 750)
airbnb_Hawaii <- dplyr::filter(airbnb_US, city == 'Hawaii' & price <= 750)

airbnb_clean_cities <- rbind(airbnb_boston, airbnb_chicago, airbnb_NYC, airbnb_LA, airbnb_DC, airbnb_Hawaii)

boston_mean <- mean(airbnb_boston$price)
boston_mean <- round(boston_mean, digits=2)
chicago_mean <- mean(airbnb_chicago$price)
chicago_mean <- round(chicago_mean, digits=2)
NYC_mean <- mean(airbnb_NYC$price)
NYC_mean <- round(NYC_mean, digits=2)
LA_mean <- mean(airbnb_LA$price)
LA_mean <- round(LA_mean, digits=2)
DC_mean <- mean(airbnb_DC$price)
DC_mean <- round(DC_mean, digits=2)
Hawaii_mean <- mean(airbnb_Hawaii$price)
Hawaii_mean <- round(Hawaii_mean, digits=2)

boston_median <- median(airbnb_boston$price)
chicago_median <- median(airbnb_chicago$price)
NYC_median <- median(airbnb_NYC$price)
LA_median <- median(airbnb_LA$price)
DC_median <- median(airbnb_DC$price)
DC_median <- round(DC_median, digits=2)
Hawaii_median <- median(airbnb_Hawaii$price)
Hawaii_median <- round(Hawaii_median, digits=2)

# US - MEDIAN & MEAN INFO FOR SELECTED CITIES
# -------------------------------------------
US_mean_info <- c(boston_mean, chicago_mean, Hawaii_mean, LA_mean,NYC_mean, DC_mean)
US_median_info <- c(boston_median, chicago_median, Hawaii_median,LA_median, NYC_median, DC_median )
City_info <-c("Boston","Chicago", "Hawaii", "LA", "NYC", "Wash. D.C.")

US_desc_info <- data.frame(City_info, US_mean_info, US_median_info)

US_mean <- ggplot(US_desc_info, aes(x= City_info, y=US_mean_info, fill=City_info)) + 
  geom_bar(stat='identity', position='dodge')+
  geom_label(mapping = aes(label=US_mean_info))

US_median <- ggplot(US_desc_info, aes(x= City_info, y=US_median_info, fill=City_info)) + 
  geom_bar(stat='identity', position='dodge')+
  geom_label(mapping = aes(label=US_median_info))

plot_grid(US_mean, US_median, nrow=2, ncol=1)

Entire_home_US <- airbnb_clean_cities %>%
  dplyr::select(room_type, city, price) %>%
  dplyr::filter(room_type == 'Entire home/apt') %>%
  group_by(city, price) %>%
  summarise(number_of_entire_home = n())
Entire_home_US

Private_room_US <- airbnb_clean_cities %>%
  dplyr::select(room_type, city, price) %>%
  dplyr::filter(room_type == 'Private room') %>%
  group_by(city, price) %>%
  summarise(number_of_private_room = n())
Private_room_US

Hotel_room_US <- airbnb_clean_cities %>%
  dplyr::select(room_type, city, price) %>%
  dplyr::filter(room_type == 'Hotel room') %>%
  group_by(city, price) %>%
  summarise(number_of_hotel_room = n())
Hotel_room_US

Shared_room_US <- airbnb_clean_cities %>%
  dplyr::select(room_type, city, price) %>%
  dplyr::filter(room_type == 'Shared room') %>%
  group_by(city, price) %>%
  summarise(number_of_shared_room = n())
Shared_room_US


Avg_price_Entire_US <- aggregate(Entire_home_US$price, list(Entire_home_US$city), median)
Avg_price_Entire_US
Avg_price_Private_US<- aggregate(Private_room_US$price, list(Private_room_US$city), median)
Avg_price_Hotel_US <- aggregate(Hotel_room_US$price, list(Hotel_room_US$city), median)
Avg_price_Hotel_US
Avg_price_Shared_US <- aggregate(Shared_room_US$price, list(Shared_room_US$city), median)
Avg_price_Shared_US


City_info <-c("Boston","Chicago", "Hawaii", "LA", "NYC", "Wash D.C")

Room_Type_US <- data.frame(City_info, Avg_price_Entire_US$x, Avg_price_Private_US$x, Avg_price_Hotel_US$x, Avg_price_Shared_US$x)

mean_Entire_US <- ggplot(Room_Type_US, aes(x= City_info, y=Avg_price_Entire_US.x, fill=City_info)) + 
  geom_bar(stat='identity', position='dodge')

mean_Private_US <- ggplot(Room_Type_US, aes(x= City_info, y=Avg_price_Private_US.x, fill=City_info)) + 
  geom_bar(stat='identity', position='dodge')

mean_Hotel_US <- ggplot(Room_Type_US, aes(x= City_info, y=Avg_price_Hotel_US.x, fill=City_info)) + 
  geom_bar(stat='identity', position='dodge')

mean_Shared_US <- ggplot(Room_Type_US, aes(x= City_info, y=Avg_price_Shared_US.x, fill=City_info)) + 
  geom_bar(stat='identity', position='dodge')

plot_grid(mean_Entire_US, mean_Private_US,mean_Hotel_US,mean_Shared_US ,nrow=2, ncol=2)


#----------------------------------------------------------------------------------------------------------

#                                         CASE STUDY: ANALYSIS OF HAWAII AIRBNB
#                                    ----------------------------------------------


# req_map_hawaii <- subset(airbnb_clean_df,city== "Hawaii",select = c(neighbourhood,neighbourhood_group ,latitude,longitude, 
                                                                       # price, room_type,number_of_reviews, minimum_nights,availability_365, reviews_per_month))

req_map_hawaii <- dplyr::filter(airbnb_US, city == 'Hawaii' & price <= 750)


ggplot(data = req_map_hawaii) + #passing the input dataframe  
  geom_boxplot(mapping = aes(x = room_type, y = availability_365, fill=room_type))+ #creating the boxplot
  xlab('Room Type')+  # X-axis label contains 'Stroke'
  ylab('Availability') # Y-axis label contains 'Age'

ggplot(data = req_map_hawaii) + #passing the input dataframe 
  geom_point(mapping = aes(x = longitude, y = latitude, color = neighbourhood))+
  theme(legend.key.size = unit(0.5,"line"))


#---------------------------------------------------------------------------------------------------------
# HISTOGRAM FOR PRICE IN HAWAII AIRBNB
# ------------------------------------

options(repr.plot.width=15, repr.plot.height=5)
a <- ggplot(data = req_map_hawaii, mapping = aes(x = price)) +
  geom_histogram(fill = "steel blue", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("Price") +
  ggtitle("Price Histogram") 
 


df <- data.frame(price = req_map_hawaii["price"][req_map_hawaii["price"] <= 750])
b <- ggplot(data = df, mapping = aes(x = price)) +
  geom_histogram(fill = "steel blue", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("Price") +
  ggtitle("Price <= 1000 | Histogram")

plot_grid(a, b, ncol=2, nrow=1)

#Cullen & Frey Graph
descdist(df$price)

#for normal
fit_normal <- fitdist(df$price, "norm")
summary(fit_normal)

#for gamma
fit_gamma <- fitdist(df$price, "gamma")
summary(fit_gamma)

# for log normal
#fit_lnormal <- fitdist(df$price, "lnorm")
#summary(fit_lnormal)

# for exponential
fit_expo <- fitdist(df$price, "exp")
summary(fit_expo)

# goodness-of-fit plots
par(mfrow=c(2,2))
plot.legend <- c("exp", "gamma","normal")
denscomp(list(fit_expo,fit_gamma, fit_normal), legendtext = plot.legend, xlab = 'x', xlegend = 'topleft')
cdfcomp (list(fit_expo,fit_gamma, fit_normal), legendtext = plot.legend, xlab = 'x')
qqcomp  (list(fit_expo,fit_gamma, fit_normal), legendtext = plot.legend, xlab = 'x')
ppcomp  (list(fit_expo,fit_gamma, fit_normal), legendtext = plot.legend, xlab = 'x')



#--------------------------------------------------------------------------------------------------------------------

# PLOT FOR ROOM DISTRIBUTION IN HAWAII
# ------------------------------------

room_dist_type <- data.frame(cbind(Frequency = table(req_map_hawaii$room_type), 
                                   Percent = prop.table(table(req_map_hawaii$room_type)) * 100))
room_dist_type <- room_dist_type[order(room_dist_type$Frequency),]
room_dist_type

options(repr.plot.width=15, repr.plot.height=6)
e <- ggplot(data = room_dist_type, mapping = aes(x = row.names(room_dist_type), y = Frequency)) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(room_dist_type), color = row.names(room_dist_type)), alpha = .7, size = 1.1) +
  geom_label(mapping = aes(label=Frequency), fill = "white", size = 3, color = "black", hjust=.7) +
  ylab("") +
  ggtitle("Room type distribution") +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 10, hjust = .5),
        axis.text.x = element_text(size = 6, face = "bold"),
        axis.text.y = element_text(size = 6, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        legend.position = "none")

e
#---------------------------------------------------------------------------------------------------------------------

#  NEIGHBOURHOODS WITH HIGH NUMBER OF AIRBNB 
# ------------------------------------------------

highfreq_neigh <- data.frame(cbind(Frequency = table(req_map_hawaii$neighbourhood), 
                                   Percent = prop.table(table(req_map_hawaii$neighbourhood)) * 100))
highfreq_neigh <- highfreq_neigh[order(highfreq_neigh$Frequency),]
highfreq_neigh

highfreq_neigh_df <- data.frame(neighbourhood = row.names(tail(highfreq_neigh, 10)), 
                                Frequency = tail(highfreq_neigh, 10)$Frequency)
r <- c()
for(i in 10:1){r <- c(r, i)}
row.names(highfreq_neigh_df) <- r
highfreq_neigh_df


options(repr.plot.width=15, repr.plot.height=6)
ggplot(data = highfreq_neigh_df, mapping = aes(x = neighbourhood, y = Frequency)) +
  theme_minimal() + 
  geom_point(size = 6, color = "green") +
  ggtitle("The 10 most frequent neighbourhood") +
  xlab("") +
  geom_line(color = "black", size = 2, linetype= 16, group = 1, alpha = .5) + 
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood, color = neighbourhood), alpha = .8, size = 1.5) +
  theme(plot.title = element_text(size = 12, hjust = .5),
        axis.text.x = element_text(size = 8, angle=15, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.title.x = element_text(size = 19),
        axis.title.y = element_text(size = 19),
        legend.text = element_text(size = 10, face = "bold"))

#---------------------------------------------------------------------------------------------------------------------

# MEAN MODE & MEDIAN OF PRICE & MINIMUM NIGHTS IN HAWAII AIRBNB
# -------------------------------------------------------------

medians <- data.frame(Median = c(median(req_map_hawaii$price), median(req_map_hawaii$minimum_nights)))
row.names(medians) <- c("Price", "Minimum nights")
medians

medians_1 <- median(req_map_hawaii$price)

means <- data.frame(Mean = c(mean(req_map_hawaii$price), mean(req_map_hawaii$minimum_nights)))
row.names(means) <- c("Price", "Minimum nights")
means

modes <- function(x){
  
  freq <- table(x)
  return(names(freq)[freq == max(freq)])
}

mod <- data.frame(Mode = c(modes(req_map_hawaii$price), modes(req_map_hawaii$minimum_nights)))
row.names(mod) <- c("Price", "Minimum nights")
mod

options(repr.plot.width=15, repr.plot.height=10)
options(warn=-1)
price_density_df <- data.frame(price = req_map_hawaii["price"][req_map_hawaii["price"] <= 750])
g <- ggplot(data = price_density_df, mapping = aes(x = price)) +
  geom_density(fill = "blueviolet", size = 1.3, color = "black") +
  geom_vline(xintercept = mean(req_map_hawaii$price), size = 1, linetype="dotdash", color = "black") +
  geom_vline(xintercept = median(req_map_hawaii$price), size = 1, linetype="dotdash", color = "red") +
  geom_vline(xintercept = as.numeric(modes(req_map_hawaii$price)), size = 1, linetype="dotdash", color = "blue") +
  annotate("text", label="Mean = 226", x = 270, y = .0085, color = "black", size=3, fontface = "bold") +
  annotate("text", label="Median = 189", x = 300, y = .0075, color = "red", size=3, fontface = "bold") +
  annotate("text", label="Mode = 150", x = 330, y = .0065, color = "blue", size=3, fontface = "bold") +
  ylab("") +
  xlab("Price") +
  ggtitle("Price <= 750 | Density")

min_night_density_df <- data.frame(minimum_nights = req_map_hawaii["minimum_nights"][req_map_hawaii["minimum_nights"] <= 32])
h <- ggplot(data = min_night_density_df, mapping = aes(x = minimum_nights)) +
  geom_density(fill = "coral1", size = 1.3, color = "black") +
  geom_vline(xintercept = mean(req_map_hawaii$minimum_nights), size = 1, linetype="dotdash", color = "black") +
  geom_vline(xintercept = median(req_map_hawaii$minimum_nights), size = 1, linetype="dotdash", color = "red") +
  geom_vline(xintercept = as.numeric(modes(req_map_hawaii$minimum_nights)), size = 1, linetype="dotdash", color = "blue") +
  annotate("text", label="Mean = 6", x = 12, y = .33, color = "black", size=3, fontface = "bold") +
  annotate("text", label="Median = 3", x = 13.5, y = .28, color = "red", size=3, fontface = "bold") +
  annotate("text", label="Mode = 1", x = 15, y = .23, color = "blue", size=3, fontface = "bold") +
  ylab("") +
  xlab("Minimum nights") +
  ggtitle("Minimum nights <= 32 | Density") 

plot_grid(g, h, ncol=2, nrow=1)

#----------------------------------------------------------------------------------------------------------------------------------

# VARIANCE & STD DEVIATION ON PRICES 
# ----------------------------------

var_hawaii <- data.frame("Variance" = c(var(req_map_hawaii$price), var(req_map_hawaii$minimum_nights)))
row.names(var_hawaii) <- c("Price", "Minimum nights")
var_hawaii

std_hawaii <- data.frame("Standard deviation" = c(sqrt(var(req_map_hawaii$price)), 
                                                  sqrt(var(req_map_hawaii$minimum_nights))))
row.names(std_hawaii) <- c("Price", "Minimum nights")
std_hawaii

mean_room_type <- aggregate(list(average_price = req_map_hawaii$price), list(room_type = req_map_hawaii$room_type), mean)
mean_room_type$Percent <- prop.table(mean_room_type$average_price) * 100
mean_room_type

options(repr.plot.width=15, repr.plot.height=6)
options(warn=-1)
i <- ggplot(data = mean_room_type, aes(x=room_type, y=average_price)) +
  coord_flip() +
  geom_segment(aes(xend=room_type, yend=0, color = room_type), size = 2) +
  geom_point(size=7, mapping = aes(color = room_type)) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  ggtitle("Average price per room type")

i

options(repr.plot.width=14, repr.plot.height=6)
j <- ggplot(data = req_map_hawaii, mapping = aes(x = price)) +
  geom_histogram(mapping = aes(fill = room_type), bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("Price") +
  ggtitle("Price Histogram") +
  theme(
    plot.title = element_text(size = 23, hjust = .5),
    axis.text.x = element_text(size = 19, face = "bold"),
    axis.text.y = element_text(size = 19, face = "bold"),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    legend.position = "none")



k_df <- data.frame(price = req_map_hawaii["price"][req_map_hawaii["price"] <= 750], 
                   room_type = req_map_hawaii["room_type"][req_map_hawaii["price"] <= 750])
k <- ggplot(data = k_df, mapping = aes(x = price)) +
  geom_histogram(mapping = aes(fill = room_type), bins = 70, size = 1.3) +
  theme_minimal() +
  ylab("") +
  xlab("Price") +
  ggtitle("Price <= 1000 | Histogram") +
  theme(
    plot.title = element_text(size = 12, hjust = .5),
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size =6),
    axis.title.y = element_text(size = 6))+
  theme(legend.position="bottom", legend.text = element_text(size=8, angle= 90,face="bold"))

options(repr.plot.width=12, repr.plot.height=5)
l_df <- data.frame(price = req_map_hawaii["price"][req_map_hawaii["price"] <= 750], 
                   room_type = req_map_hawaii["room_type"][req_map_hawaii["price"] <= 750])
l <- ggplot(data = l_df, mapping = aes(x = price, fill = room_type)) +
  geom_density(mapping = aes(fill = room_type), bins = 70, size = 1.3, color = "black", alpha = .6, size = 1.5) +
  theme_minimal() +
  ylab("Density") +
  xlab("Price") +
  ggtitle("Price <= 1000 | Histogram") +
  theme(
    plot.title = element_text(size = 12, hjust = .5),
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6))+
  theme(legend.position="bottom", legend.text = element_text(size=8, angle= 90,face="bold"))


plot_grid(k, l, ncol=2, nrow=1)

#-----------------------------------------------------------------------------------------------------------------

# PRICE DISTRIBUTION FOR EACH ROOM TYPE IN 4 DIFFERENT ISLANDS
# ------------------------------------------------------------

Kauai<- req_map_hawaii %>%
  dplyr::select(room_type, neighbourhood_group, neighbourhood, price) %>%
  dplyr::filter(neighbourhood_group == "Kauai") %>%
  group_by(room_type, neighbourhood, price)

Oahu<- req_map_hawaii %>%
  dplyr::select(room_type, neighbourhood_group, neighbourhood, price) %>%
  dplyr::filter(neighbourhood_group == "Honolulu") %>%
  group_by(room_type, neighbourhood, price)

Maui<- req_map_hawaii %>%
  dplyr::select(room_type, neighbourhood_group, neighbourhood, price) %>%
  dplyr::filter(neighbourhood_group == "Maui") %>%
  group_by(room_type, neighbourhood, price)

Big_Island<- req_map_hawaii %>%
  dplyr::select(room_type, neighbourhood_group, neighbourhood, price) %>%
  dplyr::filter(neighbourhood_group == "Hawaii") %>%
  group_by(room_type, neighbourhood, price)



h1 <- ggplot(data = Kauai) + #passing the input dataframe  
  geom_boxplot(mapping = aes(x = room_type, y = price, fill=room_type))+
  ggtitle("Kauai Island")

h2 <- ggplot(data = Oahu) + #passing the input dataframe  
  geom_boxplot(mapping = aes(x = room_type, y = price, fill=room_type))+
  ggtitle("Honolulu Island")

h3 <- ggplot(data = Maui) + #passing the input dataframe  
  geom_boxplot(mapping = aes(x = room_type, y = price, fill=room_type))+
  ggtitle("Maui Island")

h4 <- ggplot(data = Big_Island) + #passing the input dataframe  
  geom_boxplot(mapping = aes(x = room_type, y = price, fill=room_type))+
  ggtitle("Hawaii Island")


plot_grid(h1, h2, h3, h4, nrow= 2, ncol=2)

Avg_price_Kauai <- mean(Kauai$price)
Avg_price_Honolulu <- mean(Oahu$price)
Avg_price_Maui <- mean(Maui$price)
Avg_price_Hawaiii<- mean(Big_Island$price)

island_info <- c("Hawaii","Honolulu", "Kauai", "Maui")

island_bar <- data.frame(island_info, Avg_price_Hawaiii, Avg_price_Honolulu, Avg_price_Kauai, Avg_price_Maui)

Avg_price_Entire_US <- aggregate(Entire_home_US$price, list(Entire_home_US$city), median)
Avg_price_Private_US<- aggregate(Private_room_US$price, list(Private_room_US$city), median)
Avg_price_Hotel_US <- aggregate(Hotel_room_US$price, list(Hotel_room_US$city), median)
Avg_price_Shared_US <- aggregate(Shared_room_US$price, list(Shared_room_US$city), median)

avg_island_price <- c(Avg_price_Hawaiii, Avg_price_Honolulu, Avg_price_Kauai, Avg_price_Maui)

island_bar <- data.frame(island_info,avg_island_price)

Two_Island<- req_map_hawaii %>%
  dplyr::select(neighbourhood_group, price) %>%
  dplyr::filter(neighbourhood_group == "Hawaii" & neighbourhood_group== "Honolulu") %>%
  group_by(neighbourhood_group, price)


Two_Island_long <- melt(Two_Island) 

ggplot(data = Big_Island) + #passing the input dataframe  
  geom_boxplot(mapping = aes(x = room_type, y = price, fill=room_type))+
  ggtitle("Hawaii Island")

Island_1<- req_map_hawaii %>%
  dplyr::select(neighbourhood_group,price) %>%
  dplyr::filter(neighbourhood_group == "Hawaii") %>%
  group_by(neighbourhood_group, price)

Island_2 <- req_map_hawaii %>%
  dplyr::select(neighbourhood_group, price) %>%
  dplyr::filter(neighbourhood_group == "Honolulu") %>%
  group_by(neighbourhood_group, price)

b1 <- ggplot(data = Island_1) + #passing the input dataframe  
  geom_boxplot(mapping = aes(x = neighbourhood_group, y = price, fill=neighbourhood_group, 
                             fill="orange"))+
  ggtitle("Hawaii Island")+
  scale_fill_brewer(palette="Dark2")+
  theme(legend.position="bottom")
  

b2 <- ggplot(data = Island_2) + #passing the input dataframe  
  geom_boxplot(mapping = aes(x = neighbourhood_group, y = price, fill=neighbourhood_group))+
  ggtitle("Honolulu Island")+
  scale_fill_brewer(palette="BuPu")+
  theme(legend.position="bottom")

plot_grid(b1, b2, nrow=1, ncol=2)


# EXPENSIVE NEIGHBOURHOODS IN EACH ISLANDS (KAUAI, OAHU, MAUI & HAWAII)
# ---------------------------------------------------------------------

Exp_neighbourhood_Kauai <- aggregate(list(Kauai$price), list(Kauai$neighbourhood), mean)
colnames(Exp_neighbourhood_Kauai) <- c("neighbourhood", "Average_price_per_neighborhood")
Exp_neighbourhood_Kauai <- Exp_neighbourhood_Kauai[order(Exp_neighbourhood_Kauai$Average_price_per_neighborhood),]
Exp_neighbourhood_Kauai <- head(Exp_neighbourhood_Kauai, 6)
r <- c()
for(i in 1:6){r <- c(r, i)}
row.names(Exp_neighbourhood_Kauai) <- r
Exp_neighbourhood_Kauai


Exp_neighbourhood_Oahu <- aggregate(list(Oahu$price), list(Oahu$neighbourhood), mean)
Exp_neighbourhood_Oahu
colnames(Exp_neighbourhood_Oahu) <- c("neighbourhood", "Average_price_per_neighborhood")
Exp_neighbourhood_Oahu <- Exp_neighbourhood_Oahu[order(Exp_neighbourhood_Oahu$Average_price_per_neighborhood),]
Exp_neighbourhood_Oahu <- head(Exp_neighbourhood_Oahu, 8)
r <- c()
for(i in 1:8){r <- c(r, i)}
row.names(Exp_neighbourhood_Oahu) <- r
Exp_neighbourhood_Oahu


Exp_neighbourhood_Maui <- aggregate(list(Maui$price), list(Maui$neighbourhood), mean)
Exp_neighbourhood_Maui
colnames(Exp_neighbourhood_Maui) <- c("neighbourhood", "Average_price_per_neighborhood")
Exp_neighbourhood_Maui <- Exp_neighbourhood_Maui[order(Exp_neighbourhood_Maui$Average_price_per_neighborhood),]
Exp_neighbourhood_Maui <- head(Exp_neighbourhood_Maui, 8)
r <- c()
for(i in 1:8){r <- c(r, i)}
row.names(Exp_neighbourhood_Maui) <- r
Exp_neighbourhood_Maui

Exp_neighbourhood_Hawaii <- aggregate(list(Big_Island$price), list(Big_Island$neighbourhood), mean)
Exp_neighbourhood_Hawaii
colnames(Exp_neighbourhood_Hawaii) <- c("neighbourhood", "Average_price_per_neighborhood")
Exp_neighbourhood_Hawaii <- Exp_neighbourhood_Hawaii[order(Exp_neighbourhood_Hawaii$Average_price_per_neighborhood),]
Exp_neighbourhood_Hawaii <- head(Exp_neighbourhood_Hawaii, 9)
r <- c()
for(i in 1:9){r <- c(r, i)}
row.names(Exp_neighbourhood_Hawaii) <- r
Exp_neighbourhood_Hawaii

options(repr.plot.width=15, repr.plot.height=11)
m <- ggplot(data = Exp_neighbourhood_Kauai, mapping = aes(x = neighbourhood, y = Average_price_per_neighborhood)) +
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood), alpha = .8, size = 1.5) +
  geom_label(mapping = aes(label = round(Average_price_per_neighborhood, 2)), size = 2, fill = "white", fontface = "bold") +
  ggtitle("The Affordability of neighborhoods in Kauai") +
  xlab("") +
  ylab("") +
  theme(
    plot.title = element_text(size = 12, hjust = .5),
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6))

n <- ggplot(data = Exp_neighbourhood_Oahu, mapping = aes(x = neighbourhood, y = Average_price_per_neighborhood)) +
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood), alpha = .8, size = 1.5) +
  geom_label(mapping = aes(label = round(Average_price_per_neighborhood, 2)), size = 2, fill = "white", fontface = "bold") +
  ggtitle("The Affordability of neighborhoods in Oahu") +
  xlab("") +
  ylab("") +
  theme(
    plot.title = element_text(size = 12, hjust = .5),
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6))


o <- ggplot(data = Exp_neighbourhood_Maui, mapping = aes(x = neighbourhood, y = Average_price_per_neighborhood)) +
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood), alpha = .8, size = 1.5) +
  geom_label(mapping = aes(label = round(Average_price_per_neighborhood, 2)), size = 2, fill = "white", fontface = "bold") +
  ggtitle("The Affordability of neighborhoods in Maui") +
  xlab("") +
  ylab("") +
  theme(
    plot.title = element_text(size = 12, hjust = .5),
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6))
 

p <- ggplot(data = Exp_neighbourhood_Hawaii, mapping = aes(x = neighbourhood, y = Average_price_per_neighborhood)) +
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood), alpha = .8, size = 1.5) +
  geom_label(mapping = aes(label = round(Average_price_per_neighborhood, 2)), size = 2, fill = "white", fontface = "bold") +
  ggtitle("The Affordability of neighborhoods in Hawaii") +
  xlab("") +
  ylab("") +
  theme(
    plot.title = element_text(size = 12, hjust = .5),
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6))



plot_grid(m, n, o, p, ncol=1, nrow=4)

#----------------------------------------------------------------------------------------------------------------------


ggplot(data = req_map_hawaii, mapping = aes(x = longitude, y = latitude)) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = price), size = 3) +
  ggtitle("") 

scatter_df <- data.frame(price = req_map_hawaii["price"][req_map_hawaii["price"] <= 200], room_type = req_map_hawaii["room_type"][req_map_hawaii["price"] <= 200], lat = req_map_hawaii["latitude"][req_map_hawaii["price"] <= 200], lon = req_map_hawaii["longitude"][req_map_hawaii["price"] <= 200],
                         neighbourhood= req_map_hawaii["neighbourhood"][req_map_hawaii["price"] <= 200],
                         minimum_nights = req_map_hawaii["minimum_nights"][req_map_hawaii["price"] <= 200])
scatter_df$minimum_nights <- factor(scatter_df$minimum_nights)
q<- ggplot(data = scatter_df, mapping = aes(x = lat, y = lon, color = price)) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = price), size = 3) +
  ggtitle("Price <= 200 dollars ")+
  theme(plot.title = element_text(size = 12, hjust = .5),
        axis.text.x = element_text(size = 6, face = "bold"),
        axis.text.y = element_text(size = 6, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        legend.text = element_text(colour="black", size=6, face="bold"),
        legend.background = element_rect(fill="white", size=0.5, linetype="dashed", 
                                         colour ="black"))


r<-ggplot(data = scatter_df, mapping = aes(x = lat, y = lon)) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = neighbourhood), size = 3) +
  ggtitle("Neighbourhood") +
  xlab("Latitude") +
  ylab("Longitude")+
  theme(plot.title = element_text(size = 12, hjust = .5),
        axis.text.x = element_text(size = 6, face = "bold"),
        axis.text.y = element_text(size = 6, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        legend.text = element_text(colour="black", size=6, face="bold"),
        legend.background = element_rect(fill="white", size=0.5, linetype="dashed", 
                                         colour ="black"))

s<-ggplot(data = scatter_df, mapping = aes(x = lat, y = lon)) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = room_type), size = 3) +
  ggtitle("Room Type ") +
  xlab("Latitude") +
  ylab("Longitude") +
  theme(plot.title = element_text(size = 12, hjust = .5),
        axis.text.x = element_text(size = 6, face = "bold"),
        axis.text.y = element_text(size = 6, face = "bold"),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        legend.text = element_text(colour="black", size=6, face="bold"),
        legend.background = element_rect(fill="white", size=0.5, linetype="dashed", 
                                         colour ="black"))

t<-ggplot(data = scatter_df, mapping = aes(x = lat, y = lon)) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = minimum_nights), size = 3) +
  ggtitle("Minimum Nights") +
  xlab("Latitude") +
  ylab("Longitude") +
  theme(plot.title = element_text(size = 12, hjust = .5),
      axis.text.x = element_text(size = 6, face = "bold"),
      axis.text.y = element_text(size = 6, face = "bold"),
      axis.title.x = element_text(size = 6),
      axis.title.y = element_text(size = 6),
      legend.text = element_text(colour="black", size=6, face="bold"),
      legend.background = element_rect(fill="white", size=0.5, linetype="dashed", 
                                       colour ="black"))

plot_grid(q, r, s, t,ncol=2,nrow=2)

#------------------------------------------------------------------------------------------------------------

#CORRELATION PLOT

# CORRELATION HEATMAP
airbnb_cor <- req_map_hawaii[, sapply(req_map_hawaii, is.numeric)]
airbnb_cor <- airbnb_cor[complete.cases(airbnb_cor), ]
correlation_matrix <- cor(airbnb_cor)
corrplot(correlation_matrix, method = "color")


req_colum <- dplyr::select(req_map_hawaii, number_of_reviews, reviews_per_month, price)
head(req_colum, 5)
  
corr_df <- dplyr::filter(req_colum, number_of_reviews <= 200)
  
ggscatter(corr_df, x = "price", y = "reviews_per_month", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Rent in Dollars", ylab = "Reviews per month")+
          geom_smooth(method=lm, se= FALSE)

ggscatter(corr_df, x = "reviews_per_month", y = "price", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Reviews per month", ylab = "Rent in Dollars")+
  geom_smooth(method=lm, se= FALSE)


ggscatter(corr_df, x = "price", y = "reviews_per_month", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "Rent in Dollars", ylab = "Reviews per month")+
  geom_smooth(method=lm, se= FALSE)

ggscatter(corr_df, x = "reviews_per_month", y = "price", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "Reviews per month", ylab = "Rent in Dollars")+
  geom_smooth(method=lm, se= FALSE)


dataf <- dummy_cols(req_map_hawaii, select_columns = 'room_type')

cor(dataf$price, dataf$`room_type_Entire home/apt`)
cor(dataf$`room_type_Private room`,dataf$price)
cor(dataf$`room_type_Hotel room`,dataf$price)
cor(dataf$`room_type_Shared room`,dataf$price)

#-----------------------------------------------------------------------------------------------------

# LINEAR REGRESSION

req_colum_5 <- dplyr::select(req_map_hawaii, reviews_per_month, price)
corr_df_5<- dplyr::filter(req_colum_5, reviews_per_month<=10)

ggplot(corr_df_5, aes(x = reviews_per_month, y = price)) +
  geom_point() +
  stat_smooth()

model <- lm(price ~ reviews_per_month, data = corr_df_5)
model

ggplot(corr_df_5, aes(reviews_per_month, price)) +
  geom_point() +
  stat_smooth(method = lm)

summary(model)

#-----------------------------------------------------------------------------------------------------

# Hypothesis Testing
# ------------------

#1  Test if Mean Airbnb rental in Hawaii is greater than the National Average

# H0: National Mean of Airbnb Rental greater than Hawaii Mean
# H1: Hawaii mean greater than National Mean

airbnb_df_clean= subset(airbnb_clean_df,price <=750, select= c(price,city))
mean(airbnb_df_clean$price, na.rm= TRUE)

set.seed(100)
Sample <- sample_n(req_map_hawaii, 50)
xbar_ <-  mean(Sample$price) #sample mean of airbnb rental in Hawaii for Sample size n= 200
xbar_

# Population Mean of Airbnb rental (Pop_Mean)
mue0 <- airbnb_df_clean= subset(airbnb_clean_df,price <=750, select= c(price,city))
mean(airbnb_df_clean$price, na.rm= TRUE)

sigma_price <- var(airbnb_df_clean$price, na.rm=TRUE)
sqrt(sigma_price)

# X = R.V. of price of AirBNB
# H0: Samp_Mean < 168.23
# H1: Samp_Mean >168.23 

one.sample.z(null.mu = 168.22, xbar =xbar_ , sigma = 130, n = 50, conf= 0.95, alternative = 'greater')

#2 Neighbourhood group Hawaii mean is 50 less than Neighbourhood group Honolulu

#H0: Mean of Hawaii greater than or equal to Mean of Oahu by 50
#H1: Mean of Hawaii lesser than Mean of Oahu by 50

sample1_hawaii <-req_map_hawaii %>%
  dplyr::select(neighbourhood_group, price) %>%
  dplyr::filter(neighbourhood_group == "Hawaii") %>%
  group_by(price)

hawaii <- data.frame(sample1_hawaii)

sample1_Oahu <-req_map_hawaii %>%
  dplyr::select(neighbourhood_group,price) %>%
  dplyr::filter(neighbourhood_group == "Honolulu") %>%
  group_by(price)
oahu <- data.frame(sample1_Oahu)

Sample_1 <- sample_n(hawaii, 20)
Sample_2 <- sample_n(oahu, 20)

xb1<- mean(Sample_1$price)
s_sqare_1<- var(Sample_1$price, na.rm=TRUE)
s1 <- sqrt(s_sqare_1)
n1 <- 20

xb2 <- mean(Sample_2$price)
s_sqare_2<- var(Sample_2$price, na.rm=TRUE)
s2 <- sqrt(s_sqare_2)
n2 <- 20

# TWO SAMPLE T TEST
tsum.test(mean.x= xb1, s.x = s1, n.x = n1, mean.y = xb2, s.y = s2,
          n.y = n2, alternative = "less", mu = 50, var.equal = TRUE,
          conf.level = 0.95)


#----------------------------------------------------------------------------------------------------------

# Finding Probability of getting AIRBNB for less than $200 in Hawaii

# Follows a Gamma Distribution

prob1 <- dplyr::select(req_map_hawaii, price)
head(req_colum, 5)

r1 <- nrow (dplyr::filter(prob1, price <= 150))
r2 <- nrow (dplyr::filter(prob1, price >150 & price <=300))
r3 <- nrow (dplyr::filter(prob1, price >300 & price <=450))
r4 <- nrow (dplyr::filter(prob1, price >450 & price <=600))
r5 <- nrow (dplyr::filter(prob1, price >600 & price <=750))


r21 <- rbind(r1, r2, r3, r4, r5, r5)

x1 <- pgamma(q=150, shape = 3,rate=0.013)
x2 <- pgamma(q=200, shape = 3,rate=0.013)
x3 <- pgamma(q=450, shape = 3,rate=0.013)
x4 <- pgamma(q=600, shape = 3,rate=0.013)
x5 <- pgamma(q=750, shape = 3,rate=0.013)

Proby <- rbind(x1, x2, x3, x4, x5)






