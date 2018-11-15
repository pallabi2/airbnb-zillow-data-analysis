#loading airbnb listings.csv dataset
dataset <- read.csv("C:/Users/palla/Downloads/listings.csv/listings.csv")
View(dataset)

#converting to a dataframe
dataset <- data.frame(dataset)

#checking the statistical attributes of the dataset
summary(dataset)
dim(dataset)

#subsetting the data
new_data <- data.frame(dataset$id, dataset$name, dataset$description, dataset$last_scraped, dataset$host_id, dataset$city, dataset$zipcode, dataset$price,
                       dataset$monthly_price, dataset$weekly_price, dataset$availability_30, dataset$availability_60,dataset$availability_90, dataset$availability_365,
                       dataset$neighbourhood_cleansed, dataset$neighbourhood_group_cleansed, dataset$review_scores_location, dataset$review_scores_value, dataset$host_is_superhost,
                       dataset$host_since, dataset$host_identity_verified, dataset$latitude, dataset$longitude, dataset$property_type, dataset$room_type, dataset$bathrooms, 
                       dataset$bedrooms, dataset$amenities, dataset$minimum_nights, dataset$maximum_nights, dataset$cancellation_policy)
View(new_data)

#counting number of listings by neighbourhood_cleansed
install.packages('Rcpp')
library(dplyr)
install.packages('dplyr')
library('dplyr')
new_data1 <- count(new_data, vars=dataset.neighbourhood_cleansed)
new_data1<-new_data1[order(-new_data1$n),]
colnames(new_data1)[colnames(new_data1)=="vars"]<- "Neighbourhood"
View(new_data1)

##price vs neighborhood bar plot

#transformation price column to remove '$' and 'NA'
new_price <- aggregate(as.numeric(gsub("\\$", "", new_data$dataset.price)), list(new_data$dataset.neighbourhood_cleansed), mean, na.rm=TRUE)
colnames(new_price)[colnames(new_price)=="Group.1"]<- "Neighbourhood"
colnames(new_price)[colnames(new_price)=="x"]<- "Price"
new_price <- merge.data.frame(x= new_price , y = new_data1, by="Neighbourhood")
new_price<-new_price[order(-new_price$Price),]
#selecting top 30 rows based on price
new_price <- new_price[1:30,]
View(new_price)

# visualizing neighborhood and price data using ggplot2
library(ggplot2)
neighborhood_price <- new_price[order(-new_price$Price),]
View(neighborhood_price)
bar_plot <- ggplot(data=new_price, aes(x=reorder(Neighbourhood, Price), y=Price)) +
  geom_bar(stat="identity", fill= "steelblue")+ ggtitle("Neighbourhood VS Price of Listings")
bar_plot + coord_flip()

## availability vs neighborhood bar plot

#transformation price column to remove '$' and 'NA'
neighborhood_availability <- aggregate(as.numeric(gsub("\\$", "", new_data$dataset.availability_30)), list(new_data$dataset.neighbourhood_cleansed), mean, na.rm=TRUE)
View(neighborhood_availability)
colnames(neighborhood_availability)[colnames(neighborhood_availability)=="Group.1"]<- "Neighbourhood"
colnames(neighborhood_availability)[colnames(neighborhood_availability)=="x"]<- "Availability"

#merged dataframes to include count of listings for each neighborhood 
neighborhood_availability <- merge.data.frame(x= neighborhood_availability , y = new_data1, by="Neighbourhood")
neighborhood_availability<-neighborhood_availability[order(neighborhood_availability$Availability),]
neighborhood_availability <- neighborhood_availability[1:30,]
View(neighborhood_availability)
library(ggplot2)
bar_plot <- ggplot(data=neighborhood_availability, aes(x=reorder(Neighbourhood, -Availability), y=Availability)) +
  geom_bar(stat="identity", fill= "pink")+ ggtitle("Neighbourhood VS Availability")
bar_plot + coord_flip()

##price vs availability scatter plot

#considering 2-bedroom roperties only
new_data_2bed<-new_data[new_data$dataset.bedrooms==2,]
price_availability <- aggregate(as.numeric(gsub("\\$", "", new_data_2bed$dataset.availability_365)), list(new_data_2bed$dataset.price), mean, na.rm=TRUE)
View(price_availability)
colnames(price_availability)[colnames(price_availability)=="Group.1"]<- "Price"
colnames(price_availability)[colnames(price_availability)=="x"]<- "Availability"

View(price_availability)
library(ggplot2)
scatter_plot <- ggplot(data=price_availability, aes(x=Price, y=Availability)) +
  geom_point()+ ggtitle("Price and Availability of Listings")
scatter_plot <- scatter_plot + theme(
  axis.text.x = element_blank(),
  axis.text.y = element_blank() )
scatter_plot + coord_flip()

# correlation between both variables- Availability and Price
library(Hmisc)
cor.test(as.numeric(gsub("[$,]", "", price_availability$Price)), price_availability$Availability)

# correlation between bedrooms and bathrooms
library(Hmisc)
cor.test(dataset_zip5_complete$input.bathrooms, dataset_zip5_complete$input.bedrooms)

#average quality of a listing vs average price barplot
price_review <- aggregate(as.numeric(gsub("\\$", "", new_data$dataset.review_scores_value)), list(new_data$dataset.price), mean, na.rm=TRUE)
View(price_review)
colnames(price_review)[colnames(price_review)=="Group.1"]<- "Price"
colnames(price_review)[colnames(price_review)=="x"]<- "Review_score"
View(price_review)
price_review <- price_review[1:30,]
View(price_review)
library(ggplot2)
bar_plot <- ggplot(data=price_review, aes(x=reorder(Price, -Review_score), y=Review_score)) +
  geom_bar(stat="identity", fill= "blue")+ ggtitle("quality of a listing vs average price")
bar_plot + coord_flip()

input <- dataset <- read.csv("C:/Users/palla/Downloads/listings.csv/listings.csv")
dataset <- data.frame(input$neighbourhood_cleansed, input$zipcode, input$latitude, input$longitude, input$property_type, input$room_type, input$bathrooms, input$bedrooms, input$price, input$minimum_nights, input$maximum_nights, input$review_scores_rating, input$review_scores_location)
View(dataset)

#transforming 9-digit zipcodes to 5-digit zipcodes
dataset$zip5<-substr(dataset$input.zipcode, 0, 5)
#filtering for the zipcodes in state of NY
dataset_zip5<-dataset[as.numeric(dataset$zip5) >= 10000,]
#filtering for 2-bedroom listings only
dataset_zip5<-dataset_zip5[dataset_zip5$input.bedrooms==2,]
#filtering NA values from zipcode column
dataset_zip5<-dataset_zip5[!is.na(dataset_zip5$zip5),]
dataset_zip5$input.price<-as.numeric(gsub("[$,]","",dataset_zip5$input.price))
data_median_zip<-aggregate(dataset_zip5$input.price, by = list(dataset_zip5$zip5), FUN = median)
View(data_median_zip)
#renameing column names
colnames(data_median_zip)<-c("Zip", "median.price")
library(plyr)

# merge count of listings for each zipcode
data_median_zip <- merge(x = data_median_zip, y = count(dataset_zip5, vars="zip5"), by.x = "Zip", by.y = "zip5")
data_median_zip[order(-data_median_zip$median.price),]

#verify sum of frequencies with number of 2-bedroom listings in the dataset
sum(data_median_zip$freq)
nrow(dataset_zip5[dataset_zip5$input.bedrooms==2,])

#considering only complete cases to build data models
dataset_zip5_complete<-dataset_zip5[complete.cases(dataset_zip5),]
View(dataset_zip5_complete)
model1<-lm(dataset_zip5_complete$input.price~dataset_zip5_complete$input.bathrooms+dataset_zip5_complete$input.bedrooms+dataset_zip5_complete$input.minimum_nights+dataset_zip5_complete$input.maximum_nights+dataset_zip5_complete$input.review_scores_location+dataset_zip5_complete$input.review_scores_rating)
summary(model1)

install.packages('car')
library('car')
vif(model1)

model2<-lm(dataset_zip5_complete$input.price~dataset_zip5_complete$input.bathrooms+dataset_zip5_complete$input.bedrooms+dataset_zip5_complete$input.minimum_nights+dataset_zip5_complete$input.review_scores_location+dataset_zip5_complete$input.review_scores_rating)
summary(model2)
vif(model2)

model3<-lm(dataset_zip5_complete$input.price~dataset_zip5_complete$input.bathrooms+dataset_zip5_complete$input.bedrooms+dataset_zip5_complete$input.minimum_nights+dataset_zip5_complete$input.review_scores_location+dataset_zip5_complete$input.review_scores_rating+dataset_zip5_complete$input.room_type)
summary(model3)
vif(model3)

dataset_zip5_complete$prediction<-predict.lm(object = model3)
#generating pairwise relation models between variables
pairs(dataset_zip5_complete$input.price~dataset_zip5_complete$input.bathrooms+dataset_zip5_complete$input.bedrooms+dataset_zip5_complete$input.minimum_nights+dataset_zip5_complete$input.review_scores_location+dataset_zip5_complete$input.review_scores_rating+dataset_zip5_complete$input.room_type)


#loading zillow dataset into workspace
dataset <- read.csv("C:/Users/palla/Downloads/airbnb-zillow-data-challenge-master/airbnb-zillow-data-challenge-master/Zip_Zhvi_2bedroom.csv")
View(dataset)

dim(dataset)

#converting to a dataframe
dataset <- data.frame(dataset)

#considering data for New York Metro region and NY state
dataset <- dataset[dataset$Metro == 'New York' & dataset$State == 'NY',]

#creating a new column in the dataset to hold growth percentage
dataset$percentagechange <- (dataset$X2017.01-dataset$X2012.01)/dataset$X2012.01
dataset_desc <- dataset[order(-dataset$percentagechange),]

#considering top 10 neighborhoods in terms of growth
dataset_t10 <- head(dataset_desc, n=10)
View(dataset_t10$percentagechange)
install.packages("reshape")
library("reshape")
#transposing data into columns
transpose_data <- melt(dataset_t10, id.vars = c("RegionID", "RegionName", "City", "State", "Metro", "CountyName", "SizeRank"))
View(transpose_data)
#removing X from the column values
transpose_data$variable <- gsub("X", "", transpose_data$variable)
#formating date column
transpose_data$variable <- gsub("\\.", "-", transpose_data$variable)
transpose_data$variable <- paste(transpose_data$variable, "-01", sep = "")

library(ggplot2)
#converting string to Date
transpose_data$variable <- as.Date(transpose_data$variable)
transpose_data$RegionName <- as.character(transpose_data$RegionName)
View(transpose_data)
#plotting time series data for analysis
transpose_data$zip_region <- paste(transpose_data$RegionName, transpose_data$CountyName, sep = " - ")
lp <- ggplot(transpose_data, aes(x= variable, y= value, color=zip_region)) + 
  geom_line(linetype="solid", size=2) + labs(title=" Top 10 Regions in New York for Growth in Property Valuation", y="Value", x="Year")
lp



