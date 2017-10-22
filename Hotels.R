#
# Notes: create Shiny app where bar for number of positive words changes grid charts.
full = read.csv('Hotel_Reviews.csv')

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(leaflet.extras)
library(grid)
library(gridExtra)
library(scales)
library(stringr)
source('functions.R')

# Basic Transformations

full$Review_Range = cut(full$Reviewer_Score, breaks=1.5:11, include.lowest=T)
full$lng[is.na(full$lng)] <- 0
full$lat[is.na(full$lat)] <- 0
full$Country = coords2country(cbind(full$lng, full$lat))
full$Total_Words = full$Review_Total_Negative_Word_Counts + full$Review_Total_Positive_Word_Counts
full$Positive_Word_Rate = full$Review_Total_Positive_Word_Counts/full$Total_Words

full$Review_Range = as.factor(cut(full$Reviewer_Score, breaks=1.5:11, include.lowest=T))
full$Review_Range = gsub(']',')',full$Review_Range)
full$Review_Range = factor(full$Review_Range, levels = c('(9.5,10.5)','(8.5,9.5)',
                                                         '(7.5,8.5)','(6.5,7.5)',
                                                         '(5.5,6.5)','(4.5,5.5)',
                                                         '(3.5,4.5)','(2.5,3.5)',
                                                         '[1.5,2.5)'))

full$Country_2 = word(as.character(full$Hotel_Address),start = -2, end=-1)



#Get stats by unique hotel

hotel.names = full %>%
    select(Hotel_Name, Hotel_Address, lat, lng, Country, Average_Score, Total_Number_of_Reviews,
           Review_Total_Positive_Word_Counts, Review_Total_Negative_Word_Counts) %>%
    #Remove the 17 records without geo coordinates
    #filter(lat != 0 & lng != 0) %>%
    group_by(Hotel_Name, Hotel_Address, lat, lng, Country,Average_Score, Total_Number_of_Reviews) %>%
    summarise(Tot_Pos_Words = sum(Review_Total_Positive_Word_Counts),
              Tot_Neg_Words = sum(Review_Total_Negative_Word_Counts),
              Total_Words = sum(Tot_Pos_Words + Tot_Neg_Words),
              Pos_Word_Rate = percent(Tot_Pos_Words/Total_Words),
              Neg_Word_Rate = percent(Tot_Neg_Words/Total_Words),
              Number_Reviews = n()) #%>%
    #use fx to get country from lat/lng
    #mutate(Country = coords2country(cbind(lng,lat)))


#Get Hotel Countries with Review_Range derived variable.
# Step 2: country/NumberHotels (find count distinct Hotel_Name)
# Step 3: ggplot
#__________________________________________________________________________________________
country.review_range = full %>%
    select(Country, Review_Range, Total_Number_of_Reviews,
           Review_Total_Positive_Word_Counts, Review_Total_Negative_Word_Counts, Hotel_Name) %>%
    # #Remove the 17 records without geo coordinates
    # filter(lat != 0 & lng != 0) %>%
    group_by(Country, Review_Range) %>%
    summarise(Tot_Pos_Words = sum(Review_Total_Positive_Word_Counts),
              Tot_Neg_Words = sum(Review_Total_Negative_Word_Counts),
              Total_Words = sum(Tot_Pos_Words + Tot_Neg_Words),
              Pos_Word_Rate = percent(Tot_Pos_Words/Total_Words),
              Neg_Word_Rate = percent(Tot_Neg_Words/Total_Words),
              Num_Reviews = n()
             )

sum(country.review_range$Num_Reviews)
n_distinct(full$Hotel_Name)

review_range = full %>%
    select(Review_Range, Total_Number_of_Reviews,
           Review_Total_Positive_Word_Counts, Review_Total_Negative_Word_Counts, Hotel_Name) %>%
    # #Remove the 17 records without geo coordinates
    # filter(lat != 0 & lng != 0) %>%
    group_by(Review_Range) %>%
    summarise(Tot_Pos_Words = sum(Review_Total_Positive_Word_Counts),
              Tot_Neg_Words = sum(Review_Total_Negative_Word_Counts),
              Total_Words = sum(Tot_Pos_Words + Tot_Neg_Words),
              Pos_Word_Rate = percent(Tot_Pos_Words/Total_Words),
              Neg_Word_Rate = percent(Tot_Neg_Words/Total_Words),
              Num_Reviews = n(),
              Avg_Words_Per_Review = format(Total_Words/Num_Reviews,digits = 4)
    )


#__________________________________________________________________________________________
#                   Plot Review Range df
#__________________________________________________________________________________________

#plot rev rng df
library(grid)
library(gridExtra)

p1 <- ggplot(review_range, aes(x=Review_Range,
                         y=Num_Reviews,
                         fill=Review_Range))+
    geom_bar(stat='identity', alpha=0.7)+
    geom_text(label=review_range$Num_Reviews,nudge_y = 0.5)+
    ggtitle(label="Number of Reviews by Range of Score")+
    labs(xlab('Review Range'),ylab('Number of Reviews'))+
    theme(legend.position = 'off')

p2 <- ggplot(review_range, aes(x=Review_Range,
                               y=Pos_Word_Rate,
                               fill=Review_Range))+
    geom_bar(stat='identity', alpha=0.7)+
    geom_text(label=review_range$Pos_Word_Rate,nudge_y = 0.2)+
    ggtitle(label="Percent Positive Words by Range of Score")+
    labs(xlab('Review Range'),ylab('Percent(%) Positive Words'))+
    theme(legend.position = 'bottom')

p3 <- ggplot(review_range, aes(x=Review_Range,
                               y=Avg_Words_Per_Review,
                               fill=Review_Range))+
    geom_bar(stat='identity', alpha=0.7)+
    geom_text(label=review_range$Avg_Words_Per_Review,nudge_y = 0.2)+
    ggtitle(label="Average Words per Review by Range of Score")+
    labs(xlab('Review Range'),ylab('Avg. Words per Review'))+
    theme(legend.position = 'off')

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)

grid.draw(rbind(g1,g3, g2, size='last'))
#__________________________________________________________________________________________
zero_positive <- filter(full, Review_Total_Positive_Word_Counts == 0 & Review_Total_Negative_Word_Counts > 0)

zero_pos_rng = zero_positive %>%
    select(Review_Range, Total_Number_of_Reviews, Review_Total_Negative_Word_Counts, Hotel_Name) %>%
    # #Remove the 17 records without geo coordinates
    # filter(lat != 0 & lng != 0) %>%
    group_by(Review_Range) %>%
    summarise(Total_Words = sum(Review_Total_Negative_Word_Counts),
              Num_Reviews = n(),
              Avg_Words_Per_Review = format(Total_Words/Num_Reviews,digits = 4)
              )


# -- -- -- -- -- -- -- -- -- --     >      >      >     >
p4 <- ggplot(zero_pos_rng, aes(x=Review_Range,
                               y=Num_Reviews,
                               fill=Review_Range))+
    geom_bar(stat='identity', alpha=0.7)+
    geom_text(label=zero_pos_rng$Num_Reviews,nudge_y = 0.5)+
    ggtitle(label="Negative Words Only - Number of Reviews by Range of Score")+
    labs(xlab('Review Range'),ylab('Number of Reviews'))+
    scale_fill_brewer(palette = 'Set3')+
    theme(legend.position = 'off')

p5 <- ggplot(zero_pos_rng, aes(x=Review_Range,
                               y=Total_Words,
                               fill=Review_Range))+
    geom_bar(stat='identity', alpha=0.7)+
    geom_text(label=zero_pos_rng$Total_Words,nudge_y = 0.2)+
    ggtitle(label="Number of Negative Words by Range of Score")+
    labs(xlab('Review Range'),ylab('Number of Negative Words'))+
    scale_fill_brewer(palette = 'Set3')+
    theme(legend.position = 'off')

p6 <- ggplot(zero_pos_rng, aes(x=Review_Range,
                               y=Avg_Words_Per_Review,
                               fill=Review_Range))+
    geom_bar(stat='identity', alpha=0.7)+
    geom_text(label=zero_pos_rng$Avg_Words_Per_Review,nudge_y = 0.2)+
    ggtitle(label="Negative Words Only - Average Words per Review by Range of Score")+
    labs(xlab('Review Range'),ylab('Avg. Words per Review'))+
    scale_fill_brewer(palette = 'Set3')+
    theme(legend.position = 'bottom')

g4 <- ggplotGrob(p4)
g5 <- ggplotGrob(p5)
g6 <- ggplotGrob(p6)

grid.draw(rbind(g4,g5,g6, size='last'))

### ---------###     ------------####      -----#####
#
#  Positive Reviews only

zero_negative <- filter(full, Review_Total_Positive_Word_Counts > 0 & Review_Total_Negative_Word_Counts == 0)

zero_neg_rng = zero_negative %>%
    select(Review_Range, Total_Number_of_Reviews, Review_Total_Positive_Word_Counts, Hotel_Name) %>%
    # #Remove the 17 records without geo coordinates
    # filter(lat != 0 & lng != 0) %>%
    group_by(Review_Range) %>%
    summarise(Total_Words = sum(Review_Total_Positive_Word_Counts),
              Num_Reviews = n(),
              Avg_Words_Per_Review = format(Total_Words/Num_Reviews,digits = 4)
    )


# -- -- -- -- -- -- -- -- -- --     >      >      >     >
p7 <- ggplot(zero_neg_rng, aes(x=Review_Range,
                               y=Num_Reviews,
                               fill=Review_Range))+
    geom_bar(stat='identity', alpha=0.7)+
    geom_text(label=zero_neg_rng$Num_Reviews,nudge_y = 0.5)+
    ggtitle(label="Reviews with Only Positive Words - Number of Reviews by Range of Score")+
    labs(xlab('Review Range'),ylab('Number of Reviews'))+
    scale_fill_brewer(palette = 'Pastel1')+
    theme(legend.position = 'off')

p8 <- ggplot(zero_neg_rng, aes(x=Review_Range,
                               y=Avg_Words_Per_Review,
                               fill=Review_Range))+
    geom_bar(stat='identity', alpha=0.7)+
    geom_text(label=zero_neg_rng$Avg_Words_Per_Review,nudge_y = 0.2)+
    ggtitle(label="Reviews with Only Positive Words - Average Words per Review by Range of Score")+
    labs(xlab('Review Range'),ylab('Avg. Words per Review'))+
    scale_fill_brewer(palette = 'Pastel1')+
    theme(legend.position = 'off')


p9 <- ggplot(zero_neg_rng, aes(x=Review_Range,
                               y=Total_Words,
                               fill=Review_Range))+
    geom_bar(stat='identity', alpha=0.7)+
    geom_text(label=zero_pos_rng$Total_Words,nudge_y = 0.2)+
    ggtitle(label="Reviews with Only Positive Words - Number of Positive Words by Range of Score")+
    labs(xlab('Review Range'),ylab('Number of Positive Words'))+
    scale_fill_brewer(palette = 'Pastel1')+
    theme(legend.position = 'bottom')

g7 <- ggplotGrob(p7)
g8 <- ggplotGrob(p8)
g9 <- ggplotGrob(p9)

grid.draw(rbind(g7,g8,g9, size='last'))

# ------- # ------- # ------- # -------- # -------- # ------- #
#hotel.names$Country = coords2country(cbind(hotel.names$lng,hotel.names$lat))

#Get stats by country
country.stats = hotel.names %>%
    select(Country, Average_Score, Total_Number_of_Reviews, Tot_Pos_Words, Tot_Neg_Words,
           Total_Words) %>%
    group_by(Country) %>%
    summarize(Avg_Hotel_Review = mean(Average_Score),
              Positive_Words = sum(Tot_Pos_Words),
              Negative_Words = sum(Tot_Neg_Words),
              Total_Words = sum(Total_Words),
              Pos_Word_Rate = percent(Positive_Words/Total_Words),
              Neg_Word_Rate = percent(Negative_Words/Total_Words),
              Number_Hotels = n(),
              Total_Number_of_Reviews = sum(Total_Number_of_Reviews))

#Plot country stats
ggplot(data = country.stats, aes(x=reorder(Country,-Number_Hotels), y=Number_Hotels, fill=Country))+
    geom_bar(stat='identity', alpha=0.9) +
    geom_text(label=format(country.stats$Avg_Hotel_Review, digits = 2)) +
    labs(x='Country', y='Number of Hotels Reviewed') +
    ggtitle(label='Number of Hotels Reviewed & Avg. Score by Country')+
    scale_fill_brewer(palette = "Pastel2") +
    theme_hc()
    

#---------------------------------------------------------------------------- #
#---------------------------------------------------------------------------- #
# ------------------------------ Reviewer Stats ------------------------------ #
# get percentiles so can plot the top percentiles of nationalities represented by 
# number of reviews or number of words




nationality_country = full %>%
    select(Reviewer_Nationality, Country, Total_Number_of_Reviews,
           Review_Total_Positive_Word_Counts, Review_Total_Negative_Word_Counts, Hotel_Name) %>%
    # #Remove the 17 records without geo coordinates
    # filter(lat != 0 & lng != 0) %>%
    group_by(Reviewer_Nationality, Country) %>%
    summarise(Tot_Pos_Words = sum(Review_Total_Positive_Word_Counts),
              Tot_Neg_Words = sum(Review_Total_Negative_Word_Counts),
              Total_Words = sum(Tot_Pos_Words + Tot_Neg_Words),
              Pos_Word_Rate = percent(Tot_Pos_Words/Total_Words),
              Neg_Word_Rate = percent(Tot_Neg_Words/Total_Words),
              Num_Reviews = n(),
              Avg_Words_Per_Review = format(Total_Words/Num_Reviews,digits = 4)
    )


#---------------------------------------------------------------------------- #
#--------------------------------------------------------------------------- #
#-------------------------------------------------------------------------- #
# ------------------------------ Leaflet Map ----------------------------- #
#-------------------------------------------------------------------------- #
#--------------------------------------------------------------------------- #
#---------------------------------------------------------------------------- #


points <- cbind(hotel.names$lng,hotel.names$lat)

leaflet() %>% 
    addProviderTiles('OpenStreetMap.Mapnik',
                     options = providerTileOptions(noWrap = TRUE)) %>%
    addMarkers(data = points,
               popup = paste0("<strong>Hotel: </strong>",
                              hotel.names$Hotel_Name,                 
                              "<br><strong>Address: </strong>", 
                              hotel.names$Hotel_Address, 
                              "<br><strong>Average Score: </strong>", 
                              hotel.names$Average_Score, 
                              "<br><strong>Number of Reviews: </strong>", 
                              hotel.names$Total_Number_of_Reviews,
                              "<br><strong>Percent Positive Review Words: </strong>",
                              hotel.names$Pos_Word_Rate),
               clusterOptions = markerClusterOptions())

#-------------------------------------------------------------------------- #
# Explore Dataset
#-------------------------------------------------------------------------- #
zero_positive <- filter(full, Review_Total_Positive_Word_Counts == 0)
zero_negative <- filter(full, Review_Total_Negative_Word_Counts == 0)

tag_col <- full$Tags

tags = strsplit(as.character(tag_col),split = ',')


# Look at Top Hotels
Top = full %>%
    filter(Average_Score >= 9)
#-------------------------------------------------------------------------- #
#------------------------------------------------------------------------- #
#------------------------------------------------------------------------ #
#----------------------------------------------------------------------- #
#------------------------------------------------------------------------ #
#------------------------------------------------------------------------- #
#-------------------------------------------------------------------------- #
#
# -------           Machine Learning                ------------------------ #
#
#-------------------------------------------------------------------------- #
#------------------------------------------------------------------------- #
#------------------------------------------------------------------------ #
#----------------------------------------------------------------------- #
#---------------------------------------------------------------------- #
#--------------------------------------------------------------------- #

library(caret)

#Try one hotel

arena = full[full$Hotel_Name =='Hotel Arena',]
davinci = full[full$Hotel_Name =='Hotel Da Vinci',]
britannia = full[full$Hotel_Name == 'Britannia International Hotel Canary Wharf',]
    
iT = createDataPartition(full$Average_Score, p=0.75, list=F)
train = full[iT,]
test = full[-iT,]

forest = train(Reviewer_Score ~ ., data = train, method = 'gbm')
forest

