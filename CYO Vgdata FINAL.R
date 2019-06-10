#I would like to thank the course's staff and stack overflow, whom I just met and where I found solace. 

#Greetings dear reviewer. 

#I decided to use the following dataset because it was something I understood. While I was looking for it I understood that it would be 
#impossible for me to take any value out of it without understanding what I was handling. My knowledge regarding biology are not great,
#so that was a big filter as well. 

#Unfortunetly this dataset doesn't include the N64 games, and it's only up to 2017. I don't know the reasons for that. But we can still 
#gain some really useful insights from it. 

#Everything shown in the following report is up to debate. It's perfectly natural to find disagreements with it, and I do not intend to 
#establish an unarguable result. I mean, I don't even know the things I still need to learn! But oh well, let's do our best. 

# As you may have seen from my user name, English is not my first language so I hope you may excuse me for any language 
# mistakes along the way. 

#The first section of the following code consists of Data cleaning and Shaping looking to find the top 10 games of our dataset.

#In the second section we will analyze performance of the Publishers against each other in sales. 

#Then, on the third section, we will see the console that perform the best from our dataset.

#And lastly, in the fourth section will see how good can we predict the Users acceptance of a game by checking the Critics Scores.


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(ggrepel)
library(gridExtra)
library(Rborist)
library(randomForest)
library(knitr)

url <- "https://raw.githubusercontent.com/juanpmendoza/Test/master/Video_Games_Sales_as_at_22_Dec_2016.csv"
vgdata <- read.csv(url)

#Let's check how many different inputs we have for the following categories:
n_vgdata <- vgdata %>% summarize(n_Games = n_distinct(Name),
                                 n_Platforms = n_distinct(Platform), 
                                 n_Genres = n_distinct(Genre),
                                 n_Publishers = n_distinct(Publisher))
n_vgdata

#Top 10 games. Data Exploration and cleaning.-----------------------------------------------------------------------------------------------------------------------

#We can see that there are a lot of Publishers:
n_vgdata$n_Publishers


#So trying to plot them all would be quite unproductive, not only because of the amount of them, but also the huge disparity between as we can see in 
#the following plot, where even if we take a log scale of 100, distance between publishers is gigantic! 
vg_by_publisher <- vgdata %>% group_by(Publisher) %>% summarize(count = n()) %>% 
  mutate(logcount = log(count, exp(100)))
vg_by_publisher %>% ggplot(aes(Publisher, logcount)) + geom_bar(stat = "identity")


#So we are going to have to take just a part of them into consideration, specially since there are some indie ones that are not really much of a match
#in terms of quality when compared to others (no offense). When checking the top 20 Publishers, we find the following:  
vgdata %>% group_by(Publisher) %>% summarize(count = n()) %>%  arrange(desc(count)) %>% print(n = 20)
#We can see that the #17 publisher in the list is "Unknown", we must then delete this for the sake of the analysis. We can do so with the following code:
vgdata <- vgdata %>% filter(!Publisher%in%c("Unknown"))
#We can then check and see that the games with Unknown Publishers have indeed been deleted. 
vgdata %>% group_by(Publisher) %>% summarize(count = n()) %>%  arrange(desc(count)) %>% print(n = 20)

#Now there's another issue to adress beforehand, and it's about the NAs present in both the Critic and User scores, since they will be important for our
#analysis. Let's adjust the print options so we can see the first 20 results and then check for NAs: 
options(max.print = 320)
is.na(vgdata) %>% print()

#We can see that there are indeed NAs present in the scores, we can delete them and then check again with the following code:
vgdata<- na.omit(vgdata)
is.na(vgdata) %>% print()

#Let's create a column with the average of the proportion of Score over Count, so we can use it as an estimate for the following step...
#Now we're going to divide the Critic_Count column by 10 for it to be on a 10 scale, like the User_Count and create an AvgP column with the result... 
vgdata <- vgdata %>% mutate(AvgP=(Critic_Count/10)/User_Count)
#Now let's create a column with the average of all the proportions, an "Average TOtal" (AvgT)
vgdata <- vgdata %>% mutate(AvgT = sum(AvgP)/(length(AvgP)))

#Now the reason we created the AvgT column is because we need it to balance the weight of the Critics' rate, since the values are too volatile.
#One would think that the critics are specialized people in the segment in which they operate, however, when it comes down to video games there
#usually are great discrepancies between them and the users, a particular example would be the following:
vgdata %>% filter(Name == "Call of Duty: Modern Warfare 3") 
#Notice the huge difference between audience and critic score. Critics usually get too technical (if they even try to do a proper critic), and
#I personally believe the users are more to trust since they focus more on the "Fun" of the game. Regardless of what may be true, the AvgT 
#column works to standardize a fixed proportion so we can aim to assess each game similarly. 
#I'm honestly not 100% sure if this is correct, but I sincerely hope it's not too wrong. 

#Now let's create a "True_Score" column with the weighted average of the two.
#We are giving the users' score a weight of almost twice the one of the critics' score. Of course this is a consideration opened to debate.  
vgdata <- vgdata %>% mutate(True_Score= ((Critic_Score/10)*AvgT*3)+(as.numeric(as.character(User_Score))*(1-AvgT*3)))
#I had to use the as.numeric(as.character(User_Score)) part because User_Score was a factor and for reasons that go waaaaaay beyond my current
#understandment, it had to be done like that when working with factors, god bless stackoverflow.com.

#Now we have a column with values over which to base our judgement. 

#Ok now let's reorganize the data frame from highest to lowest True_Score.  
vgdata_ordered <- vgdata %>%  arrange(desc(True_Score))
#However, we must notice that on the Name column we may find the same game more than once, this is because the very same game can be ported to
#different consoles, and the data set shows each. It makes sense to asume that a game is not enjoyable if it can't run properly, so let's keep
#just the best performing of each according to our True_Score,  this would also make it more "fair" for the games that were not ported. Since we had them already ordered by True_Score it suffices with just filtering
#the repeated names. 
vgdata_ordered <- vgdata_ordered[!duplicated(vgdata_ordered$Name),] 

#Now let's check the first 20 games sorted by True_score:
vgdata_ordered %>% group_by(Name, True_Score) %>% summarise() %>% arrange(desc(True_Score)) %>% print(n = 20)

#We can see that there are some parts of the ranking that don't really make sense. 
#Let's see an example. 
vgdata_ordered %>% filter(Name%in%c("Boktai: The Sun is in Your Hand"))
#We can see that the reason this game did so great in our analysis was because it had a tiny amount of User reviews with great score, which is a 
#similar scenario with the cult movies in the Movielens Project, so it makes sense to see cases like this as cult games, with great 
#score among its cult. This means that we must adjust our analysis and avoid games with minuscular amount of User reviews.

#Perhaps it makes sense to think that a great game would sell a lot, but this isn't a very safe assumption from what we saw with the 
#Call of Duty: MOdern Warfare check, some games can do great in sales thanks to it's marketing campaign or just because of its name. 
#Maybe it is safer to think that a great game would motivate a lot of users to leave great reviews. 
#I honestly have no idea, so let's check both and see how we do! 


#Let's check first at the top 50% games in global sales performance and order them by True_Score. We are pickign this amount of games for 
#further analysis.
vgdata_ordered_GSUC <- vgdata_ordered %>% arrange(desc(Global_Sales)) #Let's arrange the games from top to bottom in Global Sales.
#We'll call this object "vgdata_ordered_SC" since we are first filtering by global sales (GS) and then User Counts (UC)  
n <- length(vgdata_ordered_GSUC$Name) #Let's see how many games we have.
Q2 <- as.integer(n*0.5) #We now grab the half of them 
vgdata_ordered_GSUC <- vgdata_ordered_GSUC[1:Q2,] #Since we had already ordered them by sales, we are picking the first half.
vgdata_ordered_GSUC <- vgdata_ordered_GSUC %>% arrange(desc(User_Count)) #And now we arrange him by User_COunt
n <- length(vgdata_ordered_GSUC$Name) 
P90 <- as.integer(n*0.1) #We now grab the first 10% of the User_Counts since there are greatdisparities between  the amount of them, 
#and we want the best of the best here. It's not anything to be the game that was able to pull the biggest amount of User reviews for example. 
vgdata_ordered_GSUC <- vgdata_ordered_GSUC[1:P90,]
vgdata_ordered_GSUC %>% group_by(Name, True_Score) %>% summarise() %>% arrange(desc(True_Score)) %>% print(n = 20)

#Now let's try it the other order around
vgdata_ordered_CUSG <- vgdata_ordered %>% arrange(desc(User_Count)) 
n <- length(vgdata_ordered_CUSG$Name) 
Q2 <- as.integer(n*0.1) 
vgdata_ordered_CUSG <- vgdata_ordered_CUSG[1:Q2,] 
vgdata_ordered_CUSG <- vgdata_ordered_CUSG %>% arrange(desc(Global_Sales))
n <- length(vgdata_ordered_CUSG$Name)
P90 <- as.integer(n*0.5)
vgdata_ordered_CUSG <- vgdata_ordered_CUSG[1:P90,]
vgdata_ordered_CUSG %>% group_by(Name, True_Score) %>% summarise() %>% arrange(desc(True_Score)) %>% print(n = 20)

#The dataset at hand didn't have N64 games, so that's kind of a bummer, and it's only up to 2017. 
#Let's plot them together and see how they turned out. 
vgdata_ordered_GSUC_20 <- vgdata_ordered_GSUC %>%  arrange(desc(True_Score)) %>% .[1:20,]
P_GSUC_Bar <- vgdata_ordered_GSUC_20 %>% mutate(reorder = reorder(Name, True_Score))
P_GSUC_Bar <- vgdata_ordered_GSUC_20 %>% mutate(reorder = reorder(Name, True_Score)) %>%
  ggplot(aes(reorder, True_Score))+
  geom_text(aes(label=format(round(True_Score, 2), nsmall = 2)), size = 3, vjust = 1, angle = 90)+
  geom_bar(stat = "identity", color = "black", fill = "lightgray") +
  coord_flip(ylim = c(9, 9.5)) +
  theme(axis.text.y = element_text(size = 6)) +
  xlab("Games")+
  ylab("True_Score")+
  ggtitle("Filtered by G_Sales, then U_Count") +
  scale_color_discrete(name = "Platform") +
  theme_classic()

vgdata_ordered_CUSG_20 <- vgdata_ordered_CUSG %>%  arrange(desc(True_Score)) %>% .[1:20,]
P_CUSG_Bar <- vgdata_ordered_CUSG_20 %>% mutate(reorder = reorder(Name, True_Score))
P_CUSG_Bar <- vgdata_ordered_CUSG_20 %>% mutate(reorder = reorder(Name, True_Score)) %>%
  ggplot(aes(reorder, True_Score))+
  geom_text(aes(label=format(round(True_Score, 2), nsmall = 2)), size = 3, vjust = 1, angle = 90)+
  geom_bar(stat = "identity", color = "black", fill = "lightgray") +
  coord_flip(ylim = c(9, 9.5)) +
  theme(axis.text.y = element_text(size = 6)) +
  xlab("Games")+
  ylab("True_Score")+
  ggtitle("Filtered by U_Count, then G_Sales") +
  scale_color_discrete(name = "Platform") +
  theme_classic()

P_GSUC_Point <- vgdata_ordered_GSUC_20 %>% ggplot(aes(True_Score, Global_Sales, label = Name))+
  geom_point(aes(col= Platform), size = 3) +
  geom_text_repel() +
  ggtitle("Filtered by G_Sales, then U_Count") +
  xlab("True_Score") + 
  ylab("Global_Sales") +
  scale_color_discrete(name = "Platform") +
  theme_classic()

P_CUSG_Point <- vgdata_ordered_CUSG_20 %>% ggplot(aes(True_Score, Global_Sales, label = Name))+
  geom_point(aes(col= Platform), size = 3) +
  geom_text_repel() +
  ggtitle("Filtered by U_Count, then G_Sales") +
  xlab("True_Score") + 
  ylab("Global_Sales") +
  scale_color_discrete(name = "Platform") +
  theme_classic()

#Let's plot them together by Geom_Bar.
grid.arrange(P_GSUC_Bar, P_CUSG_Bar, ncol = 2)

#And let's see their performance in sales for more insight.
grid.arrange(P_GSUC_Point, P_CUSG_Point, nrow = 2)

#Now I am aware this could get into a deeply subjective argument. I personally have some disagreements with the results but I believe the parameters
#we used have a certain degree of being right. This is all of course opened to discussion (please remember there weren't any N64 games in the dataset).
#Under my personal judgement I believe that the more prudent resulting arrangements would be the ones produced by filtering first by Global_Sales
#and then by User_Counts, since the games in the first one seem to fit more than the ones in the second. 

#I would then believe the following to be the top 10 games from our dataset:

vgdata_ordered_GSUC_top10 <- vgdata_ordered_GSUC %>% arrange(desc(True_Score)) %>% .[1:10,]
top10vg <- vgdata_ordered_GSUC_top10 %>% group_by(Name, Platform, Publisher, Year_of_Release, True_Score) %>% summarise() %>% arrange(desc(True_Score))

top10vg %>% knitr::kable()

#And there we go! The Top 10 games from the data we have.

#But there's still so much to see from the power that R gives us! 

#Region outcomes ----------------------------------------------------------------------------------------------------------------------------------------


#Let's check the results of every region when choosing the top 10 selling games and their True_Score

#Let's begin globally. 
vgdata_sales_global <- vgdata_ordered %>% arrange(desc(Global_Sales)) %>% .[1:10,]

Global_Sales_Point <- vgdata_sales_global %>% ggplot(aes(True_Score, Global_Sales, label = Name))+
  geom_point(aes(col= Publisher), size = 3) +
  geom_text_repel() +
  ggtitle("Top 10 GS ") +
  xlab("True_Score") + 
  ylab("Global_Sales") +
  scale_color_discrete(name = "Publisher") +
  theme_classic()
Global_Sales_Point
#We can see that Nintendo has a strong position with high True Scores 


vgdata_sales_NA <- vgdata_ordered %>% arrange(desc(NA_Sales)) %>% .[1:10,]

NA_Sales_Point <- vgdata_sales_NA %>% ggplot(aes(True_Score, NA_Sales, label = Name))+
  geom_point(aes(col= Publisher), size = 3) +
  geom_text_repel() +
  ggtitle("Top 10 NA sales ") +
  xlab("True_Score") + 
  ylab("NA_Sales") +
  scale_color_discrete(name = "Publisher") +
  theme_classic()
NA_Sales_Point
#Nintendo also has a strong position in North American sales, although we can see two more publishers than before. 

vgdata_sales_EU <- vgdata_ordered %>% arrange(desc(EU_Sales)) %>% .[1:10,]

EU_Sales_Point <- vgdata_sales_EU %>% ggplot(aes(True_Score, EU_Sales, label = Name))+
  geom_point(aes(col= Publisher), size = 3) +
  geom_text_repel() +
  ggtitle("Top 10 EU sales ") +
  xlab("True_Score") + 
  ylab("EU_Sales") +
  scale_color_discrete(name = "Publisher") +
  theme_classic()
EU_Sales_Point
#We can see that Nintendo dominates the top 10 selling games in the EU 

vgdata_sales_JP <- vgdata_ordered %>% arrange(desc(JP_Sales)) %>% .[1:10,]

JP_Sales_Point <- vgdata_sales_JP %>% ggplot(aes(True_Score, JP_Sales, label = Name))+
  geom_point(aes(col= Publisher), size = 3) +
  geom_text_repel() +
  ggtitle("Top 10 JP sales ") +
  xlab("True_Score") + 
  ylab("JP_Sales") +
  scale_color_discrete(name = "Publisher") +
  theme_classic()
JP_Sales_Point
#The top 10 games sold in Japan are also dominated by Nintendo, Capcom is the only Publisher besides it with one game. 



#Now let's check who the top 10 Publishers would be ordered by Global Sales ---------------------------------------------------
Top_Pub <- vgdata_ordered %>%
  group_by(Publisher) %>% summarise(Global_Sales_Per_Pub = sum(Global_Sales), Games_Published = n(), Average_Score = sum(True_Score)/n()) %>% arrange(desc(Global_Sales_Per_Pub)) %>% .[1:10,]
Top_Pub
#Nintendo wins in the sales area, and it's also the one with the highest Average Score among the 10.

#We could check the order by Score but we run once again into the segmentation issue. The publisher with the best score has only one game published. 
Top_Pub2 <- vgdata_ordered %>%
  group_by(Publisher) %>% summarise(Global_Sales_Per_Pub = sum(Global_Sales), Games_Published = n(), Average_Score = sum(True_Score)/n()) %>% arrange(desc(Average_Score)) %>% .[1:10,]
Top_Pub2

#Let's graph the performance of the Publishers ordered by sales. 
Top_Pub_Point <-Top_Pub %>% ggplot(aes(Average_Score, Global_Sales_Per_Pub, label = Publisher))+
  geom_point(size = 3) +
  geom_text_repel() +
  ggtitle("Top 10 Publishers") +
  xlab("Average Score") + 
  ylab("Global Sales Per Publisher") +
  scale_color_discrete(name = "Publisher") +
  theme_classic()
Top_Pub_Point
#So yeah, Nintendo has an overwhelming position when noticing that it has almost twice the sales of the #2 Publisher, while maintaining a high Score. 

#Now let's check how every Publisher did in Sales using a boxplot, to check the statistical distribution of each Publisher's performance.
Pub_List <- Top_Pub$Publisher

Top_Pub_Box <- vgdata_ordered %>% dplyr::filter(Publisher %in% Pub_List) %>% 
  mutate(median = median(Global_Sales)) %>% mutate(reordered = reorder(Publisher, median, order = TRUE)) 
Top_Pub_Box %>% 
  ggplot(aes(reordered, Global_Sales, fill = Publisher)) + 
  geom_boxplot(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Publishers") +
  ylab("Global Sales Performance ")

#Well that's not so informatieve, although we can see that the disparity of the distribution is huge, and that the most selling game that
# each Publisher has greatly outperforms the rest. It's quite a heterogeneous distribution. Let's use coord_cartesian() to zoom in
Top_Pub_Box %>% 
  ggplot(aes(reordered, Global_Sales, fill = Publisher)) + 
  geom_boxplot(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_cartesian(
    ylim = c(0,4)
  )+
  xlab("Publishers") +
  ylab("Global Sales Performance ")
#Better! We can now have a better glimpse of the usual performance that each Publisher has. Nintendo still has a superior performance.

#I know there may be disagreements around these results, but we have to keep in mind that the dataset doesn't have the N64 games, which 
#would give Nintendo a huge boost. So for the time being and with the data at hand, I believe Nintendo would be the winner of the 
#"Publisher Wars". 


#Now let's check on the consoles! -------------------------------------------------------------------------------------------------------------------------------------

#Let's see how each console performed when comparing Global Sales and Average Score
Platform_wars <- vgdata_ordered %>%
  group_by(Platform) %>% summarise(Global_Sales_Per_Platform = sum(Global_Sales), Games_Published = n(), Average_Score = sum(True_Score)/n()) %>% arrange(desc(Global_Sales_Per_Platform)) %>% .[1:10,]
Platform_wars

Platform_wars_point <-Platform_wars %>% ggplot(aes(Average_Score, Global_Sales_Per_Platform, label = Platform))+
  geom_point(size = 3) +
  geom_text_repel() +
  ggtitle("Platform Wars") +
  xlab("Average Score") + 
  ylab("Global Sales Per Platform") +
  scale_color_discrete(name = "Platform") +
  theme_classic()
Platform_wars_point
#Now despite Nintendo's predominance in sales, we can see that when we check by console, the PS2 has a great advantage over the rest.


#Now let's see how each Platform performed using a boxplot. 
Platform_List <- as.character(unique(vgdata_ordered$Platform))

Platform_wars_box <- vgdata_ordered %>% dplyr::filter(Platform %in% Platform_List) 

Platform_wars_box%>% 
  ggplot(aes(Platform, Global_Sales)) + 
  geom_boxplot(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Platforms") +
  ylab("Global Sales Performance ")
#Let's use coord() to zoom up to 2.5M so we can see the distributions better
Platform_wars_box%>% 
  ggplot(aes(Platform, Global_Sales)) + 
  geom_boxplot(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_cartesian(
    ylim = c(0,2.5)
  )+
  xlab("Platforms") +
  ylab("Global Sales Performance ")
#There we go. We can see that even if the distribution of the Ps2 doesn't seem to be that big of a deal, the amount of games it had pushed it to
#the top in sales. It could be said that it was because of a nice timing for the PS2 in the game industry, but when we see that despite its great
#amount of games it manages to keep the second best score among the top 10, we have to give it some credit. 

#The winner of the console wars would be the PS2. 

#Machine Learning Algorithm.-----------------------------------------------------------------------------------------------------------------------

#We can see that there is a certain tendency between the score provided by the users and the one provided by the Critics. Let's see if we can make 
#an algorithm for this.

vgdata_ordered %>% arrange(desc(True_Score)) %>% ggplot(aes(User_Score, Critic_Score/10)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90))

#Let's try to predict user score using critic's score, since it's how it's supposed to work right? Let's check out!------------------------

#I know I'm not supposed to use lm here, but I thought it would be of use to have its result as a reference, and to see if it was possible to do this.

set.seed(2)

vgdataml <- vgdata_ordered
vgdataml$User_Score <- as.numeric(as.character(vgdataml$User_Score))
vgdataml$Critic_Score <- as.numeric(vgdataml$Critic_Score)/10

y <- as.numeric(vgdataml$User_Score) 
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- vgdataml %>% slice(-test_index)
test_set <- vgdataml %>% slice(test_index)
fit <- lm(User_Score ~ Critic_Score, data = train_set)
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$User_Score)^2)

#Well that didn't go too bad.

#Now let's try with other different approaches! 

#Please keep in mind that the $User_Score column is a factor, hence why I transform it frequently. 


train_set$User_Score <- as.numeric(train_set$User_Score)
test_set<- test_set[-c(2217:2219),]

#Trying Knn ---------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2)
train_knn <- train(User_Score ~ Critic_Score, method = "knn",
                   data = train_set,
                   trControl = trainControl(
                     method = "cv", number = 15, 
                     verboseIter = TRUE
                   ),
                   tuneGrid = data.frame(k = seq(0.5, 50, 0.5)))

y_hat_knn <- predict(train_knn, test_set, type = "raw", na.action = na.pass)

#I had to round up the y_hat prediction values because, unlike them, the User Scores had only one decimal, and the Accuracy value was always zero. 
y_hat_knn <- round(y_hat_knn,digits=1)

#Now I must clarify something. When I was making this code I tried several ML algorithms, but none of them seemed to work for me. There was a huge amount of different errors.
#I tried different ways to solve them, but none really seemed to work. I had no idea how to proceed, until I found the following 
#code in Stack Overflow that arranged the data without mistales 

predicted <- factor(y_hat_knn)
real <- factor(test_set$User_Score)
my_data1 <- data.frame(data = predicted, type = "prediction")
my_data2 <- data.frame(data = real, type = "real")
my_data3 <- rbind(my_data1,my_data2)
identical(levels(my_data3[my_data3$type == "prediction",1]) , levels(my_data3[my_data3$type == "real",1]))

#And then I could get the Accuracy!
Accuracy_knn <-confusionMatrix(my_data3[my_data3$type == "prediction",1], my_data3[my_data3$type == "real",1],  dnn = c("Prediction", "Reference"))$overall["Accuracy"]
Accuracy_knn


#Trying rpart ------------------------------------------------------------------------------------------------------------------------------------------------------------

#Now let's see how we do with Rpart. 
set.seed(2)
train_rpart <- train(User_Score ~ Critic_Score,
                     method = "rpart",
                     data = train_set,
                     trControl = trainControl(
                       method = "cv", number = 15, 
                       verboseIter = TRUE
                     ),
                     tuneGrid = data.frame(cp = seq(0.0, 0.05, len = 25))
)

y_hat_rpart <- predict(train_rpart, train_set, type = "raw", na.action = na.pass)
y_hat_rpart <- round(y_hat_rpart,digits=1)
predicted <- factor(y_hat_rpart)
real <- factor(test_set$User_Score)
my_data1 <- data.frame(data = predicted, type = "prediction")
my_data2 <- data.frame(data = real, type = "real")
my_data3 <- rbind(my_data1,my_data2)
identical(levels(my_data3[my_data3$type == "prediction",1]) , levels(my_data3[my_data3$type == "real",1]))
Accuracy_rpart <- confusionMatrix(my_data3[my_data3$type == "prediction",1], my_data3[my_data3$type == "real",1],  dnn = c("Prediction", "Reference"))$overall["Accuracy"]
Accuracy_rpart


#Trying Random Forest -----------------------------------------------------------------------------------------------------------------------------------------------------------

#I tried using Random Forest but I couldn't get it to work :/ 
set.seed(2)
train_rf <- train(User_Score ~ Critic_Score,
                  method = "Rborist",
                  trControl = trainControl(
                    method = "cv", number = 15, 
                    verboseIter = TRUE
                  ),
                  tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                  data = train_set)
#It gets stopped and I honestly don't know why. It's pointless to run the rest of the code since it would just use the 
#"my_data1" and the rest from Rpart. 

#And so our results are...
Results <- data_frame(method = "Knn", 
                      Accuracy = Accuracy_knn)

Results <- bind_rows(Results,
                     data_frame(method="Rpart",
                                Accuracy = Accuracy_rpart))

Results <- bind_rows(Results, 
                     data_frame(method="Random Forest",
                                Notes = "Couldn't make it work :c"))

Results %>% knitr::kable()

#There we have it everyone! I guess Critics Score are not a useful way to see how the users will judge a game!


# End of the code ###################################################################################################################################################################################################

#Thank you dear reviewer for checking my submission. Once again every result provided here is of course up to debate. 
#If by any reason you do not consider my code to be up to expectations, I will appreaciate your comments and keep them
#in mind for the future. 

# Thank you for your time and best of lucks for you too on the Capstone course, may we all make the best of it!







