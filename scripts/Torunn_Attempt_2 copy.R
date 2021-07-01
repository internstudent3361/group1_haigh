

## Choosing packages and cleaning up the data

library(tidyverse)
library(ggplot2)

# READ DATA -----------------------------------------------------------

Exp_1_data <- read_csv(file= "Study_1_data.csv") # Named my data Exp_1_data (Experiment 1 data)

# CLEAN UP -----------------------------------------------------------

Exp_1_data <- Exp_1_data %>% slice(-1:-2) #removes lines 2 and 3 

Exp_1_data <- as.data.frame(Exp_1_data) # changes data into a data frame so `filter()` can work 

Exp_1_data <- Exp_1_data %>% 
  rename(recall_score = SC0, condition = FL_10_DO) # rename incomprehensible variable names 

## Exclusions - unplanned and planned

#Unplanned - In the paper the authors report that while only 312 people singed up for the study, there were a total of 371 survey responses. The authors realised that some of the participants had completed the survey twice so their second attempt at the survey had to be removed. After some googling I discovered that the `distinct()` function did this. However, when I ran the code I found that it had also removed all other variables. After some more googling I found that by using the argument `.keep_all = TRUE`, I was able to remove duplicate lines (based on the Prolific_PID variable) whilst keeping all other variables.

# Excluding 2nd attempts  -------------------------------------------------

Exp_1_data <- Exp_1_data %>% 
  distinct(Prolific_PID, .keep_all = TRUE) 


# Planned - I then used `filter()` to do the pre-registered exclusions.


# Planned exclusions  -----------------------------------------------------

Exp_1_data <- Exp_1_data %>%
  filter(Finished == 1, Serious_check == 1, recall_score >= 4) # Incomplete task, no serious response, failed attention check 


## Descriptives - After making all the necessary exclusions and determining that the number of participants that were still included matched the reported number in the paper (294 participants), I could get started on the descriptives:


## No. of participants: 294 
count(Exp_1_data)


## Number of female and male participants**
# By looking at the survey (provided in the OSF files) I could see that males were coded as 1 and females as 2 (seems a lil sexist if you ask me!). 

Exp_1_data %>% count(Gender) 


#Age Demographics

# Mean Age: 34.29, SD: 12.97, Age Range: 18-69

Exp_1_data$Age <- as.numeric(Exp_1_data$Age) #changing 'Age' from character to numeric 

Exp_1_data %>% summarise(mean_age = mean(Age),
                         SD_age = sd(Age), 
                         min_age = min(Age), 
                         max_age = max(Age)
                         ) 

## Finding our independent and dependent variables
# The variable 'condition' is a string that needs to be separated (by underscore) so that we can have the two IVs, Format and Conflict, as separate variables. An example of how the condition variable is originally written is: 'Block_1_Generic_Conflict". By separating them, 'block' and 'number' become meaningless and 'format' and 'conflict' become their own variables.    

# Separating the IVs

Exp_1_data <- Exp_1_data %>% 
  separate(col = condition, into = c("block", "number", "Format", "Conflict"))

# The next step was to change the new variables into factors. We also renamed the factors of the variable 'Conflict' so it would be more readable in the plot.

# changing IV from characters to factors 
Exp_1_data$Format <- as.factor(Exp_1_data$Format)

Exp_1_data$Conflict <- as.factor(Exp_1_data$Conflict)

# changing DV from characters to numeric 
Exp_1_data$advancement <- as.numeric(Exp_1_data$advancement)

Exp_1_data$confusion <- as.numeric(Exp_1_data$confusion) 

# renaming factors of 'Conflict'   
Exp_1_data$Conflict <- recode(Exp_1_data$Conflict, "Conflict"="Conf.", "Consistent"="Non-Conf.")


# We noticed that in the data the DV 'Contradiction' was separated into 6 different ratings. We needed to combine them to get one contradiction rating for each participant. The `rowwise()` function allows you to sum across the selected columns for each row (participant) individually. Without this function the contradiction rating for every participant was the same. 
# But first, it was necessary to convert the contradiction variables into numeric form so that the `mutate()` function would work.

# Change from Character to Numeric for all contradiction variables
Exp_1_data$contradiction_1 <- as.numeric(Exp_1_data$contradiction_1)
Exp_1_data$contradiction_2 <- as.numeric(Exp_1_data$contradiction_2)
Exp_1_data$contradiction_3 <- as.numeric(Exp_1_data$contradiction_3)
Exp_1_data$contradiction_4 <- as.numeric(Exp_1_data$contradiction_4)
Exp_1_data$contradiction_5 <- as.numeric(Exp_1_data$contradiction_5)
Exp_1_data$contradiction_6 <- as.numeric(Exp_1_data$contradiction_6)

#Create new variable called "Contradiction" that is the sum of all 6 contradiction ratings

Exp_1_data <- Exp_1_data %>% 
  rowwise() %>% 
  mutate(
    Contradiction = sum(
      contradiction_1, contradiction_2, contradiction_3, contradiction_4, contradiction_5, contradiction_6))

## Making some plots


# Contradiction plot
contradiction_plot <- ggplot(
  data = Exp_1_data) + 
  geom_violin(mapping = aes(x = Conflict, y = Contradiction, fill = Conflict)) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00")) +
  ggtitle(label = "Contradiction") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title of the plot 
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Perceived Contradiction", limits = c(0,30)) + # naming y axis and setting the scale to 0-30
  facet_wrap(vars(Format), strip.position = "bottom") + #dividing the plot by format (generic/qualified) and positioning the label at the bottom of the plot
  geom_boxplot(mapping = aes(x = Conflict, y = Contradiction), width = 0.2)
  
print(contradiction_plot)


# Advancement plot 
advancement_plot <- ggplot(Exp_1_data) +  
  geom_violin(aes(x= Conflict, y = advancement)) + 
  ggtitle(label= "Advancement") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name= "Perceived Scientific Advancement", limits=c(-1,1)) +
  facet_wrap(vars(Format), strip.position = "bottom") 
  
              
print(advancement_plot)

# Confusion plot 
confusion_plot <- ggplot(Exp_1_data) +
  geom_violin(aes(x= Conflict, y = confusion)) + 
  ggtitle(label= "Confusion") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name= "Perceived Confusion", limits= c(1,5)) + 
  facet_wrap(vars(Format), strip.position= "bottom")

print(confusion_plot)

# Removing some variables 
Data_refined <- Exp_1_data %>% select(contradiction_1:Contradiction) %>% 
  as_tibble(Data_refined)


glimpse(Data_refined)


#Figure 2. Histogram displaying the number of participants in each condition who felt the body of
#research reported in the headlines resulted in us knowing more than before, less than before or the same as before.
#option 1 which creates 4 graphs side by side 

# changing advancement from numeric to factor/ labeling values of advancement 
Exp_1_data$advancement_levels <- factor(Exp_1_data$advancement,
                                   levels = c(-1, 0, 1),
                                   labels = c("Less", "Same", "More")) 

#making the histogram 


advancement_levels<-ggplot(Exp_1_data) +
  geom_bar(aes(x=advancement_levels, 
               group=Conflict:Format, #grouping by condition
               fill=Conflict:Format), #colouring the bars based on condition 
           position="dodge") + # making it so the bars are next to each other instead of stacked 
  scale_x_discrete(name = "Advancement", labels=xAxisLabels)+ # labeling the x axis 
  scale_y_continuous(name = "Number of Participants") +
  scale_fill_grey(name = "Condition",                     # "scale_*_grey" creates a grey colour gradient and by inserting 'fill' it applies it to the condition 
                  labels = c("Conflicting/Generic",        #labeling legend
                             "Conflicting/Qualified", 
                             "Non-conflicting/Generic", 
                             "Non-conflicting/Qualified")) 


print(advancement_levels) #print 


