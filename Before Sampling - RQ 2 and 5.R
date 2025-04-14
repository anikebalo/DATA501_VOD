piracy.dataset <- read.csv("PiracySVOD_Clean.csv", stringsAsFactors = FALSE)
library(ggplot2)
library(dplyr)

#---------------------------------------------------------------------------------------
# RQ2: Education & Piracy
#---------------------------------------------------------------------------------------

piracy.dataset$Education <- factor(piracy.dataset$Education,
                                     levels = c(1, 2, 3),
                                     labels = c("Primary", "Secondary", "Tertiary"))
piracy.dataset$Piracy_use <- factor(piracy.dataset$Piracy_use,
                                      levels = c(0, 1),
                                      labels = c("No", "Yes"))

# Education Distribution of Those Who Pirate
ggplot(piracy.dataset[piracy.dataset$Piracy_use == "Yes", ], aes (x = Education, fill = Education)) + 
  geom_bar(col = "grey", na.rm = TRUE, width = 0.7) + geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Primary" = "pink", "Secondary" = "purple", "Tertiary" = "lightblue")) + 
  xlab("Education Level") + ylab("Count") + ggtitle("Education Distribution of Those Who Pirate") + theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

edu.pirate.count <- table(piracy.dataset$Education[piracy.dataset$Piracy_use == "Yes"])
edu.pirate.prop <- edu.pirate.count / sum(edu.pirate.count)
round(edu.pirate.prop, digits = 2)
# Out of all users who pirate, 49% have a tertiary education level, 41% have a secondary education level, and 10%
# have a primary education level.

# Education Distribution of Those Who Do Not Pirate
ggplot(piracy.dataset[piracy.dataset$Piracy_use == "No", ], aes (x = Education, fill = Education)) + 
  geom_bar(col = "grey", na.rm = TRUE, width = 0.7) + geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Primary" = "pink", "Secondary" = "purple", "Tertiary" = "lightblue")) + 
  xlab("Education Level") + ylab("Count") + ggtitle("Education Distribution of Those Who Do Not Pirate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

edu.non.pirate.count <- table(piracy.dataset$Education[piracy.dataset$Piracy_use == "No"])
edu.non.pirate.prop <- edu.non.pirate.count / sum(edu.non.pirate.count)
round(edu.non.pirate.prop, digits = 2)
# Out of all users who do not pirate, 51% have a tertiary education level, 37% have a secondary education level
# , and 11% have a primary education level.

# Education Distribution of All Users
ggplot(piracy.dataset, aes (x = Education, fill = Education)) + geom_bar(col = "grey", na.rm = TRUE, width = 0.7) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Primary" = "pink", "Secondary" = "purple", "Tertiary" = "lightblue")) + 
  xlab("Education Level") + ylab("Count") + ggtitle("Education Distribution of All Users") + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

# Insights
table(piracy.dataset$Education)
# Majority of users have a tertiary education (444 users). Secondary education is the next highest
# education level among users (346 users). Finally, primary education is the least common education level (93 users).
# Relationship: majority of users have tertiary education.

education.count <- table(piracy.dataset$Education)
education.prop <- education.count / sum(education.count)
round(education.prop, digits = 2)
# Out of all users, 50% have a tertiary education level, 39% have a secondary education level, and 11% have a 
# primary education level.

# Chi-Squared Test of Independence
edu.piracy.table <- table(piracy.dataset$Education, piracy.dataset$Piracy_use)
chisq.test(edu.piracy.table)

# Insights
# The chi-square test between education and use of piracy results in a p-value of 0.4745. Since this value is
# greater than 0.05, we can conclude there is NO significant dependence between education level and piracy use.

#---------------------------------------------------------------------------------------
# RQ2: Age & Piracy
#---------------------------------------------------------------------------------------

piracy.dataset$Age <- factor(piracy.dataset$Age,
                               levels = c(1, 2, 3, 4),
                               labels = c("16-30", "31-40", "41-50", "51+"))

# Age Distribution of Those Who Pirate
ggplot(piracy.dataset[piracy.dataset$Piracy_use == "Yes", ], aes(x = Age, fill = Age)) + 
  geom_bar(col = "grey", na.rm = TRUE, width = 0.7) + geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("16-30" = "lightgreen", "31-40" = "pink", "41-50" = "lightblue", "51+" = "purple")) + 
  xlab("Age") + ylab("Count") + ggtitle("Age Distribution of Those Who Pirate") + theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none") .

age.pirate.count <- table(piracy.dataset$Age[piracy.dataset$Piracy_use == "Yes"])
age.pirate.prop <- age.pirate.count / sum(age.pirate.count)
round(age.pirate.prop, digits = 2)
# Out of all users who pirate, 41% are between 16-30, 23% between 41-50, 19% between 31-40, and 17% are 51+.

# Age Distribution of Those Who Do Not Pirate
ggplot(piracy.dataset[piracy.dataset$Piracy_use == "No", ], aes (x = Age, fill = Age)) + 
  geom_bar(col = "grey", na.rm = TRUE, width = 0.7) + geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("16-30" = "lightgreen", "31-40" = "pink", "41-50" = "lightblue", "51+" = "purple")) +
  xlab("Age") + ylab("Count") + ggtitle("Age Distribution of Those Who Do Not Pirate") + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

age.non.pirate.count <- table(piracy.dataset$Age[piracy.dataset$Piracy_use == "No"])
age.non.pirate.prop <- age.non.pirate.count / sum(age.non.pirate.count)
round(age.non.pirate.prop, digits = 2)
# Out of all users who do not pirate, 34% are 51+, 29% between 41-50, 20% between 16-30, and 18% between 31-40.

# Age Distribution of All Users
ggplot(piracy.dataset, aes (x = Age, fill = Age)) + geom_bar(col = "grey", na.rm = TRUE, width = 0.7) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("16-30" = "lightgreen", "31-40" = "pink", "41-50" = "lightblue", "51+" = "purple")) +
  xlab("Age") + ylab("Count") + ggtitle("Age Distribution of All Users") + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

age.count <- table(piracy.dataset$Age)
age.prop <- age.count / sum(age.count)
round(age.prop, digits = 2)
# Out of all users, 31% are between the ages of 16-30, 26% between 41-50, 25% 51+, and 18% 31-40.

# Chi-Sqaured Test of Independence
age.piracy.table <- table(piracy.dataset$Age, piracy.dataset$Piracy_use)
chisq.test(age.piracy.table)
# Insights
# The chi-square test between age and use of piracy results in a p-value of 0.0000. Since this value is
# less than 0.05, we can conclude there is a significant dependence between education level and piracy use.

#---------------------------------------------------------------------------------------
# RQ5: Education & Message Credibility
#---------------------------------------------------------------------------------------

# Pro-social Message
prosocial.recieved <- piracy.dataset %>%
  filter(Prosocial_message == 1)

ggplot(prosocial.recieved, aes(x = factor(Education), y = Prosocial_credibility, fill = Education)) + geom_boxplot(outlier.color = "red") +
  xlab("Education Level") + ylab("Credibility of Prosocial Message") + ggtitle("Prosocial Credibility vs. Education Level") +
  scale_fill_manual(values = c("Primary" = "pink", "Secondary" = "purple", "Tertiary" = "lightblue")) +
  coord_flip() + scale_x_discrete(labels = c("1" = "Primary", "2" = "Secondary", "3" = "Tertiary")) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

# Insights
summary(prosocial.recieved$Prosocial_credibility)
# 25% of people rated the message a 3 for credibility, 50% of people rated credibility less than 4 and 50% rated it
# more than 4, 3.498 was the average credibility rating chosen by users, and 70% of users rated it a 4.
# Relationship: most users rated the credibility of the pro-social message at around 3.5 (higher than average).
# However, the education level does not seem to effect the pro-social credibility rating. There is symmetrical distribution.

summary(prosocial.recieved$Prosocial_credibility[prosocial.recieved$Education == "1"])
# Users with primary education only, chose an average credibility rating of 3.542. 50% of users rated credibility
# less than 4 while the other 50% rated it more than 4.

summary(prosocial.recieved$Prosocial_credibility[prosocial.recieved$Education == "2"])
# Users with secondary education only, chose an average credibility rating of 3.531. 50% of users rated credibility
# less than 4 while the other 50% rated it more than 4.

summary(prosocial.recieved$Prosocial_credibility[prosocial.recieved$Education == "3"])
# Users with tertiary education only, chose an average credibility rating of 3.465. 50% of users rated credibility
# less than 4 while the other 50% rated it more than 4.

# Threatening Message
threatening.recieved <- piracy.dataset %>%
  filter(Threatening_message == 1)

ggplot(threatening.recieved, aes(x = factor(Education), y = Threatening_credibility, fill = Education)) + 
  geom_boxplot(outlier.color = "red") + xlab("Education Level") + ylab("Credibility of Threatening Message") +
  scale_fill_manual(values = c("Primary" = "pink", "Secondary" = "purple", "Tertiary" = "lightblue")) +
  ggtitle("Threatening Credibility vs. Education Level") + coord_flip() + 
  scale_x_discrete(labels = c("1" = "Primary", "2" = "Secondary", "3" = "Tertiary")) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

# Insights
summary(threatening.recieved$Threatening_credibility)
# 25% of people rated the message a 2 for credibility, 50% of people rated credibility less than 3 and 50% rated it
# more than 3, 3.103 was the average credibility rating chosen by users, and 70% of users rated it a 4.
# Relationship: most users rated the credibility of the pro-social message at around 3 (somewhat higher than average).
# However, the education level does not effect the threatening credibility rating. There is symmetrical distribution.

summary(threatening.recieved$Threatening_credibility[threatening.recieved$Education == "Primary"])
# Users with primary education only, chose an average credibility rating of 3.13. 50% of users rated credibility
# less than 3 while the other 50% rated it more than 3.

summary(threatening.recieved$Threatening_credibility[threatening.recieved$Education == "Secondary"])
# Users with secondary education only, chose an average credibility rating of 3.07. 50% of users rated credibility
# less than 3 while the other 50% rated it more than 3.

summary(threatening.recieved$Threatening_credibility[threatening.recieved$Education == "Tertiary"])
# Users with tertiary education only, chose an average credibility rating of 3.123. 50% of users rated credibility
# less than 3 while the other 50% rated it more than 3.


