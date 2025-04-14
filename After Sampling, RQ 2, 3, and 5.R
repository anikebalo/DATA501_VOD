piracy.dataset <- read.csv("PiracySVOD_Clean.csv", stringsAsFactors = FALSE)
library(ggplot2)
library(dplyr)

balanced.dataset <- piracy.dataset %>%
  group_by(Education, Age) %>%
  sample_n(size = min(table(piracy.dataset$Age, piracy.dataset$Education)), replace = FALSE)

#---------------------------------------------------------------------------------------
# RQ2: Education & Piracy
#---------------------------------------------------------------------------------------

balanced.dataset$Education <- factor(balanced.dataset$Education,
                                   levels = c(1, 2, 3),
                                   labels = c("Primary", "Secondary", "Tertiary"))
balanced.dataset$Piracy_use <- factor(balanced.dataset$Piracy_use,
                                    levels = c(0, 1),
                                    labels = c("No", "Yes"))

# Education Distribution of Those Who Pirate
ggplot(balanced.dataset[balanced.dataset$Piracy_use == "Yes", ], aes (x = Education, fill = Education)) + 
  geom_bar(col = "grey", na.rm = TRUE, width = 0.7) + geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Primary" = "pink", "Secondary" = "purple", "Tertiary" = "lightblue")) + 
  xlab("Education Level") + ylab("Count") + ggtitle("Education Distribution of Those Who Pirate") + theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

edu.pirate.count <- table(balanced.dataset$Education[balanced.dataset$Piracy_use == "Yes"])
edu.pirate.prop <- edu.pirate.count / sum(edu.pirate.count)
round(edu.pirate.prop, digits = 2)

# Education Distribution of Those Who Do Not Pirate
ggplot(balanced.dataset[balanced.dataset$Piracy_use == "No", ], aes (x = Education, fill = Education)) + 
  geom_bar(col = "grey", na.rm = TRUE, width = 0.7) + geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Primary" = "pink", "Secondary" = "purple", "Tertiary" = "lightblue")) + 
  xlab("Education Level") + ylab("Count") + ggtitle("Education Distribution of Those Who Do Not Pirate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

edu.non.pirate.count <- table(balanced.dataset$Education[balanced.dataset$Piracy_use == "No"])
edu.non.pirate.prop <- edu.non.pirate.count / sum(edu.non.pirate.count)
round(edu.non.pirate.prop, digits = 2)

# Education Distribution of All Users
ggplot(balanced.dataset, aes (x = Education, fill = Education)) + geom_bar(col = "grey", na.rm = TRUE, width = 0.7) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Primary" = "pink", "Secondary" = "purple", "Tertiary" = "lightblue")) + 
  xlab("Education Level") + ylab("Count") + ggtitle("Education Distribution of All Users") + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

education.count <- table(balanced.dataset$Education)
education.prop <- education.count / sum(education.count)
round(education.prop, digits = 2)
# 33% of users are allocated to each education level due to stratified sampling

# Chi-Squared Test of Independence
edu.piracy.table <- table(balanced.dataset$Education, balanced.dataset$Piracy_use)
chisq.test(edu.piracy.table)

#---------------------------------------------------------------------------------------
# RQ2: Age & Piracy
#---------------------------------------------------------------------------------------

balanced.dataset$Age <- factor(balanced.dataset$Age,
                                   levels = c(1, 2, 3, 4),
                                   labels = c("16-30", "31-40", "41-50", "51+"))

# Age Distribution of Those Who Pirate
ggplot(balanced.dataset[balanced.dataset$Piracy_use == "Yes", ], aes(x = Age, fill = Age)) + 
  geom_bar(col = "grey", na.rm = TRUE, width = 0.7) + geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("16-30" = "lightgreen", "31-40" = "pink", "41-50" = "lightblue", "51+" = "purple")) + 
  xlab("Age") + ylab("Count") + ggtitle("Age Distribution of Those Who Pirate") + theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none") 

age.pirate.count <- table(balanced.dataset$Age[balanced.dataset$Piracy_use == "Yes"])
age.pirate.prop <- age.pirate.count / sum(age.pirate.count)
round(age.pirate.prop, digits = 2)

# Age Distribution of Those Who Do Not Pirate
ggplot(balanced.dataset[balanced.dataset$Piracy_use == "No", ], aes (x = Age, fill = Age)) + 
  geom_bar(col = "grey", na.rm = TRUE, width = 0.7) + geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("16-30" = "lightgreen", "31-40" = "pink", "41-50" = "lightblue", "51+" = "purple")) +
  xlab("Age") + ylab("Count") + ggtitle("Age Distribution of Those Who Do Not Pirate") + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

age.non.pirate.count <- table(balanced.dataset$Age[balanced.dataset$Piracy_use == "No"])
age.non.pirate.prop <- age.non.pirate.count / sum(age.non.pirate.count)
round(age.non.pirate.prop, digits = 2)

# Age Distribution of All Users
ggplot(balanced.dataset, aes (x = Age, fill = Age)) + geom_bar(col = "grey", na.rm = TRUE, width = 0.7) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("16-30" = "lightgreen", "31-40" = "pink", "41-50" = "lightblue", "51+" = "purple")) +
  xlab("Age") + ylab("Count") + ggtitle("Age Distribution of All Users") + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

age.count <- table(balanced.dataset$Age)
age.prop <- age.count / sum(age.count)
round(age.prop, digits = 2)

# Chi-Squared Test of Independence
age.piracy.table <- table(balanced.dataset$Age, balanced.dataset$Piracy_use)
chisq.test(age.piracy.table)

#---------------------------------------------------------------------------------------
# RQ3: Education & Justice Sensibility
#---------------------------------------------------------------------------------------

# Checking normality through visualization: looks somewhat normally distributed
ggplot(balanced.dataset, aes(x = Average_Justice_Sensibility)) + geom_histogram(aes(y = ..density..), 
                                                                                bins = 10, fill = "skyblue", color = "black", alpha = 0.7) + geom_density(color = "red", size = 1)

# The p-value <= 0.05, therefore the data is not normally distributed
shapiro.test(balanced.dataset$Average_Justice_Sensibility)

# Since Justice Sensibility is not normal, we would used the Kruskal Test which yields a p-value > 0.05
# meaning that there is no possible dependence
kruskal.test(Average_Justice_Sensibility ~ Education, data = balanced.dataset)

tapply(balanced.dataset$Average_Justice_Sensibility, balanced.dataset$Education, mean)

# Box Plot
ggplot(balanced.dataset, aes(x = factor(Education), y = Average_Justice_Sensibility, fill = Education)) + 
  geom_boxplot(outlier.color = "red") + xlab("Education Level") + ylab("Average Justice Sensibility Rating") +
  scale_fill_manual(values = c("Primary" = "pink", "Secondary" = "purple", "Tertiary" = "lightblue")) +
  ggtitle("Average Justice Sensibility Rating vs. Education Level") + coord_flip() + 
  scale_x_discrete(labels = c("1" = "Primary", "2" = "Secondary", "3" = "Tertiary")) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

summary(balanced.dataset$Average_Justice_Sensibility)

#---------------------------------------------------------------------------------------
# RQ3: Education & Fear of Punishment
#---------------------------------------------------------------------------------------

# Checking normality through visualization: looks not normal
ggplot(balanced.dataset, aes(x = Average_Fear_punishment)) + geom_histogram(aes(y = ..density..), 
                                                                            bins = 10, fill = "skyblue", color = "black", alpha = 0.7) + geom_density(color = "red", size = 1)

# The p-value <= 0.05, therefore the data is not normally distributed
shapiro.test(balanced.dataset$Average_Fear_punishment)

# Since Fear of Punishment is not normal, we would used the Kruskal Test which yields a p-value > 0.05
# meaning that there is no possible dependence
kruskal.test(Average_Fear_punishment ~ Education, data = balanced.dataset)

tapply(balanced.dataset$Average_Fear_punishment, balanced.dataset$Education, mean)

# Box Plot
ggplot(balanced.dataset, aes(x = factor(Education), y = Average_Fear_punishment, fill = Education)) + 
  geom_boxplot(outlier.color = "red") + xlab("Education Level") + ylab("Average Fear of Punishment Rating") +
  scale_fill_manual(values = c("Primary" = "pink", "Secondary" = "purple", "Tertiary" = "lightblue")) +
  ggtitle("Average Justice Sensibility Rating vs. Education Level") + coord_flip() + 
  scale_x_discrete(labels = c("1" = "Primary", "2" = "Secondary", "3" = "Tertiary")) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

summary(balanced.dataset$Average_Fear_punishment)

#---------------------------------------------------------------------------------------
# RQ5: Education & Message Credibility
#---------------------------------------------------------------------------------------

# Pro-social Message
prosocial.recieved <- balanced.dataset %>%
  filter(Prosocial_message == 1)

ggplot(prosocial.recieved, aes(x = factor(Education), y = Prosocial_credibility, fill = Education)) + geom_boxplot(outlier.color = "red") +
  xlab("Education Level") + ylab("Credibility of Prosocial Message") + ggtitle("Prosocial Credibility vs. Education Level") +
  scale_fill_manual(values = c("Primary" = "pink", "Secondary" = "purple", "Tertiary" = "lightblue")) +
  coord_flip() + scale_x_discrete(labels = c("1" = "Primary", "2" = "Secondary", "3" = "Tertiary")) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

summary(prosocial.recieved$Prosocial_credibility)

# Threatening Message
threatening.recieved <- balanced.dataset %>%
  filter(Threatening_message == 1)

ggplot(threatening.recieved, aes(x = factor(Education), y = Threatening_credibility, fill = Education)) + 
  geom_boxplot(outlier.color = "red") + xlab("Education Level") + ylab("Credibility of Threatening Message") +
  scale_fill_manual(values = c("Primary" = "pink", "Secondary" = "purple", "Tertiary" = "lightblue")) +
  ggtitle("Threatening Credibility vs. Education Level") + coord_flip() + 
  scale_x_discrete(labels = c("1" = "Primary", "2" = "Secondary", "3" = "Tertiary")) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), legend.position = "none")

summary(threatening.recieved$Threatening_credibility)

