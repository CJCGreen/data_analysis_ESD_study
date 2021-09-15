# C Green 26th August 2021
# analyse_quiz2_data_clean.R
# R script for results in Chapter 6: Results of ESD Transfer Experiment
#######################################################################
#
# Quiz 2 data: Basic and inferential statistics and visualisations of qualitative data
#
# Note on dataframe names:
# quiz2: all participant data (total 106)
# quiz2_no_outlier: quiz2 minus the most extreme outlier (total 105)
# quiz2_no_outliers: quiz2 minus two outliers (total 104)

# Use xlsx package to import Excel
# install.packages("xlsx")
library(xlsx)

# quiz2 dataframe contains quiz2 data for all groups
quiz2 <- read.xlsx("RDataSources/scores_tidy_ORIGINAL_DO_NOT_DELETE.xlsx",sheetIndex=4)

# Create new ST and Sim properties, one for each factor (necessary for inferential statistical tests)
quiz2$ST <- as.factor(ifelse(quiz2$Group==1, 1, ifelse(quiz2$Group==3, 1, 0)))
quiz2$Sim <- as.factor(ifelse(quiz2$Group==2, 1, ifelse(quiz2$Group==3, 1, 0)))

# Rename the groups and order them as factors (improves graph and table presentation)
quiz2$Group <- ifelse(quiz2$Group==0, "Control", ifelse(quiz2$Group==1, "ST", ifelse(quiz2$Group==2, "Sim","ST+Sim"))) 
quiz2$Group <- factor(quiz2$Group, levels = c("Control", "ST", "Sim", "ST+Sim"))

# Summary figures for all participants:
nrow(quiz2)                       # 106
summary(quiz2$QuizScore)          # Min 38, max 97, mean 77.73, median 79.50
sort(table(quiz2$QuizScore))      # Mode 80
sd(quiz2$QuizScore)               # 10.58618

# Test normality of residuals of all quiz 2 scores
res <- residuals(aov(QuizScore ~ ST + Sim, quiz2))  
shapiro.test(res)
# Result:
# Shapiro-Wilk normality test
# data:  res
# W = 0.96019, p-value = 0.002921
# Result: Significant. Not normal.

# Histogram - all scores (INC outliers and zero-engagers)
hist(quiz2$QuizScore, main="Histogram: Quiz 2 Scores (All Participants)", ylim= c(0, 40),xlab="Percentage Score", col="bisque2")

# With density line
hist(quiz2$QuizScore, 
     main="Histogram: Quiz 2 Scores (All Participants)", 
     xlab="Percentage Score", 
     col="bisque2",
     ylim= c(0, 0.040),
     freq=FALSE)
lines(density(quiz2$QuizScore),col=2) 

###########################
# GROUP 0 (Control group) #
###########################

# Get Group0 data
quiz2group0 <- quiz2[quiz2$Group == "Control",]

# Summary figures for group 0:
nrow(quiz2group0)                       # 28
summary(quiz2group0$QuizScore)          # Min 38, max 92, mean 78.29, median 80.50
sort(table(quiz2group0$QuizScore))      # Modes 80, 83
sd(quiz2group0$QuizScore)               # 11.07502

# Histogram..
hist(quiz2group0$QuizScore, main="Histogram: Quiz 2 Group 0 Scores", xlab="Percentage Score", col="azure3")

# .. with density line
hist(quiz2group0$QuizScore, 
     main="Histogram: Quiz 2 Group 0 Scores", 
     xlab="Percentage Score", 
     col="azure3",
     freq=FALSE)
lines(density(quiz2group0$QuizScore),col=2) 

######################
# GROUP 1 (ST Group) #
######################

# Get Group1 data
quiz2group1 <- quiz2[quiz2$Group == "ST",]

# Summary figures for group 1:
nrow(quiz2group1)                       # 26
summary(quiz2group1$QuizScore)          # Min 55, max 97, mean 77.54, median 78.00
sort(table(quiz2group1$QuizScore))      # Modes 60 69 72 77 78 82
sd(quiz2group1$QuizScore)               # 11.22223

# Histogram..
hist(quiz2group1$QuizScore, main="Histogram: Quiz 2 Group 1 Scores", xlab="Percentage Score", col="bisque2")

# .. with density line
hist(quiz2group1$QuizScore, 
     main="Histogram: Quiz 2 Group 1 Scores", 
     xlab="Percentage Score", 
     col="bisque2",
     freq=FALSE)
lines(density(quiz2group1$QuizScore),col=2) 

#######################
# GROUP 2 (Sim group) #
#######################

# Get Group2 data
quiz2group2 <- quiz2[quiz2$Group == "Sim",]

# Summary figures for group 2:
nrow(quiz2group2)                       # 24
summary(quiz2group2$QuizScore)          # Min 55, max 95, mean 81.96, median 83.50
sort(table(quiz2group2$QuizScore))      # Mode 94
sd(quiz2group2$QuizScore)               # 9.932289

# Histogram..
hist(quiz2group2$QuizScore, main="Histogram: Quiz 2 Group 2 Scores", xlab="Percentage Score", col="bisque2")

# .. with density line
hist(quiz2group2$QuizScore, 
     main="Histogram: Quiz 2 Group 2 Scores", 
     xlab="Percentage Score", 
     col="bisque2",
     freq=FALSE)
lines(density(quiz2group2$QuizScore),col=2) 

############################
# GROUP 3 (ST + Sim group) #
############################

# Get Group3 data
quiz2group3 <- quiz2[quiz2$Group == "ST+Sim",]

# Summary figures for group 3:
nrow(quiz2group3)                       # 28
summary(quiz2group3$QuizScore)          # Min 54, max 89, mean 73.71, median 73.50
sort(table(quiz2group3$QuizScore))      # Mode 80
sd(quiz2group3$QuizScore)               # 8.918799

# Histogram..
hist(quiz2group3$QuizScore, main="Histogram: Quiz 2 Group 3 Scores", xlab="Percentage Score", col="bisque2")

# .. with density line
hist(quiz2group3$QuizScore, 
     main="Histogram: Quiz 2 Group 3 Scores", 
     xlab="Percentage Score", 
     col="bisque2",
     freq=FALSE)
lines(density(quiz2group3$QuizScore),col=2) 

###########################
# VISUALISE QUIZ 2 SCORES #
###########################

####################################
# ggplot dotplots and violin plots #
####################################

# install.packages("ggplot2")
library(ggplot2)

# Dot plot
ggplot(quiz2, aes(x=Group, y=QuizScore, fill=Group)) + 
    ggtitle("Dot Plot: Quiz 2 Scores by Group\n(Each group mean shown in red, median in navy)") +
    scale_fill_manual(values = c("azure3", "bisque2", "bisque2", "bisque2")) +
    labs(y="Quiz 2 Scores (%)", x = "Group") +
    geom_dotplot(binaxis='y', stackdir='center', stackratio=1.3, dotsize=1.2, position="dodge", binwidth=1) + 
    scale_y_continuous(breaks = seq(min(40), max(100), by = 10)) +
    theme(legend.position="none") +
    stat_summary(fun=mean, geom="point", shape=18, size=4, color="red") +
    stat_summary(fun=median, geom="point", shape=18, size=4, color="navy") +
    scale_x_discrete(labels=c("0" = "Control", "1" = "ST", "2" = "Sim", "3" = "ST + Sim"))

# This version combines dot plot with violin plot for each group
ggplot(quiz2, aes(x=Group, y=QuizScore, fill=Group)) + 
    ggtitle("Dot Plot and Violin Plot: Quiz 2 Scores by Group") +
    scale_fill_manual(values = c("azure3", "bisque2", "bisque2", "bisque2")) +  
    labs(y="Quiz 2 Scores (%)", x = "Group") +
    theme(legend.position="none") +
    geom_violin(trim = FALSE) +
    geom_dotplot(binaxis='y', stackdir='center', stackratio=1.3, dotsize=1.2, position="dodge", binwidth=1) +
    scale_y_continuous(breaks = seq(min(20), max(100), by = 10)) +
    scale_x_discrete(labels=c("0" = "Control", "1" = "ST", "2" = "Sim", "3" = "ST + Sim"))

############
# Boxplots #
############

# Side-by-side boxplots for all groups, and all participants
boxplot(quiz2$QuizScore,
        quiz2group0$QuizScore, 
        quiz2group1$QuizScore, 
        quiz2group2$QuizScore, 
        quiz2group3$QuizScore, 
        main="Boxplot: Quiz 2 Scores by Group", 
        ylab = "Score (%)",
        ylim = c(38, 100),
        names = c("All", "Control", "ST", "Sim", "ST+Sim"),
        col= c("aquamarine3", "azure3", "bisque2", "bisque2", "bisque2"))

####################################
# Density plots (ggplot and dplyr) #
####################################

# Density plot: a smooth density estimate
# Basic plot
b <- ggplot(quiz2, aes(x = QuizScore))

# Change line colours by Group, change colours manually and add titles
b + geom_density(aes(color = Group)) +
  scale_color_manual(values=c("#000000", "#388e3c", "#b71c1c","#1565c0" )) +
  ggtitle("Density Plot: Quiz 2 Scores by Group") +
  labs(x="Quiz 2 Scores (%)", y = "density")+
  theme_classic()

# Add mean line
# install.packages(plyr)
library(plyr)
means <- ddply(quiz2, "Group", summarise, grp.mean=mean(QuizScore))
b + geom_density(aes(color = Group)) +
  geom_vline(data=means, aes(xintercept=grp.mean, color=Group),
             linetype="dashed") +
  scale_color_manual(values=c("#000000", "#388e3c", "#b71c1c","#1565c0" )) +
  ggtitle("Density Plot: Quiz 2 Scores by Group, with Group Means") +
  labs(x="Quiz 2 Scores (%)", y = "density")+
  theme_classic()

#########################
# ENGAGEMENT FOR QUIZ 2 #
#########################

# Engagement for ALL participants for Quiz 2
mean(quiz2$Engagement)
# Result 1.820755

# Crosstab: Engagement frequencies
quiz2_engagement_table <- table(quiz2$Engagement)
quiz2_engagement_table
# Result
# 0  1  2 
# 2 15 89

# Crosstab: Engagement level by group
quiz2_engagement_by_group <- table(quiz2$Group, quiz2$Engagement)
quiz2_engagement_by_group
# Result
#          0  1  2
# Control  1  5 22
# ST       0  3 23
# Sim      1  6 17
# ST+Sim   0  1 27
# One non-engager in Control, one in Sim

# Remove the two zero engagers
quiz2_engaged <- quiz2[quiz2$Engagement > 0,]

# Summary statistics for quiz 2, after removal of zero-engagers

# Get Group data excluding two zero engagers (one in Control group, one in Sim group)
quiz2_engaged_gp0 <- quiz2_engaged[quiz2_engaged$Group == "Control",]
quiz2_engaged_gp1 <- quiz2_engaged[quiz2_engaged$Group == "ST",]
quiz2_engaged_gp2 <- quiz2_engaged[quiz2_engaged$Group == "Sim",]
quiz2_engaged_gp3 <- quiz2_engaged[quiz2_engaged$Group == "ST+Sim",]

# Recalculate the summary statistics for all remaining scores, and for each group
nrow(quiz2_engaged)                       # 104
summary(quiz2_engaged$QuizScore)          # Min 38, max 97, mean 77.92, median 79.50
sort(table(quiz2_engaged$QuizScore))      # Modes 80 81
sd(quiz2_engaged$QuizScore)               # 10.44699

nrow(quiz2_engaged_gp0)                   # 27
summary(quiz2_engaged_gp0$QuizScore)      # Min 38, max 92, mean 78.22, median 81
sort(table(quiz2_engaged_gp0$QuizScore))  # Mode 83
sd(quiz2_engaged_gp0$QuizScore)           # 11.2808

nrow(quiz2_engaged_gp1)                   # 26
summary(quiz2_engaged_gp1$QuizScore)      # Min 55, max 97, mean 77.54, median 78
sort(table(quiz2_engaged_gp1$QuizScore))  # Modes 60 69 72 77 78 82 
sd(quiz2_engaged_gp1$QuizScore)           # 11.22223

nrow(quiz2_engaged_gp2)                   # 23
summary(quiz2_engaged_gp2$QuizScore)      # Min 67, max 95, mean 83.13, median 84
sort(table(quiz2_engaged_gp2$QuizScore))  # Mode 94 
sd(quiz2_engaged_gp2$QuizScore)           # 8.286376

nrow(quiz2_engaged_gp3)                   # 28
summary(quiz2_engaged_gp3$QuizScore)      # Min 54, max 89, mean 73.71, median 73.50
sort(table(quiz2_engaged_gp3$QuizScore))  # Mode 80
sd(quiz2_engaged_gp3$QuizScore)           # 8.918799

# Side-by-side boxplots for all groups, excluding zero-engagers
boxplot(quiz2_engaged$QuizScore,
        quiz2_engaged_gp0$QuizScore, 
        quiz2_engaged_gp1$QuizScore, 
        quiz2_engaged_gp2$QuizScore, 
        quiz2_engaged_gp3$QuizScore, 
        main="Quiz 2 Scores by Group excluding Unengaged", 
        names = c("All", "Control", "ST", "Sim", "ST+Sim"),
        ylab = "Score (%)", 
        col= c("aquamarine3", "azure3", "bisque2", "bisque2", "bisque2"))

############
# OUTLIERS #
############

########################
# Remove both outliers #
########################

# Two outliers had scores of 38 and 59 and both were in the Control group
# Remove two outliers as well as the two zero engagers
quiz2_engaged_no_outliers <- quiz2_engaged
quiz2_engaged_no_outliers = quiz2_engaged_no_outliers[quiz2_engaged_no_outliers$ParticipantID != 97,]
quiz2_engaged_no_outliers = quiz2_engaged_no_outliers[quiz2_engaged_no_outliers$ParticipantID != 36,]

# Get Group data excluding zero engagers and outliers
quiz2_engaged_no_outliers_gp0 <- quiz2_engaged_no_outliers[quiz2_engaged_no_outliers$Group == "Control",]
quiz2_engaged_no_outliers_gp1 <- quiz2_engaged_no_outliers[quiz2_engaged_no_outliers$Group == "ST",]
quiz2_engaged_no_outliers_gp2 <- quiz2_engaged_no_outliers[quiz2_engaged_no_outliers$Group == "Sim",]
quiz2_engaged_no_outliers_gp3 <- quiz2_engaged_no_outliers[quiz2_engaged_no_outliers$Group == "ST+Sim",]

# Recalculate the summary statistics for all remaining scores, and for each group
nrow(quiz2_engaged_no_outliers)                       # 102
summary(quiz2_engaged_no_outliers$QuizScore)          # Min 54, max 97, mean 78.50, median 80.00
sort(table(quiz2_engaged_no_outliers$QuizScore))      # Modes 80 81
sd(quiz2_engaged_no_outliers$QuizScore)               # 9.572806

nrow(quiz2_engaged_no_outliers_gp0)                   # 25
summary(quiz2_engaged_no_outliers_gp0$QuizScore)      # Min 64, max 92, mean 80.6, median 81
sort(table(quiz2_engaged_no_outliers_gp0$QuizScore))  # Mode 83
sd(quiz2_engaged_no_outliers_gp0$QuizScore)           # 7.011895

nrow(quiz2_engaged_no_outliers_gp1)                   # 26
summary(quiz2_engaged_no_outliers_gp1$QuizScore)      # Min 55, max 97, mean 77.54, median 78
sort(table(quiz2_engaged_no_outliers_gp1$QuizScore))  # Modes 60 69 72 77 78 82 
sd(quiz2_engaged_no_outliers_gp1$QuizScore)           # 11.22223

nrow(quiz2_engaged_no_outliers_gp2)                   # 23
summary(quiz2_engaged_no_outliers_gp2$QuizScore)      # Min 67, max 95, mean 83.13, median 84
sort(table(quiz2_engaged_no_outliers_gp2$QuizScore))  # Mode 94 
sd(quiz2_engaged_no_outliers_gp2$QuizScore)           # 8.286376

nrow(quiz2_engaged_no_outliers_gp3)                   # 28
summary(quiz2_engaged_no_outliers_gp3$QuizScore)      # Min 54, max 89, mean 73.71, median 73.50
sort(table(quiz2_engaged_no_outliers_gp3$QuizScore))  # Mode 80 
sd(quiz2_engaged_no_outliers_gp3$QuizScore)           # 8.918799

# Side-by-side boxplots for all group - excluding zero engagers and two outliers
boxplot(quiz2_engaged_no_outliers$QuizScore,
        quiz2_engaged_no_outliers_gp0$QuizScore, 
        quiz2_engaged_no_outliers_gp1$QuizScore, 
        quiz2_engaged_no_outliers_gp2$QuizScore, 
        quiz2_engaged_no_outliers_gp3$QuizScore, 
        main="Quiz 2 Scores by Group excluding Unengaged and Outliers", 
        names = c("All", "Control", "ST", "Sim", "ST+Sim"),
        ylab = "Score (%)", 
        col= c("aquamarine3", "azure3", "bisque2", "bisque2", "bisque2"))

########################################
# Remove only the most extreme outlier #
########################################

quiz2_engaged_no_outlier <- quiz2_engaged
quiz2_engaged_no_outlier = quiz2_engaged_no_outlier[quiz2_engaged_no_outlier$ParticipantID != 97,]

# Get Group data minus two zero-engagers and one outlier
quiz2_engaged_no_outlier_gp0 <- quiz2_engaged_no_outlier[quiz2_engaged_no_outlier$Group == "Control",]
quiz2_engaged_no_outlier_gp1 <- quiz2_engaged_no_outlier[quiz2_engaged_no_outlier$Group == "ST",]
quiz2_engaged_no_outlier_gp2 <- quiz2_engaged_no_outlier[quiz2_engaged_no_outlier$Group == "Sim",]
quiz2_engaged_no_outlier_gp3 <- quiz2_engaged_no_outlier[quiz2_engaged_no_outlier$Group == "ST+Sim",]

# Recalculate the summary statistics for all remaining scores, and for each group
nrow(quiz2_engaged_no_outlier)                       # 103
summary(quiz2_engaged_no_outlier$QuizScore)          # Min 54, max 97, mean 78.31, median 80.00
sort(table(quiz2_engaged_no_outlier$QuizScore))      # Modes 80 81
sd(quiz2_engaged_no_outlier$QuizScore)               # 9.71761

nrow(quiz2_engaged_no_outlier_gp0)                   # 26
summary(quiz2_engaged_no_outlier_gp0$QuizScore)      # Min 59, max 92, mean 79.77, median 81
sort(table(quiz2_engaged_no_outlier_gp0$QuizScore))  # Mode 83
sd(quiz2_engaged_no_outlier_gp0$QuizScore)           # 8.071221

nrow(quiz2_engaged_no_outlier_gp1)                   # 26
summary(quiz2_engaged_no_outlier_gp1$QuizScore)      # Min 55, max 97, mean 77.54, median 78
sort(table(quiz2_engaged_no_outlier_gp1$QuizScore))  # Modes 69 72 77 78 82 
sd(quiz2_engaged_no_outlier_gp1$QuizScore)           # 11.22223

nrow(quiz2_engaged_no_outlier_gp2)                   # 23
summary(quiz2_engaged_no_outlier_gp2$QuizScore)      # Min 67, max 95, mean 83.13, median 84
sort(table(quiz2_engaged_no_outlier_gp2$QuizScore))  # Mode 94 
sd(quiz2_engaged_no_outlier_gp2$QuizScore)           # 8.286376

nrow(quiz2_engaged_no_outlier_gp3)                   # 28
summary(quiz2_engaged_no_outlier_gp3$QuizScore)      # Min 54, max 89, mean 73.71, median 73.50
sort(table(quiz2_engaged_no_outlier_gp3$QuizScore))  # Mode 80
sd(quiz2_engaged_no_outlier_gp3$QuizScore)           # 8.918799

# Histogram - all scores (EXC outlier and zero-engagers)
hist(quiz2_engaged_no_outlier$QuizScore, 
     main="Histogram: Quiz 2 All Scores \n(after outlier and 2 zero-engagers removed)", 
     xlab="Percentage Score", 
     col="azure3")

# Histogram - as above, displaying probability density on the y axis and an overlayed fitted density line:
hist(quiz2_engaged_no_outlier$QuizScore, 
     main="Histogram: Quiz 2 All Scores \n(after outlier and 2 zero-engagers removed)", 
     xlab="Percentage Score",
     ylim=c(0, 0.04),
     col="azure3",
     freq=FALSE)
# Add a density curve
lines(density(quiz2_engaged$QuizScore),col=2)

###############################################################
# COMPARING AVERAGE GROUP SCORES FOR EACH QUESTION IN QUIZ 2 #
###############################################################
# Note: Two zero-engagers and one outlier excluded

# How did the groups compare in their scores for Qn 1 (BOAT EXP GROWTH CALC - 1 YEAR)
barplot(c(mean(quiz2_engaged_no_outlier_gp0$Q1), mean(quiz2_engaged_no_outlier_gp1$Q1), 
          mean(quiz2_engaged_no_outlier_gp2$Q1), mean(quiz2_engaged_no_outlier_gp3$Q1)),
        main="Mean Qn 1 Scores by Group", 
        xlab="Group", ylab="Qn 1 Score", 
        ylim = c(0, 5),
        names.arg=c("Group 0", "Group 1", "Group 2", "Group 3"))
# ALL GROUPS BETTER THAN CONTROL GROUP, 1 AND 2 BEST

# Qn 2 (FISH EXP GROWTH CALC - 2 YEARS)
barplot(c(mean(quiz2_engaged_no_outlier_gp0$Q2), mean(quiz2_engaged_no_outlier_gp1$Q2), 
          mean(quiz2_engaged_no_outlier_gp2$Q2), mean(quiz2_engaged_no_outlier_gp3$Q2)),
        main="Mean Qn 2 Scores by Group", 
        xlab="Group", ylab="Qn 2 Score", 
        ylim = c(0, 5),
        names.arg=c("Group 0", "Group 1", "Group 2", "Group 3"))
# GROUP 2 BEST, GROUP 1 2ND, GROUP 3 WORSE THAN CONTROL GROUP

# Qn 3 (MAX YIELD DIV BOATS)
barplot(c(mean(quiz2_engaged_no_outlier_gp0$Q3), mean(quiz2_engaged_no_outlier_gp1$Q3), 
          mean(quiz2_engaged_no_outlier_gp2$Q3), mean(quiz2_engaged_no_outlier_gp3$Q3)),
        main="Mean Qn 3 Scores by Group", 
        xlab="Group", ylab="Qn 3 Score", 
        ylim = c(0, 4),
        names.arg=c("Group 0", "Group 1", "Group 2", "Group 3"))
# X # GROUP 2 MARGINALLY BETTER THAN CONTROL GROUP, GROUP 3 WORSE, GROUP 1 SAME

# Qn 4 (YEARS TO MAX YIELD)
barplot(c(mean(quiz2_engaged_no_outlier_gp0$Q4), mean(quiz2_engaged_no_outlier_gp1$Q4), 
          mean(quiz2_engaged_no_outlier_gp2$Q4), mean(quiz2_engaged_no_outlier_gp3$Q4)),
        main="Mean Qn 4 Scores by Group", 
        xlab="Group", ylab="Qn 4 Score", 
        ylim = c(0, 7),
        names.arg=c("Group 0", "Group 1", "Group 2", "Group 3"))
# ONLY GROUP 2 BETTER THAN CONTROL GROUP

# Qn 5 (MSY - SUSTAINABLE LIMITS)
barplot(c(mean(quiz2_engaged_no_outlier_gp0$Q5), mean(quiz2_engaged_no_outlier_gp1$Q5), 
          mean(quiz2_engaged_no_outlier_gp2$Q5), mean(quiz2_engaged_no_outlier_gp3$Q5)),
        main="Mean Qn 5 Scores by Group", 
        xlab="Group", ylab="Qn 5 Score", 
        ylim = c(0, 7),
        names.arg=c("Group 0", "Group 1", "Group 2", "Group 3"))
# ONLY GROUP 2 BETTER THAN CONTROL GROUP (AND IT DID MUCH BETTER)

# Qn 6 (CONSEQUENCES OF OVERREACHING SUSTAINABLE LIMITS)
barplot(c(mean(quiz2_engaged_no_outlier_gp0$Q6), mean(quiz2_engaged_no_outlier_gp1$Q6), 
          mean(quiz2_engaged_no_outlier_gp2$Q6), mean(quiz2_engaged_no_outlier_gp3$Q6)),
        main="Mean Qn 6 Scores by Group", 
        xlab="Group", ylab="Qn 6 Score", 
        ylim = c(0, 9),
        names.arg=c("Group 0", "Group 1", "Group 2", "Group 3"))
# X # NO GROUP BETTER THAN CONTROL GROUP

# Qn 7 (FISHERY SUSTAINABILITY DEFINITION)
barplot(c(mean(quiz2_engaged_no_outlier_gp0$Q7), mean(quiz2_engaged_no_outlier_gp1$Q7), 
          mean(quiz2_engaged_no_outlier_gp2$Q7), mean(quiz2_engaged_no_outlier_gp3$Q7)),
        main="Mean Qn 7 Scores by Group", 
        xlab="Group", ylab="Qn 7 Score", 
        ylim = c(0, 4),
        names.arg=c("Group 0", "Group 1", "Group 2", "Group 3"))
# ALL GROUPS BETTER THAN CONTROL GROUP, GROUP 2 BEST, BUT NOT BY MUCH

# Qn 8 (OVERFISHING)
barplot(c(mean(quiz2_engaged_no_outlier_gp0$Q8), mean(quiz2_engaged_no_outlier_gp1$Q8), 
          mean(quiz2_engaged_no_outlier_gp2$Q8), mean(quiz2_engaged_no_outlier_gp3$Q8)),
        main="Mean Qn 8 Scores by Group", 
        xlab="Group", ylab="Qn 8 Score", 
        ylim = c(0, 10),
        names.arg=c("Group 0", "Group 1", "Group 2", "Group 3"))
# X # NO GROUP BETTER THAN CONTROL GROUP

# Qn 9 (GRAPH OVERFISHING BEGINS)
barplot(c(mean(quiz2_engaged_no_outlier_gp0$Q9), mean(quiz2_engaged_no_outlier_gp1$Q9), 
          mean(quiz2_engaged_no_outlier_gp2$Q9), mean(quiz2_engaged_no_outlier_gp3$Q9)),
        main="Mean Qn 9 Scores by Group", 
        xlab="Group", ylab="Qn 9 Score", 
        ylim = c(0, 7),
        names.arg=c("Group 0", "Group 1", "Group 2", "Group 3"))
# X # NO GROUP BETTER THAN CONTROL GROUP

# Qn 10 (REASONS FOR COLLAPSE)
barplot(c(mean(quiz2_engaged_no_outlier_gp0$Q10), mean(quiz2_engaged_no_outlier_gp1$Q10), 
          mean(quiz2_engaged_no_outlier_gp2$Q10), mean(quiz2_engaged_no_outlier_gp3$Q10)),
        main="Mean Qn 10 Scores by Group", 
        xlab="Group", ylab="Qn 10 Score", 
        ylim = c(0, 12),
        names.arg=c("Group 0", "Group 1", "Group 2", "Group 3"))
# X # NO GROUP BETTER THAN CONTROL GROUP

# Qn 11 (SUSTAINABILITY POLICIES)
barplot(c(mean(quiz2_engaged_no_outlier_gp0$Q11), mean(quiz2_engaged_no_outlier_gp1$Q11), 
          mean(quiz2_engaged_no_outlier_gp2$Q11), mean(quiz2_engaged_no_outlier_gp3$Q11)),
        main="Mean Qn 11 Scores by Group", 
        xlab="Group", ylab="Qn 11 Score", 
        ylim = c(0, 12),
        names.arg=c("Group 0", "Group 1", "Group 2", "Group 3"))
# X # NO GROUP BETTER THAN CONTROL GROUP

# Qn 12 (SUSTAINABLE GRAPHS)
barplot(c(mean(quiz2_engaged_no_outlier_gp0$Q12), mean(quiz2_engaged_no_outlier_gp1$Q12), 
          mean(quiz2_engaged_no_outlier_gp2$Q12), mean(quiz2_engaged_no_outlier_gp3$Q12)),
        main="Mean Qn 12 Scores by Group", 
        xlab="Group", ylab="Qn 12 Score", 
        ylim = c(0, 10),
        names.arg=c("Group 0", "Group 1", "Group 2", "Group 3"))
# GROUP 2 BEST, GROUP 1 BARELY BETTER THAN CONTROL GROUP, GROUP 3 WORSE

##########################
# INFERENTIAL STATISTICS #
##########################

# Step numbers refer to the steps in the inferential analysis in the chapter

# Step 2: Check Factorial ANOVA assumptions

# 1 Test homogeneity of variances
library(car)
leveneTest(aov(QuizScore ~ ST * Sim, quiz2_engaged_no_outlier))
# Note: The leveneTest requires a saturated, or fully-crossed model, including the interactions. The formula uses the * operator.
# Result:
#    Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  3  1.3365 0.2669
#       99 

# 2 Test normality of residuals
res_quiz2 <- residuals(aov(QuizScore ~ ST + Sim, quiz2_engaged_no_outlier))  
shapiro.test(res_quiz2)
# Result:
# Shapiro-Wilk normality test
# data:  res_quiz2
# W = 0.98641, p-value = 0.3783

# Step 3: Run Factorial ANOVA overall test on all group scores
model_factors_quiz2 <- aov(QuizScore ~ ST * Sim, quiz2_engaged_no_outlier)
summary(model_factors_quiz2)

# Result:
#              Df Sum Sq Mean Sq F value Pr(>F)  
# ST           1    862   861.6  10.112 0.00197 **
# Sim          1      4     4.3   0.051 0.82229   
# ST:Sim       1    331   330.7   3.881 0.05162 . 
# Residuals   99   8435    85.2                
# ---
#    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Repeat with outlier back in
model_factors_quiz2_inc_outlier <- aov(QuizScore ~ ST * Sim, quiz2_engaged)
summary(model_factors_quiz2_inc_outlier)
# Result:
# Df Sum Sq Mean Sq F value Pr(>F)  
# ST            1    630   629.6   6.224 0.0142 *
# Sim           1      3     3.4   0.034 0.8546  
# ST:Sim        1    493   492.9   4.873 0.0296 *
# Residuals   100  10115   101.2                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Step 4: Interaction plot
interaction.plot(x.factor = quiz2_engaged_no_outlier$ST,
                 trace.factor = quiz2_engaged_no_outlier$Sim,
                 response = quiz2_engaged_no_outlier$QuizScore,
                 ylab="Quiz 2 Mean Scores",
                 xlab="Systems Thinking (ST)", 
                 main = "Interaction plot for ST and Simulation (Quiz 2)",
                 trace.label="Simulation (Sim)")

# Get the linear model (lm)
lm(model_factors_quiz2)
# Result:
# Call:
# lm(formula = model_factors_quiz2)

# Coefficients:
# (Intercept)          ST1         Sim1     ST1:Sim1  
#      79.769       -2.231        3.361       -7.185  

summary(lm(model_factors_quiz2))
# Result:
# ..
# Multiple R-squared:  0.1242,	Adjusted R-squared:  0.0977 
# F-statistic: 4.681 on 3 and 99 DF,  p-value: 0.004226

# Step 5: Remove ST+Sim group data
quiz2_engaged_no_outlier_no_gp3 <- quiz2_engaged_no_outlier[quiz2_engaged_no_outlier$Group!="ST+Sim",]
# Repeat, including the outlier:
quiz2_engaged_no_gp3 <- quiz2_engaged[quiz2_engaged$Group!="ST+Sim",]

# Step 6b: Check assumptions for one-way ANOVA test

# Test homogeneity of variances
leveneTest(aov(QuizScore ~ Group, quiz2_engaged_no_outlier_no_gp3))
# Result: 
#    Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  1.9316 0.1523
#       72    

# Test normality of residuals using R
res_quiz2_no_gp3 <- residuals(aov(QuizScore ~ Group, quiz2_engaged_no_outlier_no_gp3))
shapiro.test(res_quiz2_no_gp3)  
# Result:
# Shapiro-Wilk normality test
# data:  res_quiz2_no_gp3
# W = 0.97937, p-value = 0.2597

# Step 7: Conduct the one-way ANOVA overall test (3 groups)
model_factors_quiz2_no_gp3 <- aov(QuizScore ~ Group, quiz2_engaged_no_outlier_no_gp3)
summary(model_factors_quiz2_no_gp3)
# Result:
#             Df Sum Sq Mean Sq F value Pr(>F)
# Group        2    384  192.13     2.2  0.118
# Residuals   72   6288   87.33
# Result is not significant

# Repeat with the outlier included
model_factors_quiz2_inc_outlier_no_gp3 <- aov(QuizScore ~ Group, quiz2_engaged_no_gp3)
summary(model_factors_quiz2_inc_outlier_no_gp3)
# Result:
#             Df Sum Sq Mean Sq F value Pr(>F)
# Group        2    447   223.6   2.049  0.136
# Residuals   73   7968   109.2    
# Result is even less significant

# Step 9b: Check assumptions for an independent t-test to compare Sim group and Control group

# Remove group 1 (ST) from dataset, leaving only groups 0 (Control) and 2 (Sim)
quiz2_engaged_no_outlier_gp0_gp2 <- quiz2_engaged_no_outlier_no_gp3[quiz2_engaged_no_outlier_no_gp3$Group!="ST",]

# Test that groups 0 (Control) and 2 (Sim) have equal variances
leveneTest(QuizScore ~ Group, quiz2_engaged_no_outlier_gp0_gp2)

# Result:
#  Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  1  0.3952 0.5326
#       47

# Check that the control group is approximately normal
shapiro.test(quiz2_engaged_no_outlier_gp0$QuizScore)
# Result:
#	Shapiro-Wilk normality test
# data:  quiz2_engaged_gp0$QuizScore
# W = 0.95229, p-value = 0.2622
# Result: Can assume normality

# Check that the Sim group is approximately normal
shapiro.test(quiz2_engaged_no_outlier_gp2$QuizScore)
# Result:
#	Shapiro-Wilk normality test
# data:  quiz2_engaged_gp2$QuizScore
# W = 0.94819, p-value = 0.2682

# Step 10: Conduct an independent t-test to compare the Sim group and the Control group
# Run one-tailed t-test to compare Sim group with Control group
t.test(quiz2_engaged_no_outlier_gp2$QuizScore, 
       quiz2_engaged_no_outlier_gp0$QuizScore,
       alternative = "greater", var.equal = TRUE)

# Result:
#  Two Sample t-test
# data:  quiz2_engaged_no_outlier_gp2$QuizScore and quiz2_engaged_no_outlier_gp0$QuizScore
# t = 1.4368, df = 47, p-value = 0.0787
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   -0.5641847        Inf
# sample estimates:
#   mean of x mean of y 
# 83.13043  79.76923 
# Result is significant at 90% confidence level

# Step 11: Interpret result

# Re-run the t-test with outlier
t.test(quiz2_engaged_gp2$QuizScore, 
       quiz2_engaged_gp0$QuizScore,
       alternative = "greater",
       var.equal = TRUE)

# Result:
# Two Sample t-test
# data:  quiz2_engaged_gp2$QuizScore and quiz2_engaged_gp0$QuizScore
# t = 1.7263, df = 48, p-value = 0.04536
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   0.1395063       Inf
# sample estimates:
#   mean of x mean of y 
# 83.13043  78.22222 
# Result (p = .04536) is significant at 95% confidence level

# But is the control group approximately normal with the outlier?
shapiro.test(quiz2_engaged_gp0$QuizScore)
# Result:
#	Shapiro-Wilk normality test
# data:  quiz2_engaged_gp0$QuizScore
# W = 0.84011, p-value = 0.0007386
# Result: Cannot assume normality

# Re-run the t-test with both outliers removed
t.test(quiz2_engaged_no_outliers_gp2$QuizScore, 
       quiz2_engaged_no_outliers_gp0$QuizScore,
       alternative = "greater", var.equal = TRUE)

# Result:
# Two Sample t-test
# data:  quiz2_engaged_no_outliers_gp2$QuizScore and quiz2_engaged_no_outliers_gp0$QuizScore
# t = 1.1451, df = 46, p-value = 0.129
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#  -1.178899       Inf
# sample estimates:
#   mean of x mean of y 
# 83.13043  80.60000 
# Result (p = .129) is not significant

# Is the control group approximately normal without the outliers?
shapiro.test(quiz2_engaged_no_outliers_gp0$QuizScore)
# Result:
#	Shapiro-Wilk normality test
# data:  quiz2_engaged_no_outliers_gp0$QuizScore
# W = 0.97242, p-value = 0.7067
# Result: Can assume normalityy

###############
# EFFECT SIZE #
###############

# install.packages("effsize")
library(effsize)

#############
# Cohen's d #
#############
# Only compare group 2 to group 0

# Effect sizes are re-categorised as small (0.2), medium (0.4) or large (0.6) in education context 
# (Cumming, G. & Calin-Jageman, R. 2016)

# Original quiz 2 data
# Using control group sd
cohen.d(quiz2group2$QuizScore, quiz2group0$QuizScore, paired=FALSE, pooled=FALSE, hedges.correction=FALSE) # 0.3316128 (small)
# Pooled sd
cohen.d(quiz2group2$QuizScore, quiz2group0$QuizScore, paired=FALSE, pooled=TRUE, hedges.correction=FALSE) # 0.3476303 (small)

# Without zero-engagers
# Using control group sd
cohen.d(quiz2_engaged_gp2$QuizScore, quiz2_engaged_gp0$QuizScore, paired=FALSE, pooled=FALSE, hedges.correction=FALSE) # 0.4350944 (medium)
# Pooled sd
cohen.d(quiz2_engaged_gp2$QuizScore, quiz2_engaged_gp0$QuizScore, paired=FALSE, pooled=TRUE, hedges.correction=FALSE) # 0.4898387 (medium)

# Without zero-engagers and one outlier
# Using control group sd
cohen.d(quiz2_engaged_no_outlier_gp2$QuizScore, quiz2_engaged_no_outlier_gp0$QuizScore, paired=FALSE, pooled=FALSE, hedges.correction=FALSE) # 0.416443 (medium)
# Pooled sd
cohen.d(quiz2_engaged_no_outlier_gp2$QuizScore, quiz2_engaged_no_outlier_gp0$QuizScore, paired=FALSE, pooled=TRUE, hedges.correction=FALSE) # 0.4112753 (medium)

# Without zero-engagers and both outliers
# Using control group sd
cohen.d(quiz2_engaged_no_outliers_gp2$QuizScore, quiz2_engaged_no_outliers_gp0$QuizScore, paired=FALSE, pooled=FALSE, hedges.correction=FALSE) # 0.3608775 (medium)
# Pooled sd
cohen.d(quiz2_engaged_no_outliers_gp2$QuizScore, quiz2_engaged_no_outliers_gp0$QuizScore, paired=FALSE, pooled=TRUE, hedges.correction=FALSE) # 0.3308634 (small)

#########################
# Confounding Variables #
#########################

# Get crosstab of groups and engagement levels (excludes zero-engagers)
table(quiz2_engaged_no_outlier$Engagement, quiz2_engaged_no_outlier$Group)
# Result:
#   Control ST Sim ST+Sim
# 1       5  3   6      1
# 2      21 23  17     27

# Get a table of means by Group and Engagement for quiz 2
quiz2_scores_engagement <- 
  data.frame(quiz2_engaged_no_outlier$Group, 
             quiz2_engaged_no_outlier$Engagement, 
             quiz2_engaged_no_outlier$QuizScore)
names(quiz2_scores_engagement) <- c('Group', 'Engagement', 'QuizScore')
with(quiz2_scores_engagement, tapply(QuizScore, list(Group, Engagement), mean))
# Result:
#                1        2
# Control 83.20000 78.95238
# ST      71.33333 78.34783
# Sim     85.16667 82.41176
# ST+Sim  80.00000 73.48148

# Delays for Quiz 2

# Delay stats for participants minus two zero engagers and an outlier
mean(quiz2_engaged_no_outlier$Delay) # Result 0.1165049

table(quiz2_engaged_no_outlier$Delay)
# Result
# 0  1 
# 91 12 

# Number of delays by group
quiz2_scores_delays_by_group <- table(quiz2_engaged_no_outlier$Delay, quiz2_engaged_no_outlier$Group)
quiz2_scores_delays_by_group
# Result
#   Control ST Sim ST+Sim
# 0      25 24  22     20
# 1       1  2   1      8

# Get a crosstab of means by Group and Delay for quiz 2
quiz2_scores_delays <- 
  data.frame(quiz2_engaged_no_outlier$Group, 
             quiz2_engaged_no_outlier$Delay, 
             quiz2_engaged_no_outlier$QuizScore)
names(quiz2_scores_delays) <- c('Group', 'Delay', 'QuizScore')
with(quiz2_scores_delays, tapply(QuizScore, list(Group, Delay), mean))
# Result
#                0      1
# Control 79.64000 83.000
# ST      76.87500 85.500
# Sim     82.90909 88.000
# ST+Sim  74.55000 71.625

########################
# QUALITATIVE ANALYSIS #
########################

# CONTROL GROUP #

# Word cloud, horizontal bar plot and frequency table of bi/trigrams
textfile <- "RTextFiles/group0_fishery_sustainability_defn_corrected.txt"
myStopWords <- c("sustainability", "without", "must", "place", "can", "way", "using", 
                 "something", "will", "used", "certain", "use", "means", "think", "able", 
                 "fish", "taking", "adequate", "new", "per")
minWordFreq <- 2
analyseFreqWords(textfile, myStopWords, minWordFreq)
# Result (bi/trigrams):
#                       bitritoken Freq
# 312         sustainable yield    6
# 184       maximum sustainable    5
# 185 maximum sustainable yield    5
# 124               growth rate    3
# 16                boats catch    2
# 19               boats exceed    2
# 154             level maximum    2
# 173            manage fishing    2
# 287         reproduction rate    2
# 315  sustainable yield caught    2
# 340              year fishing    2
# 347              yield caught    2
# 352             yield numbers    2

########################
# Repeat for SIM GROUP #
########################

# Word cloud, horizontal bar plot and frequency table of bi/trigrams
textfile <- "RTextFiles/group2_fishery_sustainability_defn_corrected.txt"
myStopWords <- c("sustainability", "without", "must", "place", "can", "way", 
                 "using", "something", "will", "used", "certain", "use", "means", 
                 "think", "able", "fish", "taking", "adequate", "new", "per")
minWordFreq <- 2
analyseFreqWords(textfile, myStopWords, minWordFreq)
# Result (bi/trigrams):
#                       bitritoken Freq
# 179          maximum sustainable    8
# 180    maximum sustainable yield    8
# 287            sustainable yield    8
# 40                 catch matches    2
# 95                  fishing done    2
# 193           population decline    2
# 206        population maintained    2
# 223              quantity caught    2
# 234              rate population    2
# 292 sustainable yield population    2
# 309               within maximum    2
# 310   within maximum sustainable    2
# 329             yield population    2
# 330     yield population decline    2