# C Green 29 August 2021
# background_variables.R
# R script for results in Appendix 10: Analysis of possibly confounding variables in the main ESD study
#######################################################################################################
#
# Investigate effects of known background variables on all quiz 1 scores (including outlier)
# Most background variables are stored in the presurvey data, and quiz 1 scores are in quiz1 dataframe
# Analysis therefore requires merging two dataframes, presurvey and quiz1, by ParticipantID

# Background variables:
# -----------------------
# 1 Gender
# 2 Age Categories
# 3 Educational score
# 4 Prior sustainability knowledge
# 5 Prior ST/SD knowledge
# 6 Occupational/educational relevance
# 7 Engagement
# 8 Delay
# -----------------------

# Use xlsx package to import Excel
library(xlsx)

# presurvey dataframe contains presurvey data for all groups
presurvey <- read.xlsx("data/scores_tidy.xlsx", sheetIndex=1)

# Rename the group ids (from 0, 1, 2, and 3), and order them as factors
presurvey$Group <- ifelse(presurvey$Group==0, "Control", ifelse(presurvey$Group==1, "ST", ifelse(presurvey$Group==2, "Sim","ST+Sim"))) 
presurvey$Group <- factor(presurvey$Group, levels = c("Control", "ST", "Sim", "ST+Sim"))

# Colour palettes for graphs
library(RColorBrewer)

############
# 1 GENDER #
############

# Create a frequency table of group and gender
gender_breakdown <- table(presurvey$Gender)
gender_breakdown
# Result:
#       Female  Male 
#       62      44 

# Pie chart with percentages
pie(gender_breakdown, 
    main="Gender Breakdown: All Participants", 
    col=c("darkmagenta", "cornflowerblue"),
    labels=paste(names(gender_breakdown),"\n", gender_breakdown, " (", round(100*gender_breakdown/sum(gender_breakdown), digits = 1), "%)", sep=""))

gender_by_group <- table(presurvey$Gender, presurvey$Group)
gender_by_group
# Result
#        Control ST Sim ST+Sim
# Female      18 14  14     16
# Male        10 12  10     12

# Note that the legend had to be moved - increase the y axis max value with ylim
barplot(gender_by_group, 
        beside=T, 
        main="Gender by Group", 
        legend=TRUE, 
        ylab="Number of participants",
        ylim = c(0,20),
        col=c("darkmagenta", "cornflowerblue"),
        names.arg= c("Control", "ST", "Sim", "ST+Sim"))

# Gender and QuizScore: Is there a relationship?
presurvey_gender <- data.frame(presurvey$ParticipantID, presurvey$Gender, presurvey$Group)
names(presurvey_gender) <- c('ParticipantID', 'Gender', 'Group')
quiz1_scores_by_participant <- data.frame(quiz1$ParticipantID, quiz1$QuizScore)
names(quiz1_scores_by_participant) <- c('ParticipantID', 'QuizScore')

# Merge with quiz 1 results
quiz1_results_and_gender <- merge(presurvey_gender, quiz1_scores_by_participant)

# Side-by-side boxplots for gender
boxplot(QuizScore ~ Gender,
        data = quiz1_results_and_gender,
        main="Quiz 1 Scores by Gender", 
        ylab = "Score (%)",
        col = c("aquamarine3", "bisque2"))

# Chi-squared test on quiz1 data: are gender and group independent?
# install.packages("gmodels")
library(gmodels)
# Results for 106 participants:
CrossTable(quiz1_results_and_gender$Group, 
           quiz1_results_and_gender$Gender, 
           digits=1, 
           expected=TRUE, 
           prop.r=TRUE, 
           prop.c=TRUE, 
           prop.t=FALSE, 
           prop.chisq=TRUE, 
           sresid=FALSE, 
           format=c("SPSS"), 
           dnn = c("Group", "Gender"))
# Result: the p =  0.887335 means we cannot reject the null hypothesis that the variables are independent

# Get a table of means by Group and Gender
gender_group_means <- with(quiz1_results_and_gender, tapply(QuizScore, list(Group, Gender), mean))

# Result:
#           Female     Male
# Control 67.61111 76.50000
# Sim     74.78571 83.40000
# ST      73.42857 76.91667
# ST+Sim  71.37500 75.00000

barplot(gender_group_means, beside=TRUE, ylab="Quiz 1 Score (%)",
        main="Quiz 1 scores by Gender and Group",
        legend.text=c("Control", "ST", "Sim", "ST+Sim"),
        args.legend = list(x = "top", ncol = 2),
        ylim = c(0,90),
        col = brewer.pal(4, "Set3"))

#########
# 2 AGE #
#########

# Age Group categories used:
# Age Group Integer
# 18-25     1
# 26-35     2
# 36-45     3
# 46-55     4
# 56-65     5
# Over 65   6

# Age breakdown - all participants
age_breakdown <- table(presurvey$Age)
age_breakdown
# Result
# 18-25   26-35   36-45   46-55   56-65 Over 65 
#     7      15      18      24      19      23 

barplot(age_breakdown,
        main = "Age Breakdown: All Participants",
        xlab = "Age in years", 
        ylab = "No of participants",
        ylim = c(0,25),
        col = brewer.pal(nrow(age_breakdown), "Set3"))

# Basic statistics
# First, calculate mean and median age for all participants
# Since we're dealing with categorical age groups, a new column is needed first
# Add a numeric age for each category - this will give a value 1 for 18-25, 2 for 26-35 etc.
presurvey$AgeNum <- as.numeric(factor(presurvey$Age))
mean(presurvey$AgeNum)          # Result 3.962264 - taking midpoint of range that means age about 50
sort(table(presurvey$AgeNum))   # Result 4, ie age 46-55
median(presurvey$AgeNum)        # Result 4, ie age 46-55

# Age breakdown by group
age_by_group <- table(presurvey$Age, presurvey$Group)
age_by_group
# Result
#         Control ST Sim ST+Sim
# 18-25         1  2   2      2
# 26-35         4  3   5      3
# 36-45         4  5   4      5
# 46-55         7  6   7      4
# 56-65         9  4   4      2
# Over 65       3  6   2     12

# Get Group data
presurvey_group0 <- presurvey[presurvey$Group == "Control",]
presurvey_group1 <- presurvey[presurvey$Group == "ST",]
presurvey_group2 <- presurvey[presurvey$Group == "Sim",]
presurvey_group3 <- presurvey[presurvey$Group == "ST+Sim",]

median(presurvey$AgeNum) # 4
median(presurvey_group0$AgeNum) # 4
median(presurvey_group1$AgeNum) # 4
median(presurvey_group2$AgeNum) # 4
median(presurvey_group3$AgeNum) # 4.5

# To work out the mode, use a sorted table of frequencies
sort(table(presurvey$AgeNum)) # 4
sort(table(presurvey_group0$AgeNum)) # 5
sort(table(presurvey_group1$AgeNum)) # 4 and 6
sort(table(presurvey_group2$AgeNum)) # 4
sort(table(presurvey_group3$AgeNum)) # 6

# Boxplot age category for all, and by group
boxplot(presurvey$AgeNum,
        presurvey_group0$AgeNum, 
        presurvey_group1$AgeNum, 
        presurvey_group2$AgeNum, 
        presurvey_group3$AgeNum, 
        main="Age Category by Group",
        ylab="Age Category",
        col= c("aquamarine3", "azure3", "bisque2", "bisque2", "bisque2"),        
        names = c("All", "Control", "ST", "Sim", "ST+Sim"))

# Is there a relationship between age and score?
presurvey_age <- data.frame(presurvey$ParticipantID, presurvey$Age)
names(presurvey_age) <- c('ParticipantID', 'Age')

quiz1_scores_by_participant <- data.frame(quiz1$ParticipantID, quiz1$QuizScore)
names(quiz1_scores_by_participant) <- c('ParticipantID', 'QuizScore')

# Merge pre-survey age with scores from quiz1
quiz1_results_and_age <- merge(presurvey_age, quiz1_scores_by_participant)

# Get the quiz 1 scores per age group
quiz1_score_18_25 <- quiz1_results_and_age[quiz1_results_and_age$Age == "18-25",]
quiz1_score_26_35 <- quiz1_results_and_age[quiz1_results_and_age$Age == "26-35",]
quiz1_score_36_45 <- quiz1_results_and_age[quiz1_results_and_age$Age == "36-45",]
quiz1_score_46_55 <- quiz1_results_and_age[quiz1_results_and_age$Age == "46-55",]
quiz1_score_56_65 <- quiz1_results_and_age[quiz1_results_and_age$Age == "56-65",]
quiz1_score_over_65 <- quiz1_results_and_age[quiz1_results_and_age$Age == "Over 65",]

# Side-by-side boxplots for all age groups
boxplot(quiz1_results_and_age$QuizScore,
        quiz1_score_18_25$QuizScore, 
        quiz1_score_26_35$QuizScore, 
        quiz1_score_36_45$QuizScore, 
        quiz1_score_46_55$QuizScore, 
        quiz1_score_56_65$QuizScore,
        quiz1_score_over_65$QuizScore,
        main="Quiz 1 Scores by Age Group", 
        ylab = "Score (%)",
        names = c("All", "18-25", "26-35", "36-45", "46-55", "56-65", "Over 65"),
        col = c("aquamarine3", "bisque2", "bisque2", "bisque2", "bisque2", "bisque2", "bisque2"))

# Is age category a confounding variable in the relationship between group and score?
presurvey_group_age <- data.frame(presurvey$ParticipantID, presurvey$Group, presurvey$Age)
names(presurvey_group_age) <- c('ParticipantID', 'Group','Age')

# Merge for quiz 1 results
quiz1_results_and_group_and_age <- merge(presurvey_group_age, quiz1_scores_by_participant)

# Remove ParticipantID column, not needed for aggregating results
quiz1_results_and_group_and_age$ParticipantID <- NULL 

# Get a frequency table with age and group
group_by_age <- table(quiz1_results_and_group_and_age$Group, quiz1_results_and_group_and_age$Age)
group_by_age
# Result
#         18-25 26-35 36-45 46-55 56-65 Over 65
# Control     1     4     4     7     9       3
# ST          2     3     5     6     4       6
# Sim         2     5     4     7     4       2
# ST+Sim      2     3     5     4     2      12

# Get a table of means by Group and Age Group
with(quiz1_results_and_group_and_age, tapply(QuizScore, list(Group, Age), mean))
# Result
#         18-25    26-35 36-45    46-55    56-65  Over 65
# Control  78.0 75.25000 74.50 73.00000 66.11111 66.33333
# ST       73.0 75.00000 76.40 74.66667 68.75000 79.16667
# Sim      87.5 84.80000 70.75 72.28571 80.75000 85.00000
# ST+Sim   66.0 75.33333 84.60 61.50000 66.00000 73.58333

# Repeat but reduce the age categories, there are too few observations to stratify according to 6 categories
quiz1_results_and_group_and_age_adjusted <- quiz1_results_and_group_and_age
quiz1_results_and_group_and_age_adjusted$Age_adjusted <-
        ifelse(quiz1_results_and_group_and_age_adjusted$Age=='Over 65', 'Over 56', 
        ifelse(quiz1_results_and_group_and_age_adjusted$Age=='56-65', 'Over 56', 
        ifelse(quiz1_results_and_group_and_age_adjusted$Age=='46-55', '36-55', 
        ifelse(quiz1_results_and_group_and_age_adjusted$Age=='36-45', '36-55', 
        ifelse(quiz1_results_and_group_and_age_adjusted$Age=='26-35', '18-35',
        ifelse(quiz1_results_and_group_and_age_adjusted$Age=='18-25', '18-35', ''))))))

# Get a frequency table with adjusted age and group
group_by_age_adjusted <- table(quiz1_results_and_group_and_age_adjusted$Group, quiz1_results_and_group_and_age_adjusted$Age_adjusted)
group_by_age_adjusted
# Result
#         18-35 36-55 Over 56
# Control     5    11      12
# ST          5    11      10
# Sim         7    11       6
# ST+Sim      5     9      14

# Get a table of means by Group and adjusted Age Group
with(quiz1_results_and_group_and_age_adjusted, tapply(QuizScore, list(Group, Age_adjusted), mean))
# Result
#            18-35    36-55  Over 56
# Control 75.80000 73.54545 66.16667
# ST      74.20000 75.45455 75.00000
# Sim     85.57143 71.72727 82.16667
# ST+Sim  71.60000 74.33333 72.50000

#######################
# 3 EDUCATIONAL SCORE #
#######################

# Educational Attainment Scores:
# 1 Leaving Certificate
# 2 Degree or equivalent
# 3 Higher Diploma or Masters Degree
# 4 PhD

# First, calculate mean and median ed score for all participants (categorical data so it's approximate)
mean(presurvey$EdScore)         # 2.603774 
sort(table(presurvey$EdScore))  # mode is 3
median(presurvey$EdScore)       # 3

edscore_all <- table(presurvey$EdScore)
edscore_all
# Result:
# 1  2  3  4 
# 9 39 43 15 

barplot(edscore_all,
        main = "Educational Attainment: All Participants",
        names = c("Leaving\nCert", "Degree", "Masters", "PhD"), 
        ylab = "No of participants", 
        col = brewer.pal(nrow(edscore_all), "Set2"))

# Educational attainment and quizscore: Is there a relationship?
presurvey_ed <- data.frame(presurvey$ParticipantID, presurvey$Group, presurvey$EdScore)
names(presurvey_ed) <- c('ParticipantID', 'Group', 'EdScore')

# Merge for each set of quiz results
quiz1_results_and_ed <- merge(presurvey_ed, quiz1_scores_by_participant)

# Get a table of means by Ed
ed_means <- with(quiz1_results_and_ed, tapply(QuizScore, EdScore, mean))
ed_means
# Result:
#        1        2        3        4 
# 71.77778 70.76923 75.86047 79.20000 

# Get quiz 1 scores per age group
quiz1_score_ed1 <- quiz1_results_and_ed[quiz1_results_and_ed$EdScore == 1,]
quiz1_score_ed2 <- quiz1_results_and_ed[quiz1_results_and_ed$EdScore == 2,]
quiz1_score_ed3 <- quiz1_results_and_ed[quiz1_results_and_ed$EdScore == 3,]
quiz1_score_ed4 <- quiz1_results_and_ed[quiz1_results_and_ed$EdScore == 4,]

# Side-by-side boxplots for all age groups
boxplot(quiz1_results_and_ed$QuizScore,
        quiz1_score_ed1$QuizScore, 
        quiz1_score_ed2$QuizScore, 
        quiz1_score_ed3$QuizScore, 
        quiz1_score_ed4$QuizScore, 
        main="Quiz 1 Scores by Educational Attainment", 
        ylab = "Score (%)",
        names = c("All", "Leaving Cert", "Degree", "Masters", "PhD"),
        col = c("aquamarine3", "bisque2", "bisque2", "bisque2", "bisque2"))

# Create a frequency table of Group and EdScore
edscore_by_group <- table(presurvey$EdScore, presurvey$Group)
# I used the below for transposing the table, easier for my written report:
edscore_by_group_flipped <- table(presurvey$Group, presurvey$EdScore)
edscore_by_group_flipped
# Result:
#          1  2  3  4
# Control  1 10 12  5
# ST       3 11 11  1
# Sim      3  7 11  3
# ST+Sim   2 11  9  6

# Use Barplot with bars beside option
barplot(edscore_by_group, 
        beside=T, 
        main="Educational Attainment by Group", 
        legend=TRUE, 
        legend.text=c("Leaving Cert", "Degree", "Masters", "PhD"), 
        ylab="No of participants",
        xlab="Group",
        ylim = c(0,15),
        names.arg= c("Control", "ST", "Sim", "ST+Sim"),
        col = brewer.pal(nrow(edscore_by_group), "Set2"))

# Repeat but reduce the ed levels, there are too few observations to stratify
quiz1_results_and_ed_adjusted <- quiz1_results_and_ed
quiz1_results_and_ed_adjusted$Ed_adjusted <- 
    ifelse(quiz1_results_and_ed_adjusted$EdScore== 1 | quiz1_results_and_ed_adjusted$EdScore== 2, "1-2", 
    ifelse(quiz1_results_and_ed_adjusted$EdScore== 3 | quiz1_results_and_ed_adjusted$EdScore== 4, "3-4", ''))
reduce_edscore_by_group <- table(quiz1_results_and_ed_adjusted$Group, quiz1_results_and_ed_adjusted$Ed_adjusted)
reduce_edscore_by_group
# Result:
#         1-2 3-4
# Control  11  17
# ST       14  12
# Sim      10  14
# ST+Sim   13  15

# Chi-squared test on quiz1 data: are educational score and group independent?
library(gmodels)
CrossTable(quiz1_results_and_ed_adjusted$Group, 
           quiz1_results_and_ed_adjusted$Ed_adjusted, 
           digits=1, 
           expected=TRUE, 
           prop.r=TRUE, 
           prop.c=TRUE, 
           prop.t=FALSE, 
           prop.chisq=TRUE, 
           sresid=FALSE, 
           format=c("SPSS"), 
           dnn = c("Group", "Ed Score"))
# Result: the p = 0.7250024 means we cannot reject the null hypothesis that the variables are independent

####################################
# 4 PRIOR SUSTAINABILITY KNOWLEDGE #
####################################

# Prior Sustainability Scores:
# 0 None at all
# 1 A little
# 2 A moderate amount
# 3 A lot

# First, calculate mean and median for all participants (categorical data so it's approximate)
mean(presurvey$PriorSustKnowledgeAdjusted)              # 1.198113 
sort(table(presurvey$PriorSustKnowledgeAdjusted))       # mode is 0
median(presurvey$PriorSustKnowledgeAdjusted)            # 1

sus_score_all <- table(presurvey$PriorSustKnowledgeAdjusted)
sus_score_all
# Result:
#  0  1  2  3 
# 38 28 21 19 

barplot(sus_score_all,
        main = "Prior Sustainability Knowledge: All Participants",
        names = c("None at all", "A little", "A moderate amount", "A lot"), 
        ylab = "No of participants", 
        col = brewer.pal(nrow(sus_score_all), "Set2"))

# Prior sustainability knowledge by group
# Create a frequency table
sus_score_by_group <- table(presurvey$PriorSustKnowledgeAdjusted, presurvey$Group)
sus_score_by_group_flipped <- table(presurvey$Group, presurvey$PriorSustKnowledgeAdjusted)
sus_score_by_group_flipped
# Result
#          0  1  2  3
# Control 10  8  5  5
# ST       6  4  6 10
# Sim      8  9  5  2
# ST+Sim  14  7  5  2

# Barplot sustainability knowledge by group
barplot(sus_score_by_group, 
        beside=T, 
        main="Prior Sustainability Knowledge by Group", 
        legend=TRUE, 
        legend.text=c("None at all", "A little", "A moderate amount", "A lot"), 
        ylab="No of participants",
        args.legend = list(x = "top"),
        names.arg= c("Control", "ST", "Sim", "ST+Sim"),
        col = brewer.pal(nrow(sus_score_all), "Set2"))

# Boxplot sustainability knowledge for all, and by group
boxplot(presurvey$PriorSustKnowledgeAdjusted,
        presurvey_group0$PriorSustKnowledgeAdjusted, 
        presurvey_group1$PriorSustKnowledgeAdjusted, 
        presurvey_group2$PriorSustKnowledgeAdjusted, 
        presurvey_group3$PriorSustKnowledgeAdjusted, 
        main="Prior Sustainability Knowledge Scores by Group",
        ylab="Sustainability Knowledge Score",
        col= c("aquamarine3", "azure3", "bisque2", "bisque2", "bisque2"),
        names = c("All", "Control", "ST", "Sim", "ST+Sim"))

# Prior sustainability knowledge and quizscore: is there a relationship?
presurvey_prior_sust_know <- data.frame(presurvey$ParticipantID, as.factor(presurvey$PriorSustKnowledgeAdjusted))
names(presurvey_prior_sust_know) <- c('ParticipantID', 'PriorSustKnowledgeAdjusted')

# Merge
quiz1_results_and_prior_sust_know <- merge(presurvey_prior_sust_know, quiz1_scores_by_participant)

# Get the quiz 1 scores per prior sust knowledge category
quiz1_score_0 <- quiz1_results_and_prior_sust_know[quiz1_results_and_prior_sust_know$PriorSustKnowledgeAdjusted == "0",]
quiz1_score_1 <- quiz1_results_and_prior_sust_know[quiz1_results_and_prior_sust_know$PriorSustKnowledgeAdjusted == "1",]
quiz1_score_2 <- quiz1_results_and_prior_sust_know[quiz1_results_and_prior_sust_know$PriorSustKnowledgeAdjusted == "2",]
quiz1_score_3 <- quiz1_results_and_prior_sust_know[quiz1_results_and_prior_sust_know$PriorSustKnowledgeAdjusted == "3",]

# Side-by-side boxplots
boxplot(quiz1_results_and_prior_sust_know$QuizScore,
        quiz1_score_0$QuizScore, 
        quiz1_score_1$QuizScore, 
        quiz1_score_2$QuizScore, 
        quiz1_score_3$QuizScore, 
        main="Quiz 1 Scores by Prior Sustainability Knowledge", 
        ylab = "Score (%)",
        names = c("All", "None at all", "A little", "Moderate", "A lot"),
        xlab = "Prior Sustainability Knowledge",
        col = c("aquamarine3", "bisque2", "bisque2", "bisque2", "bisque2"))

# Is prior sustainability experience a confounding variable in the relationship between group and score?
presurvey_group_sus_knowledge <- data.frame(presurvey$ParticipantID, presurvey$Group, presurvey$PriorSustKnowledgeAdjusted)
names(presurvey_group_sus_knowledge) <- c('ParticipantID', 'Group','PriorSustKnowledgeAdjusted')

# Merge for quiz 1 results
quiz1_results_and_group_and_sus_knowledge <- merge(presurvey_group_sus_knowledge, quiz1_scores_by_participant)

# Remove ParticipantID column, not needed for aggregating results
quiz1_results_and_group_and_sus_knowledge$ParticipantID <- NULL 

# Get a table of means by Group and Prior Sus Knowledge
with(quiz1_results_and_group_and_sus_knowledge, tapply(QuizScore, list(Group, PriorSustKnowledgeAdjusted), mean))
# Result
#                0        1        2    3
# Control 70.10000 69.25000 73.20000 72.2
# ST      67.33333 73.00000 78.83333 78.2
# Sim     80.62500 77.22222 73.20000 87.5
# ST+Sim  75.21429 67.71429 69.40000 84.0

# Try chi squared test to test independence of group and prior sustainability knowledge:
# library(gmodels)
CrossTable(quiz1_results_and_group_and_sus_knowledge$Group, 
           quiz1_results_and_group_and_sus_knowledge$PriorSustKnowledgeAdjusted, 
           digits=1, 
           expected=TRUE, 
           prop.r=TRUE, 
           prop.c=TRUE, 
           prop.t=FALSE, 
           prop.chisq=TRUE, 
           sresid=FALSE, 
           format=c("SPSS"), 
           dnn = c("Group", "Prior Sus Knowledge"))

# Not enough observations in some of the cells, so not valid for Chi-squared test
# Repeat but reduce the levels
quiz1_results_and_sus_adjusted <- quiz1_results_and_group_and_sus_knowledge
quiz1_results_and_sus_adjusted$sus_adjusted <- 
        ifelse(quiz1_results_and_sus_adjusted$PriorSustKnowledgeAdjusted== 0 | 
                       quiz1_results_and_sus_adjusted$PriorSustKnowledgeAdjusted== 1, "0-1", 
        ifelse(quiz1_results_and_sus_adjusted$PriorSustKnowledgeAdjusted== 2 | 
                       quiz1_results_and_sus_adjusted$PriorSustKnowledgeAdjusted== 3, "2-3", ''))
CrossTable(quiz1_results_and_sus_adjusted$Group, 
           quiz1_results_and_sus_adjusted$sus_adjusted, 
           digits=1, 
           expected=TRUE, 
           prop.r=TRUE, 
           prop.c=TRUE, 
           prop.t=FALSE, 
           prop.chisq=TRUE, 
           sresid=FALSE, 
           format=c("SPSS"), 
           dnn = c("Group", "Prior Sus Knowledge"))
# Result: the p =  0.02927516 is significant, so we REJECT the null hypothesis that the variables are independent

###########################
# 5 Prior ST/SD knowledge #
###########################

# Prior STSD Scores:
# 0 None at all
# 1 A little
# 2 A moderate amount
# 3 A lot

# First, calculate mean and median for all participants (categorical data so it's approximate)
mean(presurvey$PriorSTSDKnowledge)              # 0.2264151 
sort(table(presurvey$PriorSTSDKnowledge))       # Mode 0
median(presurvey$PriorSTSDKnowledge)            # Result 0

sdst_score_all <- table(presurvey$PriorSTSDKnowledge)
sdst_score_all
# Result:
#  0  1  2  3 
# 92  7  4  3 

barplot(sdst_score_all,
        main = "Prior Systems Thinking / System Dynamics Knowledge: All Participants",
        names = c("None at all", "A little", "A moderate amount", "A lot"), 
        ylab = "No of participants", 
        ylim = c(0, 90),
        col = brewer.pal(nrow(sdst_score_all), "Set2"))

# Prior Systems Thinking / System Dynamics Knowledge by group 

# Create a frequency table of Group and Prior ST/SD Knowledge
stsd_score_by_group <- table(presurvey$PriorSTSDKnowledge, presurvey$Group)
stsd_score_by_group_flipped <- table(presurvey$Group, presurvey$PriorSTSDKnowledge)
stsd_score_by_group_flipped
# Result
#          0  1  2  3
# Control 21  4  2  1
# ST      25  1  0  0
# Sim     21  2  0  1
# ST+Sim  25  0  2  1

barplot(stsd_score_by_group, 
        beside=T, 
        main="Prior Systems Thinking / System Dynamics Knowledge by Group", 
        legend=TRUE, 
        legend.text=c("None", "A little", "A moderate amount", "A lot"), 
        args.legend = list(x = "top", ncol = 2),
        ylim = c(0,30),
        ylab="No of participants", 
        names.arg= c("Control", "ST", "Sim", "ST+Sim"),
        col = brewer.pal(nrow(sdst_score_all), "Set2"))

# Prior ST knowledge and quiz score: Is there a relationship?
presurvey_prior_st_know <- data.frame(presurvey$ParticipantID, as.factor(presurvey$PriorSTSDKnowledge))
names(presurvey_prior_st_know) <- c('ParticipantID', 'PriorSTSDKnowledge')

# Merge for quiz 1 results
quiz1_results_and_prior_st_know <- merge(presurvey_prior_st_know, quiz1_scores_by_participant)

# Get the quiz 1 scores per prior ST knowledge knowledge category
quiz1_st_score_0 <- quiz1_results_and_prior_st_know[quiz1_results_and_prior_st_know$PriorSTSDKnowledge == "0",]
quiz1_st_score_1 <- quiz1_results_and_prior_st_know[quiz1_results_and_prior_st_know$PriorSTSDKnowledge == "1",]
quiz1_st_score_2 <- quiz1_results_and_prior_st_know[quiz1_results_and_prior_st_know$PriorSTSDKnowledge == "2",]
quiz1_st_score_3 <- quiz1_results_and_prior_st_know[quiz1_results_and_prior_st_know$PriorSTSDKnowledge == "3",]

# Side-by-side boxplots
boxplot(quiz1_results_and_prior_st_know$QuizScore,
        quiz1_st_score_0$QuizScore, 
        quiz1_st_score_1$QuizScore, 
        quiz1_st_score_2$QuizScore, 
        quiz1_st_score_3$QuizScore, 
        main="Quiz 1 Scores by Prior Systems Thinking Knowledge", 
        ylab = "Score (%)",
        names = c("All", "None at all", "A little", "Moderate", "A lot"),
        xlab = "Prior Sustainability Knowledge",
        col = c("aquamarine3", "bisque2", "bisque2", "bisque2", "bisque2"))

##############################################
# 6 Occupational/educational relevance score #
##############################################

# Prior OccOrStudyRelevanceScore Scores:
# 0 Not at all relevant
# 1 A little relevant
# 2 Moderately relevant
# 3 Quite relevant
# 4 Highly relevant

# First, calculate mean and median ed score for all participants (categorical data so it's approximate)
mean(presurvey$OccOrStudyRelevanceScore)        # 1.301887
sort(table(presurvey$OccOrStudyRelevanceScore)) # Mode is 0
median(presurvey$OccOrStudyRelevanceScore)      # Result 1

occ_score_all <- table(presurvey$OccOrStudyRelevanceScore)
occ_score_all
# Result:
#  0  1  2  3  4 
# 38 30 10 24  4 

barplot(occ_score_all,
        main = "Occupational or Educational Relevance: All Participants",
        names = c("Not at all", "A little", "Moderately", "Quite", "Highly"), 
        ylab = "No of participants", 
        xlab = "How Relevant",
        col = brewer.pal(nrow(occ_score_all), "Set2"))

# Create a frequency table of Group and EdOccRelevance
ed_occ_rel_score_by_group <- table(presurvey$OccOrStudyRelevanceScore, presurvey$Group)
# Flip the matrix for reporting purposes
ed_occ_rel_score_by_group_flipped <- table(presurvey$Group, presurvey$OccOrStudyRelevanceScore)
ed_occ_rel_score_by_group_flipped

barplot(ed_occ_rel_score_by_group, 
        beside=T, 
        main="Occupational or Educational Relevance by Group", 
        legend=TRUE, 
        legend.text=c("Not at all", "A little", "Moderately", "Quite", "Highly"), 
        ylab="No of participants",
        xlab = "How Relevant",
        ylim = c(0,13),        
        names.arg= c("Control", "ST", "Sim", "ST+Sim"),
        col = brewer.pal(nrow(occ_score_all), "Set2"))

# Occupational or educational relevance and quiz score: is there a relationship?
presurvey_prior_occ <- data.frame(presurvey$ParticipantID, as.factor(presurvey$OccOrStudyRelevanceScore))
names(presurvey_prior_occ) <- c('ParticipantID', 'OccOrStudyRelevanceScore')

# Merge for quiz 1 results
quiz1_results_and_prior_occ <- merge(presurvey_prior_occ, quiz1_scores_by_participant)

# Get the quiz 1 scores per relevance category
quiz1_occ_score_0 <- quiz1_results_and_prior_occ[quiz1_results_and_prior_occ$OccOrStudyRelevanceScore == "0",]
quiz1_occ_score_1 <- quiz1_results_and_prior_occ[quiz1_results_and_prior_occ$OccOrStudyRelevanceScore == "1",]
quiz1_occ_score_2 <- quiz1_results_and_prior_occ[quiz1_results_and_prior_occ$OccOrStudyRelevanceScore == "2",]
quiz1_occ_score_3 <- quiz1_results_and_prior_occ[quiz1_results_and_prior_occ$OccOrStudyRelevanceScore == "3",]
quiz1_occ_score_4 <- quiz1_results_and_prior_occ[quiz1_results_and_prior_occ$OccOrStudyRelevanceScore == "4",]

# Side-by-side boxplots
boxplot(quiz1_results_and_prior_occ$QuizScore,
        quiz1_occ_score_0$QuizScore, 
        quiz1_occ_score_1$QuizScore, 
        quiz1_occ_score_2$QuizScore, 
        quiz1_occ_score_3$QuizScore,
        quiz1_occ_score_4$QuizScore,         
        main="Quiz 1 Scores by Occupational or Educational Relevance",
        ylab = "Score (%)",
        names = c("All", "Not at all", "A little", "Moderately", "Quite", "Highly"),
        xlab = "Whether Occupation or Education Relevant",
        col = c("aquamarine3", "bisque2", "bisque2", "bisque2", "bisque2", "bisque2"))

################
# 7 ENGAGEMENT #
################
# Note: These engagement scores are for quiz 1. Zero engagers for quiz 1 were already removed from 
# the dataset of 106, because they did not properly engage with the introduction, ST and/or Sim sections, 
# making the data unsuitable for assessing the impact of ST and/or Sim on learning outcomes.

# Engagement scores:
# 0 Unacceptable
# 1 Minimal
# 2 Good

# Engagement for ALL participants for Quiz 1
engagement_quiz1_all <- quiz1$Engagement

# Crosstab: Engagement frequencies
quiz1_engagement_table <- table(engagement_quiz1_all)
quiz1_engagement_table
# Result
# 1   2 
# 6 100 

# Crosstab: Engagement level by group
quiz1_engagement_by_group <- table(quiz1$Engagement, quiz1$Group)
quiz1_engagement_by_group
# Result
#   Control ST Sim ST+Sim
# 1       2  2   2      0
# 2      26 24  22     28

############
# 8 DELAYS #
############
# Note: These delay scores are for quiz 1.

# Delay scores:
# 0 No significant delay
# 1 Significant delay

# Delays for ALL participants for Quiz 1
delays_quiz1_all <- quiz1$Delay

# Crosstab: Delay frequencies
quiz1_delays_table <- table(delays_quiz1_all)
quiz1_delays_table
# Result
#  0  1 
# 89 17 

# Is number of delays a confounding variable in the relationship between group and score?
# Extract quiz1 entries where there was a delay
delay_quiz1 <- quiz1[quiz1$Delay>0,]
# Create a frequency table, no of delays by group
quiz1_delays <- table(delay_quiz1$Delay, delay_quiz1$Group)
quiz1_delays
# Result (Delay=1)
#   Control ST Sim ST+Sim
# 1       0  4   2     11

# Delays by group barplot
barplot(quiz1_delays,
        main = "Delays for Quiz 1 by Group",
        names = c("Control", "ST", "Sim", "ST+Sim"), 
        ylab = "No of participants",
        col = brewer.pal(ncol(quiz1_delays), "Set2"))

# Get a table of means by Group and Delays
quiz1_scores_delays <- data.frame(quiz1$Group, quiz1$Delay, quiz1$QuizScore)
names(quiz1_scores_delays) <- c('Group', 'Delay', 'QuizScore')
with(quiz1_scores_delays, tapply(QuizScore, list(Group, Delay), mean))
# Results:
#                0    1
# Control 70.78571   NA
# ST      75.09091 79.5
# Sim     79.45455 66.5
# ST+Sim  72.88235 73.0