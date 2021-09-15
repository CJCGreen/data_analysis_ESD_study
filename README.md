# data_analysis_ESD_study
R scripts for data analysis for ESD (Education for Sustainable Development) study\
For doctoral thesis, September 2021\
Caroline Green

**Thesis name**: Systems Thinking and System Dynamics Simulation for Sustainability Education\
**University**: NUIG (National University of Ireland Galway)\
**Submitted**: September 2021

R scripts for reproducing all results in the thesis results chapters.

SCRIPTS
-------
There are four scripts. These are listed below, with corresponding thesis chapters.

1. **function_qualitative.R**\
R function for visualisations of qualitative data (word clouds, horizontal bar plots and ngrams)\
Run before scripts 2 and 3.
2. **analyse_quiz1_data_clean.R**\
Source of results reported in Chapter 5: Results of ESD Learning Outcomes Experiment
3. **analyse_quiz2_data_clean.R**\
Source of results reported in Chapter 6: Results of ESD Transfer Experiment
4. **background_variables_clean.R**\
Source of results reported in Appendix 10: Analysis of possibly confounding variables in the main ESD study

DATA
----
***NB Data files not yet uploaded.***

The three scripts *analyse_quiz1_data_clean.R*, *analyse_quiz2_data_clean.R* and *background_variables_clean.R* all require:
- RDataSources/scores_tidy_ORIGINAL_DO_NOT_DELETE.xlsx

In addition, *analyse_quiz1_data_clean.R* requires the following text files:
- RTextFiles/general_feedback_on_learning_tool_corrected.txt
- RTextFiles/post_st_comments_corrected.txt
- RTextFiles/post_sim_comments_corrected.txt
- RTextFiles/group0_deer_sustainability_defn_corrected.txt
- RTextFiles/group2_deer_sustainability_defn_corrected.txt

And finally, *analyse_quiz2_data_clean.R* requires the following text files:
- RTextFiles/group0_fishery_sustainability_defn_corrected.txt
- RTextFiles/group2_fishery_sustainability_defn_corrected.txt
