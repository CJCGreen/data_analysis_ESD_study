# data_analysis_ESD_study
R scripts for data analysis for ESD (Education for Sustainable Development) study\
For doctoral thesis, September 2021\
Caroline Green

To cite the data set (with associated scripts) on Zenodo: [![DOI](https://zenodo.org/badge/406853626.svg)](https://zenodo.org/badge/latestdoi/406853626)

**Thesis name**: Systems Thinking and System Dynamics Simulation for Sustainability Education\
**University**: NUIG (National University of Ireland Galway)\
**Submitted**: September 2021

An open version of the ESD Learning Tool used for this research is here: https://exchange.iseesystems.com/public/carolineb/sustainability-learning-tool/

R SCRIPTS
---------
The R scripts reproduce all results in the thesis results chapters. There are four scripts. These are listed below, with corresponding thesis chapters.

1. **function_qualitative.R**\
R function for visualisations of qualitative data (word clouds, horizontal bar plots and ngrams)\
**Run before scripts 2 and 3**.
2. **analyse_quiz1_data.R**\
Source of results reported in Chapter 5: Results of ESD Learning Outcomes Experiment
3. **analyse_quiz2_data.R**\
Source of results reported in Chapter 6: Results of ESD Transfer Experiment
4. **background_variables.R**\
Source of results reported in Appendix 10: Analysis of possibly confounding variables in the main ESD study

DATA
----
The data folder contains the anonymised research data. Create a folder called data beneath the folder containing your R scripts.

The three scripts *analyse_quiz1_data.R*, *analyse_quiz2_data.R* and *background_variables.R* all require:
- data/scores_tidy.xlsx

In addition, *analyse_quiz1_data.R* requires the following text files:
- data/general_feedback_on_learning_tool_corrected.txt
- data/post_st_comments_corrected.txt
- data/post_sim_comments_corrected.txt
- data/group0_deer_sustainability_defn_corrected.txt
- data/group2_deer_sustainability_defn_corrected.txt

And finally, *analyse_quiz2_data.R* requires the following text files:
- data/group0_fishery_sustainability_defn_corrected.txt
- data/group2_fishery_sustainability_defn_corrected.txt

SURVEYS
-------
The surveys folder is for reference only. It contains the surveys and quizzes used to collect the data, together with quiz marking schemes, code books and quiz answers.

EXPERIMENTAL GROUPS
-------------------
In the data, group members are identified by a number from 0 to 3:\
group 0 means control group\
group 1 means ST group\
group 2 means Sim group\
group 3 means ST + Sim group

where:\
ST = Systems Thinking\
Sim = Simulation\
ST+Sim = Systems Thinking + Simulation

This page presents an overview. For more details see the readme.txt files in each folder.
