The three scripts analyse_quiz1_data.R, analyse_quiz2_data.R and background_variables.R all require:
- data/scores_tidy.xlsx (contains results for all the surveys/quizzes: quiz 1, quiz2, pre-survey and ST and Sim usefulness feedback ratings)

In addition, analyse_quiz1_data.R requires the following text files:
- data/general_feedback_on_learning_tool_corrected.txt
- data/post_st_comments_corrected.txt
- data/post_sim_comments_corrected.txt
- data/group0_deer_sustainability_defn_corrected.txt
- data/group2_deer_sustainability_defn_corrected.txt

And finally, analyse_quiz2_data.R requires the following text files:
- data/group0_fishery_sustainability_defn_corrected.txt
- data/group2_fishery_sustainability_defn_corrected.txt

The txt files above contain tidied qualitative answers to quiz questions, which are analysed using word clouds etc. The same qualitative answers were given a numeric score, indicating quality, in scores_tidy.xlsx, as they would in an exam. The original qualitative answers were contained in the original SurveyMonkey results data. This has not been uploaded because the data contains identifying information. Data in scores_tidy.xlsx have been anonymised, and there is no identifying information in the txt files.
