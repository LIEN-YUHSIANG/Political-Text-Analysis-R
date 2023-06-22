###############################################################################
#        Waseda University :: School of Political Science & Economics         #
#            - Political Text Analysis [2023 Spring Semester] -               #
#             Instructor: Rob Fahey <robfahey@aoni.waseda.jp>                 #
###############################################################################


# Assignment 05:  Topic Models
# ----------------------------


# The file guardian_20.csv contains a very similar dataset to the one we used
# in class - a selection of articles from The Guardian, but this time from 2020.

# Your task is import this data, pre-process it as required, and then run a
# Structural Topic Model analysis to find the best model to describe the data.

# Most of the code required to do this will be exactly the same as the in-class
# example. The real assignment here is INTERPRETATION - you need to figure out
# what each topic means (by looking at keywords and headlines), and assign it
# a descriptive name. If there are topics you can't interpret, or if you think
# some topics are mixing up categories that should be separated, you should try
# re-running STM with a different number of topics until you get a list you are
# happy with.

# Note: Remember that we set the maximum iterations to a low level so that the
#       would finish running quickly in class. You should remove this parameter
#       when running these models at home.


# TASKS:

# 1) Import the data, fit a Structural Topic Model.

# 2) Interpret the categories, giving each one a descriptive name. You may well
#    need to re-run the STM process with a different category number.

# 3) Identify any categories in your data which deal with the COVID-19 pandemic.
#    Estimate the effect of the Month variable on these categories, and plot
#    a chart showing the topic proportion for the COVID categories each month.


# Your submission should consist of an R file (include detailed comments, especially
# if something isn't working!), and a PDF or PNG file with the chart for Task 
# 3. You can export charts from RStudio with the "Export" button above the 
# chart display.

# Remember, even if you can't complete some part of the assignment, you can get
# a decent grade by attempting it and showing how you tried to solve any problem
# you encountered. Using Google, Stack Overflow, and ChatGPT is encouraged, but
# be sure to document how you used them - show what questions you asked, and how
# you understood the response.



