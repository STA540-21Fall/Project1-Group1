# Project 1

**You may want to delete this file at the end, and substitute it with your own `readme.rmd` file (see [below](#readme)).**

We have been living in the pandemic for over 1.5 years, which deeply influences every aspect of the human society and beyond. In this project, you are required to investigate a concrete problem of your interest related to Covid-19. 

Each group should:
- identify a topic/problem of interest
- collect data legally to investigate the problem
- run statistical analysis
- create an R shiny app to visualize and illustrate your findings
- (Important) keep track of the whole process (from problem identification to final conclusion) with reproducible documentation.

--------------
## Repository structure

Following [suggestions](https://nicercode.github.io/blog/2013-04-05-projects/) by RICH FITZJOHN, the repository is organized as follows.

```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```

- The `app` directory contains the app files for the Shiny app.
  - `ui.R` and `server.R` are two key components for the Shiny App (or use a single `app.R` which includes both parts);
  - `global.R` is used to preprocess the data and define functions that are used in `server.R` (or `app.R`);
  - `data` folder contains the data used for deployment

- The `data` directory contains data used in the analysis. This is treated as read only; in paricular the R/python files are never allowed to write to the files in here. Depending on the project, these might be csv files, a database, and the directory itself may have subdirectories.

- The `doc` directory contains the report or presentation files. It can have subfolders.

- The `lib` directory contains files with function definitions. If you might need to write a helper function that is used through your analysis, this is a good place where the function should go. However, no functions should be called and executed here - they should only be defined here.

- The `output` directory contains analysis output, processed datasets, logs, or other processed things.

-------

## Suggested Workflow & Timeline

- Week 1: Study & Planning
  - Read project discription, identify potential topics of interest.
  - Discuss as a team about the topics (interest, difficulty, data accessibility, etc).
  - Collect and tidy data that might be needed for your topic.
- Week 2: Design & Development
  - Build statistical models to get insight and findings that you want to present.
  - Learn about R shiny and start with simple projects.
  - Design the visuals for R shiny app - how the design would be attractive and useful to assist your presentation?
  - While you are working on statistical models, you might start programming for R shiny using fake/simulated data.
- Week 3: Implementation & Presentation
  - Finalize the R shiny app and polish your report.
  - Prepare to present in class.

Each week you should be prepared to show your progress in lecture. While in the middle of a project, you are not required to have well-formed presentations, but it might be useful for your peers to provide suggestions and for the instructor to know which step you are at.

------

## ReadMe

You are required to post a `readme.md` file at the root of your repository, including your title, team members, project abstract and a contribution statement. (Feel free to delete this markdown file then.)

Below are two samples of contribution statements.

> All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement.

> AB, CD, EF and GH designed the study. AB and CD developed baseline classification model for evaluation. EF and GH explored feature engineering for improving the baseline model. AB, EF and GH discussed and designed the model evaluation protocol. CD carried out the computation for model evaluation. All team members contributed to the GitHub repository and prepared the presentation. All team members approve our work presented in our GitHub repository including this contribution statement.
