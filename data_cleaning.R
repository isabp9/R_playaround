library(tidyverse)

## already connected R with git and github and own account. 
## now to create new project and connect it to git:
library(usethis)
use_git()
## then use right pane ("Git") and commit from there to save changes 

## to create repository for the project in github
use_github()

#### WORKFLOW ####
#1. Commit (to git) in right pane selecting file to commit
#2. Push (to github repo) using arrow in right pane
#3. Pull (from github repo) if collaborating with others 