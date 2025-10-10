# THEE_2025reproducibility

This branch hosts the (website used for the THEE reproducibility module) [https://evonerd.github.io/THEE_2025reproducibility/aboutTHEE.html]).

*Note: the **main branch** is used during the module to train students on version control, efficient collaboration, and managing conflicts via Git(Hub).*

# Purpose

Having the slides and exercises on GitHub Pages makes it easy for students to access and allows the module to be reproducibly passed down to the next instructor(s).

The website was created in R Studio as a website project.

# How to install

You will need R (4.4.3) and RStudio (2024.12.1+563). Additional package dependencies can be found in "sessionInfo.txt" in the main folder.

# File structure

The main files for the website are in the main folder: the .Rpoj file as well as "index.Rmd". Take a look at "index.Rmd" to get yourself oriented.

Slide decks are found in the "slide_decks" folder. These are also generated in R using the xaringan package (tutorials available (here)[https://rviews.rstudio.com/2021/11/18/deploying-xaringan-slides-a-ten-step-github-pages-workflow/#the-ten-step-workflow] and (here)[https://slides.yihui.org/xaringan/#1]). Each subfolder of "slide_decks" is for 1 slide deck, whose main file is always called "theSlides.Rmd".

Figures for the website are found in the "figures" folder, including a Powerpoint (.odt) file that was used to natively generate the first figure on the site.

Building the website in RStudio automatically creates the files in the "docs" subfolder, so try not to manually touch anything in there.

# Contact

This repository and its contents were developed and are maintained by Dr. Ana-Hermina Ghenu for Prof. Claudia Bank, Institute of Ecology & Evolution, University of Bern.

If you have any questions or want to contribute, please contact Hermina via their GitHub account (EvoNerd).
