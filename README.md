# THEE_2025reproducibility
This project was created by **Ana-Hermina Ghenu** for the reproducibility module of the Theoretical Ecology and Evolution research practical (Fall 2024).

The live website for 2025 is at [this link.](https://evonerd.github.io/THEE_2025reproducibility/index.html)

Learning objectives:

1. show reproducibility principles in action,
2. introduce students to the format & conventions of a GitHub repository.
 
# Introduction
A readme file is the first thing a collaborator will encounter when you share your project with them. Its purpose is to orient them to your project: brief but detailed!

Structure it from wide to narrow: Start with the title and a short description of the project's motivation/purpose. If you have main results, you can list them here. Then explain how the project can be installed (e.g., any required dependencies like R or RStudio (versions?), any required packages). Next describe how the files are structured in the repository (which files run the code?, where can collaborators find the data?, etc.). Finally, you can let users know who they can contact in case they need help with your project.

# Purpose
Teaching students in the THEE research practical how to use GitHub by giving them an example repository that they can download and run on their computer.

# How to install
You will need R and RStudio. Any newer version should work (I used R 4.3.3 and RStudio 2023.12.1).

If you want to "print" the R notebook file to a .pdf or .html file (this is called "knitting" the notebook file), you will need to install the knitr package (version 1.49 or better). You can use the command: `install.packages("knitr")`

# File structure
In the main folder you can find the code for the exercises and their solutions in the file "reproducibility.Rmd" (with a knitted .html version as well). The file reproducibility.Rmd produces .csv files whose names will be in the format "numbers_1_to_10--multiplier \d .csv", 3 example csv files are shown.

There is 1 subfolder called "archived--2024". It contains course material from 2024: 2 pdf's of lecture slides and a subfolder with the code.

# Contact
This repository and its contents were developed and are maintained by Dr. Ana-Hermina Ghenu for Prof. Claudia Bank, Institute of Ecology & Evolution, University of Bern.
If you have any questions or need any help regarding the project, please contact Hermina via their GitHub account (EvoNerd).

*Note: check out the **website branch** if you are looking for the code to build and deploy the website.*
