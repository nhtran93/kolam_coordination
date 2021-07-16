[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

# Limited scope for group coordination for stylistic variations in *kolam* art

The following repository contains data and code used for analyses in:

> Tran, N.-H., Kucharský, Š., Waring, T., Atmaca, S. & Beheim, B. A. (submitted). Limited scope for group coordination for stylistic variations in *kolam* art.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine. 

### Prerequisites

In order to be able to run the code, you will need to have an up-to-date version of [R](https://www.r-project.org/) installed on your computer and a few CRAN packages (see below). You will need [Stan](https://mc-stan.org/) in order to fit the statistical models. If your operating system is macOS Catalina or higher, you might encounter C++ compiler problems with Stan. To fix these C++ compiler problems, please check the following links: [Link1](https://discourse.mc-stan.org/t/dealing-with-catalina-iii/12731) and [Link2](https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/). To be able to plot the *kolam* drawings, you will further need to install the `kolam` R package.

```
# Install devtools package if necessary
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")

library(devtools)
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS"=TRUE)
devtools::install_github("rmcelreath/rethinking")

# Install required packages
install.packages(c("rstan", "ggpubr", "DiagrammeRsvg", "tidyr", "loo", "dplyr", "msm", "ggplot2", "DiagrammeR", "parallel", "tictoc", "patchwork", "igraph", "tidyverse", "gtools"))

# Install the stable development version of the kolam package
devtools::install_github("nhtran93/kolam")
```

### Data & Samples
The [data](data/) to run the analyses can be found all in `data/data.RData`. Two example kolam transcription files are in the repository in form of .yaml files. To get the full sample object for the statistical model in the main text, you will have to fit the statistical model.

### Structure of the Repository
`data/`: Contains the data to run the analyses.  
`output/`: Figures and posterior samples will be stored in this directory.  
`stan/`: Contains the Stan code for the full model in the main text as well as the additional models presented in the SI.  
`samples/`: This directory will store the MCMC samples.  

## Run the analyses
Download all files from https://github.com/nhtran93/kolam_coordination

If you use R, please set the working directory to the appropriate directory where you have saved these files. If you use RStudio, you can just open the respective RStudio project and the directory will be automatically set.

Code for all the figures in the manuscript can be found [here](plot_example_kolam_transitions.R) and [here](describe_models.R) .

Before fitting the statistical models, you will first need to check your directory:
```
# Make sure to check your working directory first. You should be in the root directory of this project. 
getwd()

# Make sure to create a directory where the fitted MCMC samples will be saved.
dir.create("samples")
```
**IMPORTANT NOTE**: Make sure you have enough disk space and working memory left on your computer. The statistical models take a while (~1h) to run and the samples are each up to 2GB big. It is recommended to run the statistical models on a cluster computer if possible.

You can run fit the full statistical model reported in the main text using the R script [fit_model.R](./fit_model.R). All your fitted MCMC samples will be saved in the directory [samples](./samples/). Afterwards, you can [describe](./describe_models.R) the models, compute the ICC and plot the Figures in the results section. The plots will be automatically saved in the directory [output](./output/). Please note that the R script [describe_models.R](./describe_models.R) will not be able to produce the plots without previously fitting the statistical models.

## Authors

* **[N.-Han Tran](https://www.eva.mpg.de/ecology/staff/han-tran/index.html)**
* [Šimon Kucharský](https://www.uva.nl/en/profile/k/u/s.kucharsky/s.kucharsky.html?cb)
* [Timothy Waring](https://timwaring.info/)
* [Silke Atmaca](https://www.eva.mpg.de/ecology/staff/silke-atmaca/index.html)
* [Bret A. Beheim](https://www.babeheim.com/)


## License

This project is licensed under the CC-BY-4.0 License - see the [LICENSE.md](LICENSE.md) file for details.
