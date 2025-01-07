# Project
## Overview
This repo contains the data and code used for the study presented in the following paper:

[*Parallel scaling of elite wealth in ancient Roman and modern cities with implications for sustainability.*]()

** This paper is currently under review **

## Archive
This Repository has been archived with Zenodo:

[![DOI](https://zenodo.org/badge/620744563.svg)](https://doi.org/10.5281/zenodo.10925901)

## Abstract
Rapid urbanization and rising inequality are pressing global concerns, yet inequality is an ancient trait of city life that may be intrinsictly connected to the urbanism itself. Here we investigate how elite wealth scales with urban population size across culture and time by analysing ancient Roman and modern cities. Using Bayesian models to address archaeological uncertainties, we uncovered a consistent correlation between population size and physical expressions of elite wealth in urban spaces. These patterns suggest the presence of an ancient, enduring mechanism underlying urban inequality. Supported by an agent-based network simulation and informed by Settlement Scaling Theory, we propose that the observed patterns arise from simple, common preferential attachment in social networks---a simple, yet powerful driver of unequal access to interaction potential. Our findings open up new directions in urban scaling research and underscore the importance of understanding long-term urban dynamics to chart a course toward a fairer urban future.

## Software
The R and Python scripts contained in this repository are intended for replication efforts and to improve the transparency of research. They are, of course, provided without warranty or technical support. That said, questions about the code can be directed to me, Chris Carleton, at ccarleton@protonmail.com.

### R
Most of this analysis described in the associated manuscript was performed in R. Thus, you may need to download the latest version of [R](https://www.r-project.org/) in order to make use of the scripts described below. Just download this repo, leaving the structure and files intact. Then open an R session with the working environment set to the parent directory of this repo and run the provided R script either line-by-line or with R::source(). See Src/R_environment_info.txt for information about the R environment used for this anlaysis including platform, verions, and packages.

### Python
Some analyses were also conductd using [Python](https://www.python.org/) in a [Jupyter notebook](https://jupyter.org/) with a [Conda](https://docs.anaconda.com/free/miniconda/index.html) environment. See this repo at Src\conda_environment.yml for the packages and versions used in the environment to perform the anlayses described in the paper. That YML file can be used to reconstruct the environmnet using your own Anaconda/Miniconda installation. Then, open the .ipynb file in a Jupyter notebook, select the kernel aligning with the recreated conda environment, and run the cells in the notebook.

### Nimble
This project made use of a Bayesian Analysis package called [Nimble](https://r-nimble.org/). See the Nimble website for documentation and a tutorial. Then, refer to the R scripts in this repo.

## Supplementary Information
See the `Supplement` folder within this repo for further supplementary documentation and information about how that document was produced. More specifically, the main supplementary information published with the associated research paper is `Supplement/supplement_compressed.pdf` and it contains a guide to this repo, supplementary results supporting the main analysis, a detailed outline of the analyses run in the main `Src/urban_wealth_scale.R` script, and summaries of MCMC convergence diagnostics. The document itself was created paritally by automation with Python and details about that process are located in `Supplement/supplement_builder.ipynb`. It was built with [Quarto](https://quarto.org/) from markdown files in VS Code.

## Contact

[ORCID](https://orcid.org/0000-0001-7463-8638) |
[Google Scholar](https://scholar.google.com/citations?hl=en&user=0ZG-6CsAAAAJ) |
[Website](https://wccarleton.me)

## License

Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
