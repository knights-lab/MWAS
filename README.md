R Package: MWAS
=========
Biomarker Discovery in Microbiome-Wide Association Studies (MWAS)
Performs best-practice microbiome analyses for biomarker discovery, visualization, and predictive modeling using machine learning.

Version: 0.1.0
Author: Dan Knights
Maintainer: Dan Knights <dknights@umn.edu>

License: MIT License

##Dependencies:
First, install the following R package dependencies: `flux`, `randomForest`, and `lars`.

## Installation
You can install directly from the github repository using the `devtools` package in `R`, with:
```R
install.packages('devtools')
install_github("knights-lab/MWAS")
```

If you have trouble installing the `devtools` package, you can install with `install.packages` instead:

Linux:
```R
install.packages('https://github.com/knights-lab/MWAS/raw/master/release.tar.gz',repo=NULL,method='wget')
```

Mac:
```R
install.packages('https://github.com/knights-lab/MWAS/raw/master/release.tar.gz',repo=NULL)
```

or, if these fail, try:
```R
install.packages('https://github.com/knights-lab/MWAS/raw/master/release.tar.gz',repo=NULL, method='curl')
```

or:
```R
install.packages('https://github.com/knights-lab/MWAS/raw/master/release.tar.gz',repo=NULL, method='liburl')
```

