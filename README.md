# CsvViewer

# Introduction
This R Package creates a Shiny App to review all csv files in a folder and all its subfolders.

# Features

- Allows to add comments per csv file which are saved in the main folder as json file.
- A documentation.json can be provided that can contain instructions per file or folder.

# Screenshots

<img src="https://github.com/EHDEN/CSVViewer/raw/main/inst/images/screenshot.png"/>


## Installation

```
install.packages("remotes")
remotes::install_github("ehden/CsvViewer")
```

## Execution

To run on an example set of csv files run the following code:
```
library(CSVViewer)
CSVViewer::launchdemo()
```
These example files are first downloaded to a temporary folder and then the app is launched.


To run on a folder with your csv files run the following code:
```
library(CSVViewer)
CSVViewer::launch(csvfolder = "path/to/folder")
```

In the folder a comments.json file will be created to store the comments.

All the csv files in this folder and its subfolders will be shown in the app.

if the folder contains a documentation folder and a [documentation.json](https://github.com/EHDEN/CSVViewer/raw/main/inst/extdata/documentation/documentation.json) the documentation panel will show documentation per file in the app.
See the [exampe folder](https://github.com/EHDEN/CSVViewer/raw/main/inst/extdata/) for more details.


# Support

We use the GitHub [issues tracker](https://github.com/EHDEN/CSVViewer/issues) for all bugs/issues/enhancements

# License
CsvViewer is licensed under Apache License 2.0

# Development
CsvViewer is developed in R Studio.

## Development status
CsvViewer is still under development all feedback is welcome in the [issues tracker](https://github.com/EHDEN/CSVViewer/issues).
