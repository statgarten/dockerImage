# List of packages to install
packages <- c("shiny", "data.table", "readxl", "DT", "jstable", 
              "shinycustomloader", "tableone", "labelled", "markdown", 
              "ggplot2", "GGally", "jsmodule", "survC1", "rhandsontable", 
              "magrittr", "meta", "colourpicker", "shinythemes", 
              "survey", "haven", "MatchIt", "jskm")

# Installing packages
for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
    }
}
