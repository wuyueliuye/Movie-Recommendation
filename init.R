# init.R
#
# install packages if not already installed
#

my_packages = c("tidyverse", "dplyr", 'lubridate','tibble', 'stringr',
                'stringi', 'tidytext', 'superml', 'reshape2', 'caret',
                'recommenderlab', 'shiny', 'shinythemes', 'DT', 'data.table')


install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
