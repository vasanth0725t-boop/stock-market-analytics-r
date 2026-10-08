required_packages <- c(
  "shiny",
  "bslib",
  "ggplot2",
  "dplyr",
  "plotly",
  "DT",
  "scales",
  "lubridate"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

invisible(lapply(required_packages, install_if_missing))

message("Required R packages checked and installed successfully.")
