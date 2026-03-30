# requirements.R
# Run this file once before running any other scripts
# install.packages("requirements") or run this file directly

required_packages <- c(
  "shiny",
  "dplyr",
  "ggplot2",
  "scales",
  "tm",
  "ggrepel",
  "httr",
  "jsonlite",
  "htmltools",
  "here"
)

cat("Checking and installing required packages...\n\n")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing:", pkg, "\n")
    install.packages(pkg, dependencies = TRUE)
  } else {
    cat("Already installed:", pkg, "\n")
  }
}

cat("\nAll packages are installed and ready!\n")
cat("You can now run the project.\n")