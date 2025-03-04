## script to convert excel file to csv

# Install and load the readxl package if not already installed
if (!requireNamespace("readxl", quietly = TRUE)) {
    install.packages("readxl")
}
library(readxl)

# Define the path to the Excel file
excel_file <- "path/to/your/excel_file.xlsx"

# Read the Excel file into a data frame
excel_data <- read_excel(excel_file)

# Define the path for the CSV file
csv_file <- "path/to/your/output_file.csv"

# Write the data frame to a CSV file
write.csv(excel_data, file = csv_file, row.names = FALSE)

# Print a message indicating successful conversion
cat("Excel file converted to CSV:", csv_file, "\n")
