## Plot which rotation choice is likely to be the thesis lab 

# read in data in order: 2022 (skip b/c incomplete), 2021-2013 
library(readxl)

# read in data from spreadsheet
df_list <- lapply(excel_sheets("/Users/saracarioscia/grad-school/random_scripts/JHU_rotations.xlsx"), function(x)
  read_excel("/Users/saracarioscia/grad-school/random_scripts/JHU_rotations.xlsx", sheet = x)
)

# do the below if you have an excel sheet with multiple pages (tabs) 
# merge each year into one dataframe
df <- data.table::rbindlist(df_list, fill = TRUE)

# FILTERING SPECIFIC TO MY DATA 
# skip any incomplete cohorts - any that don't have data for rotation 2 
df <- df[length(which(is.na(df$`Rotation 2`))):nrow(df),]
# filter to exclude GPP since their rotation counts are different 
df_filtered <- df[!grepl("GPP", df$`Name (Program)`),]
# combine Rotation 4 and Rotation 4 remote 
df_filtered <- df_filtered %>% mutate(`Rotation 4` = coalesce(`Rotation 4`,`Rotation 4 (Remote)`)) 
# remove columns that are just notes, and rotation 4 remote 
df_filtered <- df_filtered[,-c(8:10)]



# get the column number for which the thesis lab matches 
rotation1 <- which(df_filtered$`Rotation 1` == df_filtered$`Thesis Lab`)
rotation2 <- which(df_filtered$`Rotation 2` == df_filtered$`Thesis Lab`)
rotation3 <- which(df_filtered$`Rotation 3` == df_filtered$`Thesis Lab`)
rotation4 <- which(df_filtered$`Rotation 4` == df_filtered$`Thesis Lab`)
rotation5 <- which(df_filtered$`Rotation 5` == df_filtered$`Thesis Lab`)

# dataframe to plot
df_plot <- data.frame(
  rotation = c("Rotation 1", "Rotation 2", "Rotation 3", "Rotation 4", "Rotation 5"), 
  count = c(length(rotation1), length(rotation2), length(rotation3), length(rotation4), length(rotation5))
)

# piechart
ggplot(df_plot, aes(x="", y=count, fill=rotation)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()


# calculate percentages
df_plot_percents <- df_plot %>%
  mutate(rotation = factor(rotation, 
                           levels = rotation[length(rotation):1]),
         cumulative = cumsum(count),
         midpoint = cumulative - count /2,
         labels = paste0(round((count / sum(count)) * 100, 1), "%"))

# plot with percentage labels
ggplot(df_plot_percents, aes(x="", y=count, fill=rotation)) +
  geom_bar(stat="identity", width=1, color="white") + 
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(x = 1.2, y = midpoint, label = labels), color = "black") +
  theme_void()

# of the people who do a 5th rotation, how many pick their 5th rotation lab? 
did_fifth <- df_filtered[!is.na(df_filtered$`Rotation 5`),]
which(did_fifth$`Rotation 5` == did_fifth$`Thesis Lab`)
percent_picked_fifth <- length(which(did_fifth$`Rotation 5` == did_fifth$`Thesis Lab`)) / nrow(did_fifth) # 0.75
  
  

