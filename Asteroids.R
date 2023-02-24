library(tidyverse)


dl <- "dataset.csv"
if(!file.exists(dl))
  download.file("https://gitlab.com/mirsakhawathossain/pha-ml/-/raw/master/Dataset/dataset.csv", dl)

readfile <- read.csv("dataset.csv")

df <- readfile |> filter(pha != "" & neo != "")

df |> filter(pha == "Y") |> 
      group_by(class) |>
      summarise(n())
#1 AMO     118
#2 APO    1768
#3 ATE     174
#4 IEO       6

#we see that APO has the most Y ones

df |> filter(class %in% c("AMO","APO","ATE")) |> nrow()
#22872 looks good

df_asteroids <- df |> filter(class %in% c("AMO","APO","ATE")) |> filter(class %in% c("AMO","APO","ATE"))
