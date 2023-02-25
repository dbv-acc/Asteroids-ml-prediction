#### 1. LOAD DATA

##Set digits
options(digits=4)
###Loading required libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(stats)) install.packages("stats", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

#Pulling Asteroid Dataset from Kaggle
dl <- "dataset.csv"
if(!file.exists(dl))
  download.file("https://gitlab.com/mirsakhawathossain/pha-ml/-/raw/master/Dataset/dataset.csv", dl)
readfile <- read.csv("dataset.csv")

#count of rows
nrow(readfile)
#count of columns
ncol(readfile)

###DATA WRANGLING

#Large dataset decide what to try to evaluate - potential hazardous asteroid PHA or Near-Earth Object (NEO)
#these are the only 2 columns with flags
df <- readfile |> filter(pha != "" & neo != "") #remove blanks

df |> select(pha,neo) |> table() |> data.frame() |> kable() #explore pha, neo relationship
  
#we see that all PHA Y are also Y for NEO whereas there are some no PHA but are Yes NEO. We will focus on PHA as it seems to be 
#more hazardous hence the name, rather than simply a near earth object
#we see that there is a class of asteroids. lets see which ones appear for PHA Y.
df |> filter(pha == "Y" | neo == "Y") |>
  group_by(class,pha) |>
  summarise(n=n()) |> 
  pivot_wider(class,names_from=pha,values_from = n) |> 
  mutate(prop_hazard=percent(Y/(N+Y))) |> 
  ggplot(aes(x=class,y=prop_hazard))+
  geom_col()
  
#we see the classes below exist for those containing a flag. seems to support the description
#Apollos (APO) cross Earth's orbit and have a semi-major axis of more than 1 AU
#Amors (AMO) have orbits strictly outside Earth's orbit
#Atens (ATE) cross Earth's orbit and have a semi-major axis of less than 1 AU
#Atiras (IEO) have orbits strictly inside Earth's orbit
  
#For this reason it would be interesting to see that why some asteroids in these classes are not hazardous

  
#check if duplicates in spkid. just to make sure every row represents a unique asteroid
df_distinct <- df |> distinct(spkid) |> nrow()
df_distinct - nrow(df)

#now lets look to see if there are any NAs
df_na <- df |>
  select(everything()) |>  
  summarise_all(list(~sum(is.na(.))))

empty_columns <- sapply(df_na, function(x) all(x <= 1 ))
kable(df_na[, !empty_columns])

#we wouldn't like to elimate all the na as it is 85% of whole dataset. lets take a closer look
percent(802390/nrow(df))

###max 939.4 and median is 4. i wonder if 939 are outliers. lets look at double the 3rd quarter
df |> drop_na(diameter) |> select(diameter) |>  summary()

###histogram shows a nice poisson distribution however qqplot doesn't seem to support it being a poisson distribution 
p1 <- df |> drop_na(diameter) |> filter(diameter<20) |>  ggplot(aes(x=diameter))+
  geom_histogram()


p2 <- df |> drop_na(diameter) |> filter(diameter<20) |> ggplot(aes(sample = diameter)) + 
  stat_qq(distribution = qpois, dparams = list(lambda=4)) + 
  geom_abline(intercept = 0, slope = 1)

grid.arrange(p1, p2, ncol = 2)



####seems to show that diameter does have an impact on PHA. Maybe <10 would have been a better threshold
df |> drop_na(diameter) |>
  filter(diameter<30) |>
  ggplot(aes(y=diameter, x=pha))+
  geom_boxplot()

###albedo doesn't have too much
df |> drop_na(albedo) |>
  ggplot(aes(y=albedo, x=pha))+
  geom_boxplot()

#Given that diameter could have explanatory power but we want to have a large dataset, We will construction models across 2 data sets, w/diamter and wo/



#PRIMARY MODEL first without diamater

#diameter
#Keep the classes "AMO","APO","ATE","IEO"
#Remove various names of the asteroid. we can pull this back in later
#Also remove Epoch variables as it relates to position of satilite
#also equinox as dont care about reference frame
#delete all sgima as it measures errors


df_ast_big <- df |> filter(class %in% c("AMO","APO","ATE","IEO")) |> #Filter classes we want to evaluate
  select(-diameter,-albedo, -neo) |>  #remove diameter and albedo in first models to keep 100% of rows and remove neo as we are no testing for it
  select(-class,-id,-full_name,-pdes,-name, -prefix,-orbit_id) |>  #remove names of the asteroid its orbit
  select(-equinox) |> #remove equinox as it by definition has no impact
  select(-contains("sigma")) |> #these are the error values, we will ignore
  na.omit() #remove NAs


##Create partition
df_ast_big$pha <-  as.factor(df_ast_big$pha)
test_index <- createDataPartition(df_ast_big$spkid, times = 1, p = 0.2, list = FALSE)
test_set <- df_ast_big[test_index, ]
train_set <- df_ast_big[-test_index, ]

###try GLM, see how well it does
train_glm <- train(pha ~ ., method = "glm", data = train_set)
y_hat_glm <- predict(train_glm, test_set, type = "raw")
mod_glm_bg <- confusionMatrix(y_hat_glm, test_set$pha)$overall[["Accuracy"]]
accuracy_table <- data_frame(Model = "Primary",Method = "GLM", Accuracy = mod_glm_bg)
accuracy_table |> knitr::kable()

###train KNN. It will take a while to run. you can skip this seection if you like
train_knn <- train(pha ~ ., method = "knn", data = train_set)
y_hat_knn <- predict(train_knn, test_set, type = "raw")
mod_knn_bg <- confusionMatrix(y_hat_knn, test_set$pha)$overall[["Accuracy"]]

##KNN best is 9
train_knn$bestTune

accuracy_table <- bind_rows(accuracy_table,
                          data_frame(Model = "Primary",Method = "KNN", Accuracy = mod_knn_bg))
accuracy_table |> knitr::kable()

#decision tree
train_rpart <- train(pha ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = train_set)



mod_rpart_bg <- confusionMatrix(predict(train_rpart, test_set), test_set$pha)$overall["Accuracy"]



accuracy_table <- bind_rows(accuracy_table,
                            data_frame(Model = "Primary",Method = "RPART", Accuracy = mod_rpart_bg))
accuracy_table |> knitr::kable()


##you can see immediately we hit high accuracy
plot(train_rpart)

# view the final decision tree
plot(train_rpart$finalModel, margin = 0.1) # plot tree structure
text(train_rpart$finalModel) # add text labels



###seems like H and moid is determining, lets see if we plot against these 2 and color by PHA
df_ast_big |>
  ggplot(aes(H, moid, color=pha)) +
  geom_point()

##ok it seems clear there is a definite relationship try 22 and 0.5 as the rpart plot shows this
#instead of test_set lets look at entire set of 938599 rows
predict_simple <- df |> mutate(predict_val= factor(ifelse(H<22 & moid<0.05,"Y","N")))
mod_simple_bg <- mean(predict_simple$pha==predict_simple$predict_val)

accuracy_table <- bind_rows(accuracy_table,
                            data_frame(Model = "Primary",Method = "Simple", Accuracy = mod_simple_bg))
accuracy_table |> knitr::kable()


##strange that it does not show 100% accuracy. lets see where the outliers are
outliers <- rbind(df |> filter(H<22 & moid<0.05 & pha=="N") |> select(pha,H,moid), df |> filter(H>=22 & pha=="Y") |> select(pha,H,moid)) |> arrange(desc(H))
outliers
nrow(outliers)
nrow(outliers)/nrow(df) #very very few outliers so assumption holds.


## SECONDARY MODEL
#as we saw it seems like H and moid defines pha so lets try to see without this rule if we can determine pha.
# what if we look at other variables? We will now include diameter and exclude the true explanatory variables H and moid.

df_ast_small <- df |> filter(class %in% c("AMO","APO","ATE","IEO")) |> #Filter classes we want to evaluate
  select(-H, -moid,-moid_ld,-neo) |>  #H and moid as we previously described. remove neo as we are no testing for it
  select(-spkid,-class,-id,-full_name,-pdes,-name, -prefix,-orbit_id) |>  #remove names of the asteroid its orbit
  select(-equinox) |> #remove equinox as it by definition has no impact
  select(-contains("sigma")) |> #these are the error values, we will ignore
  na.omit() #remove NAs

##create partition
set.seed(1)
df_ast_small$pha <-  as.factor(df_ast_small$pha)
test_index <- createDataPartition(df_ast_small$q, times = 1, p = 0.2, list = FALSE)
test_set <- df_ast_small[test_index, ]
train_set <- df_ast_small[-test_index, ]

##GLM
train_glm <- train(pha ~ ., method = "glm", data = train_set)
y_hat_glm <- predict(train_glm, test_set, type = "raw")
mod_glm_sm <- confusionMatrix(y_hat_glm, test_set$pha)$overall[["Accuracy"]]
accuracy_table <- bind_rows(accuracy_table,
                            data_frame(Model = "Secondary",Method = "GLM", Accuracy = mod_glm_sm))
accuracy_table |> knitr::kable()

##KNN
train_knn <- train(pha ~ ., method = "knn", data = train_set)
y_hat_knn <- predict(train_knn, test_set, type = "raw")
mod_knn_sm <- confusionMatrix(y_hat_knn, test_set$pha)$overall[["Accuracy"]]

##KNN best is 9
train_knn$bestTune

accuracy_table <- bind_rows(accuracy_table,
                            data_frame(Model = "Secondary",Method = "KNN", Accuracy = mod_knn_sm))
accuracy_table |> knitr::kable()

#decision tree

train_rpart <- train(pha ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = train_set)

##now we see how we achieve less accuracy with H and MOID
plot(train_rpart)

mod_rpart_sm <- confusionMatrix(predict(train_rpart, test_set), test_set$pha)$overall["Accuracy"]

# view the final decision tree. (note:will not run if just a root!)
plot(train_rpart$finalModel, margin = 0.1) # plot tree structure
text(train_rpart$finalModel) # add text labels

accuracy_table <- bind_rows(accuracy_table,
                            data_frame(Model = "Secondary",Method = "RPART", Accuracy = mod_rpart_sm))
accuracy_table |> knitr::kable()


####extra notes below which were not in final project

#most orbits are have no PHA asteorids or all Asteroids however there are some inbetween so this is not a pure reason. could also show that 
df |> group_by(orbit_id) |> summarise("PHA"=mean(pha=="Y")) |> arrange(desc(PHA)) |> ggplot(aes(PHA))+
  geom_histogram()


###however this shows that most have unique orbit
right_jn<- df |> group_by(orbit_id) |> summarise("PHA"=mean(pha=="Y")) |> filter(PHA==1)
inner_join(df,right_jn,by="orbit_id") |> ggplot(aes(x=orbit_id))+
  geom_bar()

