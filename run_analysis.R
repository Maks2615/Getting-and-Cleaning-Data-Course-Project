#If you haven't installed tidyverse package then uncomment string below

#install.packages('tidyverse')
library(tidyverse)

#create a directory for the unzipped data
ifelse(!dir.exists('Week4_Assignment'), 
                   dir.create('Week4_Assignment'),
       'Directory already exists')

#this url link for the zip archive
url_zip <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'

#storing the archive in a temporary file
f <- tempfile()
download.file(url = url_zip, destfile = f)
unzip(f, exdir = 'Week4_Assignment/.')
file.remove(f)

#Some helping path variables for reading files
dir_path <- './Week4_Assignment/UCI HAR Dataset/' 
test <- 'test/'
train <- 'train/'

#Reading files from our test and train folders
X_test <- read_table(paste0(dir_path, test,'X_test.txt'), col_names = F)
y_test <- read_table(paste0(dir_path, test,'y_test.txt'), col_names = F)
subjects_test <- read_table(paste0(dir_path, test,'subject_test.txt'), col_names = F) %>% setNames('subjects')

X_train <- read_table(paste0(dir_path, train,'X_train.txt'), col_names = F)
y_train <- read_table(paste0(dir_path, train,'y_train.txt'), col_names = F)
subjects_train <- read_table(paste0(dir_path, train,'subject_train.txt'), col_names = F) %>% setNames('subjects')

features <- read_delim(paste0(dir_path, 'features.txt'), delim = ' ', col_names = F)
y_decoding <- read_table(paste0(dir_path, 'activity_labels.txt'), col_names = F)

activity_test <- y_test %>% right_join(y_decoding) %>% setNames(.,c('activity_code','activity'))
activity_train <- y_train %>% right_join(y_decoding) %>% setNames(.,c('activity_code','activity'))

#So, let's set variable names for our files
names(X_test) <- features$X2
names(X_train) <- features$X2
X_test <- bind_cols(subjects_test, activity_test, X_test)
X_train <- bind_cols(subjects_train, activity_train, X_train)

#Merge X's files to one dataset
X <- bind_rows(X_train, X_test)

#Lets remove unnecessary files - we need only our dataset and path to folder
rm(list = setdiff(ls(),c('X','dir_path')))

#We need subset only mean() and std() variables - let's subset with regex
X <- X[,c(1,3,str_which(X %>% names, '(.*mean.*)|(.*std.*)', negate = F))]

# Let's create tidy dataset - 2 options
X_tidy1 <- X %>% 
  pivot_longer(cols = 3:81,
               names_to = 'Variable_name',
               values_to ='Values')%>% 
  write_delim(file.path(dir_path,'X_tidy1.txt'), delim = ',', 
              col_names =  T)

X_tidy2 <- X %>% 
  pivot_longer(cols = -c(1,2),
               names_to = c('Mean','Std'),
               names_pattern= '(.*mean.*)|(.*std.*)',
               values_to ='Values')%>% 
  write_delim(file.path(dir_path,'X_tidy2.txt'), delim = ',', 
              col_names =  T)

# New reauested dataset with averages - again 2 options

X_requested1 <- X_tidy2 %>%
  mutate(Flag = case_when(is.na(Mean) ~ 'For_Std',
                          T~ 'For_Mean')) %>% 
  group_by(activity, Flag) %>% 
  summarise(Average =mean(Values),.groups = 'drop') %>% 
  arrange(Flag) %>% 
  pivot_wider(names_from = activity, values_from = Average)%>% 
  write_delim(file.path(dir_path,'X_requested1.txt'), delim = ',', 
              col_names =  T)


X_requested2 <- X %>% group_by(subjects,activity) %>% 
  summarise(across(everything(), mean), .groups ='drop') %>% 
  arrange(subjects, activity) %>% 
  write_delim(file.path(dir_path,'X_requested2.txt'), delim = ',', 
              col_names =  T)


