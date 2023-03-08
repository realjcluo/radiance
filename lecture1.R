

# install packages 

install.packages("tidyverse") 
install.packages("haven")

# load packages 
library(tidyverse) 
library(haven)

# set working directory 
getwd() # change this to fit your computer

# make object with the variable names we want 
vars1 <- c("pidp", "a_sex", "a_dvage", "a_hiqual_dv", "a_single_dv", 
           "a_urban_dv", "a_vote6", "a_fimngrs_dv", "a_sclfsato", 
           "a_sf12pcs_dv", "a_sf12mcs_dv", "a_istrtdaty" )

# import data using relative link and select some variables 
us1 <- read_dta("./data/a_indresp.dta", col_select = vars1)

head(us1) # see first few cases and variables 
glimpse(us1) # another description of the data

# make object with the variable names we want 
vars2 <- c("pidp", "b_single_dv", "b_urban_dv", 
           "b_vote6", "b_fimngrs_dv", "b_sclfsato", 
           "b_sf12pcs_dv", "b_sf12mcs_dv", "b_istrtdaty" )

# import data using relative link and select some variables 
us2 <- read_dta("./data/b_indresp.dta", col_select = vars2)

# make object with the variable names we want 
vars3 <- c("pidp", "c_single_dv", "c_urban_dv", 
           "c_vote6", "c_fimngrs_dv", "c_sclfsato", 
           "c_sf12pcs_dv", "c_sf12mcs_dv", "c_istrtdaty" )

# import data using relative link and select some variables 
us3 <- read_dta("./data/c_indresp.dta", col_select = vars3)


# make object with the variable names we want 
vars4 <- c("pidp", "d_single_dv", "d_urban_dv", 
           "d_vote6", "d_fimngrs_dv", "d_sclfsato", 
           "d_sf12pcs_dv", "d_sf12mcs_dv", "d_istrtdaty" )

# import data using relative link and select some variables 
us4 <- read_dta("./data/d_indresp.dta", col_select = vars4)


# get the names of the variables in us1 
names(us1)

# rename just one variable 
us1 <- rename(us1, a_age = a_dvage)

# check result 
names(us1)

# get the names of the variables 
names(us1)

# rename multiple variables 
us1 <- rename_all(us1, ~str_remove(., "a_")) 
us1 <- rename_all(us1, ~str_remove(., "_dv"))

us2 <- rename_all(us2, ~str_remove(., "b_")) 
us2 <- rename_all(us2, ~str_remove(., "_dv"))

us3 <- rename_all(us3, ~str_remove(., "c_")) 
us3 <- rename_all(us3, ~str_remove(., "_dv"))

us4 <- rename_all(us4, ~str_remove(., "d_")) 
us4 <- rename_all(us4, ~str_remove(., "_dv"))


# check results 
names(us1)


# check current names 
glimpse(us1)

# rename time varying variables 
us1 <- rename_at(us1, vars(-pidp, -sex, -age, -hiqual), ~str_c(., "_1")) 
us2 <- rename_at(us2, vars(-pidp), ~str_c(., "_2")) 
us3 <- rename_at(us3, vars(-pidp), ~str_c(., "_3")) 
us4 <- rename_at(us4, vars(-pidp), ~str_c(., "_4")) 



# inspect the results 
glimpse(us1)

# make a variable that gets value 1 
us1 <- mutate(us1, present_1 = 1)
glimpse(us1) # investigate result

us2 <- mutate(us2, present_2 = 1)
glimpse(us1) # investigate result

us3 <- mutate(us3, present_3 = 1)
glimpse(us1) # investigate result

us4 <- mutate(us4, present_4 = 1)
glimpse(us1) # investigate result

# merge waves 1 and 2 based on pidp 
us12 <- full_join(us1, us2, by = "pidp")
# look at the data 
glimpse(us12)

us123 <- full_join(us12, us3, by = "pidp")
us1234 <- full_join(us123, us4, by = "pidp")
glimpse(us1234)


# missing patterns waves 1 and 2 
count(us1234, present_1, present_2, present_3, present_4)

# keep balanced panel 
us <- filter(us1234, present_1 == 1, present_2 == 1, present_3 == 1, present_4 == 1)

# look at result 
glimpse(us)

# eliminate all variables starting with "present" 
us <- select(us, -starts_with("present"))

# check the result 
glimpse(us)

usl <- pivot_longer(us, 
                    cols = !c(pidp, sex, age, hiqual), names_sep = "_",
                    names_to = c(".value", "wave"))

# check result 
glimpse(usl)

# put pidp and wave at start of the dataset 
usl <- select(usl, pidp, wave, everything())

# arrange rows by individual id and wave 
usl <- arrange(usl, pidp, wave)

# check result 
glimpse(usl)

# check if it's balanced 
count(usl, wave)

# describe sex 
count(usl, sex) 
attributes(usl$sex)

# describe age 
count(usl, age) 
attributes(usl$age)

# table single 
count(usl, single) 
attributes(usl$single)

# see labels 
attributes(usl$sex)

# make new variable as factor 
usl <- mutate(usl, gndr = factor(sex, labels = c("Male", "Female")))

# check if the new variable is correct 
count(usl, sex, gndr)

attributes(usl$single)
usl <- mutate(usl, single_fct = factor(single, labels = c("In relationship", "Single")))

# look at original variable 
count(usl, urban) 
attributes(usl$urban)

# make new variable that is factor 
usl <- mutate(usl, 
              urban_fct = case_when(urban == 1 ~ "Urban", 
                                    urban == 2 ~ "Rural"),
              urban_fct = as.factor(urban_fct))

# check if it worked 
count(usl, urban, urban_fct)

# look at original variable 
count(usl, hiqual) 
attributes(usl$hiqual)

# make new variable that is factor 
usl <- mutate(usl,
              degree = case_when(hiqual %in% 1:2 ~ "Degree", 
                                 hiqual %in% 3:9 ~ "No degree"),
              degree = as.factor(degree))

# check if it worked 
count(usl, hiqual, degree)


count(usl, degree)


# code missing and make politics factor variable 
usl <- mutate(usl, vote6 = ifelse(vote6 < 0, NA, vote6), 
              vote6_fct = factor(vote6,
                                 labels = c("Very", "Fairly",
                                            "Not very", "Not at all")))

# check result 
count(usl, vote6, vote6_fct)


hist(usl$fimngrs)

# cap income to 0 
usl <- mutate(usl, 
              fimngrs = ifelse(fimngrs < 0, 0, fimngrs))

usl <- mutate(usl, 
              fimngrs = ifelse(fimngrs > 10000, 10000, fimngrs))

# check the result 
hist(usl$fimngrs)

hist(usl$fimngrs) # histogram of income 
hist(log(usl$fimngrs + 10)) # histogram of log income

usl <- mutate(usl, logincome = log(fimngrs + 10))

qplot(usl$sf12pcs)
qplot(usl$sf12mcs)

summary(select(usl, sf12pcs, sf12mcs))

# code all values bellow 0 as missing for 
# variables that have "sf12" in name 
usl <- mutate_at(usl, 
                 vars(matches("sf12")),
                 ~ifelse(. < 0, NA, .))

# check if coding worked 
summary(select(usl, sf12pcs, sf12mcs))


summary(usl)

# command using a pipe 
usl %>%
  mutate(famale = sex - 1) %>% 
  count(famale, gndr)

# the above is the same as:

count(mutate(usl, female = sex - 1), female, gndr)
















































































































































































































