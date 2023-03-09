
library(tidyverse)

load("./data/us_clean.RData")

# nice correlation matrix 
usw %>%
  select(matches("fimngrs")) %>% # select vars. that have this in name 
  na.omit() %>% # omit missing from data 
  cor() %>% # do correlations 
  round(2) # round to 2 decimal points

# nice correlation matrix 
usw %>%
  select(matches("logincome")) %>% # select vars. that have this in name 
  na.omit() %>% # omit missing from data 
  cor() %>% # do correlations 
  round(2) # round to 2 decimal points


# table in long format tidyverse way 
count(usw, vote6_fct_1, vote6_fct_2) %>%
  mutate(prop = n/sum(n)) %>% 
  print(n = 50) # make sure it prints all rows

# transition matrix R base way
trans_freq <- table(usw$vote6_fct_1, usw$vote6_fct_4, useNA = "always") 
trans_freq

# get row proportions 
prop.table(trans_freq, 1) %>% 
  round(2) # round to make easier to read




set.seed(1234)

# randomly select 20 pidps 
random_people <- unique(usl$pidp) %>% 
  sample(20)


# filter just the 20 people 
susl <- filter(usl, pidp %in% random_people)


(g1 <- ggplot(data = susl,
              aes(x = wave, y = fimngrs, group = pidp)) + 
    geom_line())


# same as 
(g1 <- ggplot(susl, 
              aes(wave, fimngrs, group = pidp)) + 
    geom_line())

(g2 <- g1 + theme_bw())

(g3 <- g2 + labs(y = "Gross income", x = "Wave"))


(g4 <- g3 + facet_wrap(~ pidp, as.table = F) + 
    theme(strip.background = element_blank(), 
          strip.text.x = element_blank()))

g4 + stat_smooth(method = "lm", se = F)


g1 <- ggplot(susl, aes(wave, fimngrs, group = pidp)) + 
  geom_point() + 
  stat_smooth(se = F) + 
  facet_wrap(~ pidp) +
  labs(y = "Gross income", x = "Wave") +
  theme(strip.background = element_blank(), 
        strip.text.x = element_blank()) 
g1

g1 <- ggplot(susl, aes(wave, fimngrs)) +
  geom_point(alpha = .3) + 
  stat_summary(fun = mean, geom = "line", group = 1) + 
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(y = "Gross income", x = "Wave") +
  theme_bw() 
g1

ggplot(susl, aes(wave, fimngrs, color = gndr, group = gndr)) + 
  geom_point(alpha = .3) + 
  stat_summary(fun = mean, geom = "line") + 
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(y = "Gross income", x = "Wave", color = "Gender") +
  theme_bw()


ggplot(susl, aes(wave, fimngrs, color = gndr:urban_fct, 
                 group = gndr:urban_fct)) + 
  geom_point(alpha = .3) + 
  stat_summary(fun = mean, geom = "line") + 
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(y = "Gross income", x = "Wave", color = "Gender x Urban") +
  theme_bw()


ggplot(susl, aes(wave, fimngrs, group = pidp)) +
  geom_line() + 
  stat_summary(aes(group = gndr), fun = mean, geom = "line", lwd = 2) + 
  facet_grid(~gndr) +
  labs(y = "Gross income", x = "Wave") +
  theme_bw()


ggplot(susl, aes(wave, fimngrs, group = pidp)) +
  geom_line() + 
  stat_summary(aes(group = urban_fct:gndr, color = gndr),
               fun = mean, geom = "line", lwd = 2) +
  facet_grid(~urban_fct) +
  labs(y = "Gross income", x = "Wave", color = "Gender") +
  theme_bw()


usl %>%
  filter(!is.na(urban_fct)) %>% 
  ggplot(aes(wave, fimngrs, group = pidp)) + 
  stat_summary(aes(group = urban_fct:gndr, color = gndr),
               fun = mean, geom = "line", lwd = 2) +
  facet_grid(~urban_fct) +
  labs(y = "Gross income", x = "Wave", color = "Gender") +
  theme_bw()
























































































