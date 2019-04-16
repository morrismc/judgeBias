# This is a script written on January 2, 2019, or started on that date to evaluate if there was
#Gender bias in the approval of obama judges by the Senate. I am specifically curious if the 
#Republicans in taking control of the Senate were sexist in the few judges they approved.

#################################### SECTION TITLE ####################################
library(tidyverse)
library(ggplot2)
# library(xlsx)
library(ggalt)
library(scales)
library(gridExtra)
library(rvest)
library(plyr)
library(stringr)
library(textclean)
library(lubridate)
library(gender)
rm(list = ls())
setwd('/Users/matthew/Documents/GitHub/judgeBias')

#################################### SECTION TITLE ####################################
url <-  'https://ballotpedia.org/Federal_judges_nominated_by_Barack_Obama'
website <- read_html(url)
tbls_ls <- website %>%
  html_nodes("table") %>%
  .[1:2] %>%
  html_table(fill = TRUE)

jData <- tbls_ls[[1]]

rm(tbls_ls, website,url)
#################################### Clean up date ####################################
jData$`First nomination date` <-  parse_date_time(jData$`First nomination date`, orders = c('mdy'))
jData$`Hearing date` <-  parse_date_time(jData$`Hearing date`, orders = c('mdy'))
jData$`Confirmation date` <- parse_date_time(jData$`Confirmation date`, orders = c('mdy'))

# Separate out first names
names <- as.data.frame(as.character(sapply(str_split(jData$Name,' '),'[[',1),stringsAsFactors = FALSE))
names(names) <-  c('First_name')
names$First_name <- as.character(names$First_name)
jNames <- names

# Associate gender with names
library(gender)
jNames %>%
  # rowwise() %>%
  do(results = gender(.$First_name, years = c(1960, 2000),method = "ssa")) %>%
  do(bind_rows(.$results)) -> names1

names1 %>%
  select(name,gender) %>%
  unique() -> names2
names(names2) <- c('First_name','gender')

jNames %>%
  left_join(names2, by = c('First_name')) -> jNames2

jData1 <- cbind(jData,jNames2)

rm(list=setdiff(ls(), "jData1"))

jData1 <- jData1[!is.na(jData1$gender),]
jData1 <- jData1[jData1$Name != 'Robert Chatigny']

obama <- interval(ymd(20090101),ymd(20170101))
jData1 <- jData1[jData1$`First nomination date` %within% obama,]

a <- as.data.frame(logical(length = dim(jData1)[1]))

for(i in 1:dim(jData1)[1]){
  a[i,1] <- !is.na(jData1[i,6])
  
}

b <- as.data.frame(rep(0,(dim(jData1)[1])))
for(i in 1:dim(a)[1]){
  if(a[i,1] == TRUE){
    b[i,1] <- 1
  }
  else {
    b[i,1] <- 0
  }
}
# b <- as.data.frame(b)
# a <- as.data.frame(a) 

names(b) <- c('Conf_Status')
jData1 <- cbind(jData1,b)

jData1 <- cbind(jData1,year(jData1$`First nomination date`))
colnames(jData1)[11] <- c('year')
jData1 <- jData1[!is.na(jData1$gender),]

jData1 <- cbind(jData1,jData1$`Confirmation date` - jData1$`First nomination date`)
colnames(jData1)[12] <- c('Days_2_conf')
#################################### histogram of nominations by Gender ####################################

jData1 %>%
  group_by(gender) %>%
  summarise(gender) %>%
  count() %>%
  mutate(percent = freq/(sum(freq))) %>%
  ggplot(aes(x = gender, y = percent*100))+
  geom_col()+
  theme_light()+
  scale_y_continuous(breaks = c(0,10,20,40,50,60))+
  labs(x = 'Gender',y = 'Percent')

#################################### Figure out how was confirmed ####################################


#################################### plot of confirmed ####################################

jData1 %>%
  ggplot(aes(x = gender, group = Conf_Status, fill = Conf_Status))+
  geom_bar(position = 'dodge')+
  labs(x = 'Gender',y = 'Count',fill = 'Confirmed?',title = 'Judges appointed by Obama')+
  facet_wrap(~year)

#################################### SECTION TITLE ####################################

jData1 %>% 
  group_by(gender,Conf_Status, add = TRUE) %>% 
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(freq = n/sum(n))%>%

    ggplot(aes(x = gender,
               y = freq*100,
               group = Conf_Status,
               fill= Conf_Status))+
    geom_col(position = 'dodge')+
  labs(x = 'Gender',y = 'Percentage')
  #################################### SECTION TITLE ####################################

jData1 %>%
  filter(.,Conf_Status == TRUE)%>%
  group_by(gender)%>%
  summarize(n = mean(Days_2_conf))
#################################### SECTION TITLE ####################################
## 2017-01-01 UTC--2017-11-28 UTC
jData1 %>%
  ggplot(aes(y = `First nomination date`,x = gender,color= Conf_Status))+
  geom_jitter()+
  theme_classic()+
  geom_hline(aes(yintercept = 2015))+
  labs(x = 'Gender',y = 'Count',color = 'Confirmed?',title = 'Judicial nominations made by Obama')

#################################### Number not approved ####################################
detach("package:plyr", unload=TRUE) 

jData1 %>%
  filter(.,Conf_Status == FALSE)%>%
  group_by(year,gender) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = year,
             y = n,
             color = gender))+
  geom_line(size = 0.1)+
  geom_point()+
  # geom_smooth()+
  theme_light()+
  geom_vline(aes(xintercept = 2015))+
  labs(x = 'Year',
       y = 'Number',
       color = 'Gender',
       title = 'Number of judicial nominees not confirmed')

#################################### number approved ####################################

jData1 %>%
  filter(.,Conf_Status == TRUE)%>%
  group_by(year,gender) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = year,
             y = n,
             color = gender))+
  geom_line()
#################################### Days 2 conf ####################################
jData1 %>%
  filter(.,Conf_Status == TRUE) %>%
  
  ggplot(aes(x = Days_2_conf,color=gender))+
  geom_density()+
  labs(x = 'Days until Judicial Confirmation',
       y = 'Probability',
       color = 'Gender',
       title = 'Days until Obama Judicial Nominee is confirmed')+
  theme_light()+
  facet_wrap(~year)

#################################### Days 2 conf box pot ####################################
jData1 %>%
  filter(.,Conf_Status == TRUE) %>%
  
  ggplot(aes(y = Days_2_conf,
             x = gender,
             fill = factor(gender)
             ))+
  # geom_density()+
  geom_boxplot(notch = 'true')+
  labs(y = 'Days until Judicial Confirmation',
        x = 'Gender',
       fill = 'Gender',
       title = 'Days until Obama Judicial Nominee is confirmed')+
  theme_light()+
  facet_wrap(~year)+
  coord_flip()
#################################### SECTION TITLE ####################################
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

jData1 %>%
  filter(.,Conf_Status == TRUE) %>%
  
  ggplot(aes(y = Days_2_conf,
             x = gender,
             fill = factor(gender)
  ))+
  stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") +
  facet_wrap(~year)
#################################### SECTION TITLE ####################################
jData1 %>%
  group_by(gender,year)%>%
  summarize(n = n()) %>%
  
  #################################### RStan ####################################

library(rstanarm)
library(bayesplot)
library(rstan)
library(arm)

mf <- formula(Conf_Status ~ year + gender)

fit <- stan_glm(mf, 
                data = jData1, 
                family = binomial(link = 'logit'),
                warmup = 2000,
                iter = 2000)

plot(fit, plotfun = "areas", prob = 0.6, # ?bayesplot::mcmc_areas
     pars = c("(Intercept)"))

mcmc_areas(as.matrix(fit),
           pars = c('year','gendermale'),
           prob = 0.9)
pp_check(fit, plotfun = "error_binned")

stan_plot(fit, pars = c('year','gendermale'))
summary(fit)
           
#################################### SECTION TITLE ####################################
library(shinystan)
samples <- fit %>% as.data.frame %>% tbl_df
samples

ggplot(samples) + aes(x = year) + geom_histogram()
posterior_interval(fit)

df_model <- fit %>% as_data_frame()
df_model

#################################### SECTION TITLE ####################################
fits <- sample_n(df_model, 200)
medians <- df_model %>% summarise_each(funs = funs(median))


p2 <- ggplot(data = jData1, aes(x =year, y = Conf_Status)) +
  # geom_point()+
  geom_abline(aes(color = "R", intercept = fits$`(Intercept)`,
                  slope = fits$year), data = fits, alpha = .075) +
  geom_abline(aes(color = "M", intercept = medians$`(Intercept)`,
                  slope = medians$year), data = medians, alpha = .075) 

  geom_abline(aes(color = "F", intercept = F_Intercept,
                  slope = F_Slope), data = medians, size = 1.25) +
  geom_abline(aes(color = "M", intercept = M_Intercept,
                  slope = M_Slope), data = medians, size = 1.25) +
  geom_point() +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))




#################################### post plot of lines ####################################
fits <- fit %>%
  as_data_frame() %>%
  rename(intercept = intercept) %>%
  select(-sigma)


#################################### SECTION TITLE ####################################

jData1 %>%
  ggplot(aes(x = `First nomination date`, y = Conf_Status,
             group = gender, color = gender))+
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE)+
  theme_light()+
  geom_vline(aes(xintercept = 2015))+
  labs(x = 'First nomination Date',y = 'Probability of Confirmation', color = 'Gender')

#################################### SECTION TITLE ####################################
