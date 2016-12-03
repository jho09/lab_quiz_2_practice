library(tidyverse)
library(apaTables)
library(haven)

bfi_data <- psych::bfi

raw_data <- bfi_data

categorical_variables <- select(raw_data,gender)

categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Males"=1,"Females"=2)

age <- select(raw_data,age)

education <- select(raw_data,education)

agreeableness_items <- select(raw_data,A1,A2,A3,A4,A5)

extraversion_items <- select(raw_data,E1,E2,E3,E4,E5)

neuroticism_items <- select(raw_data,N1,N2,N3,N4,N5)

psych::describe(agreeableness_items)
psych::describe(extraversion_items)
psych::describe(neuroticism_items)

agreeableness_items <- mutate(agreeableness_items,A1=7-A1)
extraversion_items <- mutate(extraversion_items,E1=7-E1)
extraversion_items <- mutate(extraversion_items,E2=7-E2)

agreeableness <- psych::alpha(as.data.frame(agreeableness_items),check.keys = FALSE)$scores
extraversion <- psych::alpha(as.data.frame(extraversion_items),check.keys = FALSE)$scores
neuroticism <- psych::alpha(as.data.frame(neuroticism_items),check.keys = FALSE)$scores

analytic_data <- cbind(agreeableness,extraversion,neuroticism,categorical_variables,education,age)

save(analytic_data,file="analytic_data.RData")
write_csv(analytic_data,path = "analytic_data.csv")
write_sav(analytic_data,path="analytic_data.sav")

# Creating analytic data set excluding "gender"
analytic_data_excluding_gender <- analytic_data %>% select(-gender)
View(analytic_data_excluding_gender)

apa.cor.table(analytic_data_excluding_gender,filename="Table1.doc",table.number = 1)

# Creating analytic data set based just on men over the age of 40

analytic_data_men_over_age_40 <- filter(analytic_data,gender=="Males" & age >40) 

apa.cor.table(analytic_data_men_over_age_40,filename="Table2.doc",table.number = 2)
              
# Creating scatter plot for agreeableness vs extraversion for men over age 40
my.plot <- qplot(agreeableness, extraversion, data = analytic_data_men_over_age_40)
my.plot <- my.plot + theme_classic(14)
my.plot <- my.plot + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),axis.line.y = element_line(colour = 'black', size=0.5, linetype = 'solid'))
my.plot <- my.plot + labs(title="Men over the age of 40", x="agreeableness", y="extraversion")
ggsave("Figure1.pdf",plot = my.plot,width=6,height = 6)
