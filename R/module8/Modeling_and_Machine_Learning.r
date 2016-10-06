
# essentials
library(dplyr)
library(ggplot2)

inpatient <- readRDS("small_data/Medicare_Provider_Charge_Inpatient_DRGALL_FY2014_clean.RDS")
cardiac <- readRDS("small_data/inpatient_charges_2014_clean_cardiac_50plus.RDS")

library(caret)

myvars <- c("Average.Total.Payments", "Provider.State", "Total.Discharges")
df <- cardiac %>% select_(.dots = myvars) %>% filter(Provider.State %in% c("DC", "MD", "VA"))

df %>% dim

## it uses Y to preserve class distributions
split <- createDataPartition(y=df$Average.Total.Payments,
                             p=0.80,
                             list=FALSE)

train <- df[split,]
test <- df[-split,]

?createDataPartition

train %>% dim

# the pre-processing 'method'...
process <- preProcess(train, method = c("center", "scale"))
process

# ... and the returned dataset
train_processed <- predict(process, train)
train_processed %>% head

# exercise: create a summary table showing that the pre-processing worked

folds  <- createFolds(y=df$Average.Total.Payments, k=5)

vignette("caret")

# create new data frame with dummy variables
df_dummies <- dummyVars(" ~ .", data=df, fullRank = TRUE) %>%
    predict(., newdata = df)

df_dummies %>% head

split <- createDataPartition(y=df$Average.Total.Payments,
                             p=0.80,
                             list=FALSE)

train <- df_dummies[split,]
test <- df_dummies[-split,]

#install.packages('elasticnet')
library('elasticnet')
set.seed(1)
model <- train(Average.Total.Payments ~ ., data=train,
               method="ridge",
               tuneLength=10     # evaluate 10 parameter (lambda) values
              )

# note 2 predictors (excludes the categorical variable). see below for details
model

plot(model)

model$results

predict(model, newdata=head(test))

# passed to train() function below
# method="cv" does not repeat;
# other options: "boot", "LOOCV", ... See: https://topepo.github.io/caret/model-training-and-tuning.html#the-traincontrol-function
sampling  <- trainControl(method = "repeatedcv",
                          p = 0.75,
                          number=10,
                          repeats=10)

# now, pass this to train()...

set.seed(1)
model <- train(Average.Total.Payments ~ ., data=train,
               method="ridge",
               trControl = sampling,
               tuneLength = 5
               )

model$finalModel$lambda

model$results

# can specify metric
#plot(model, metric="Rsquared")
 ggplot(model, metric="Rsquared")

# and plot the lambda values
plot(model, metric="lambda")

getModelInfo

library(broom)

## You can tidy up lm output with tidy()
cardiac %>%
    group_by(DRG.code) %>%
    do(
        tidy(lm(Average.Total.Payments ~ Total.Discharges, data=.))
    ) %>%
    head()

## You can review model fit with glance()
cardiac %>%
    group_by(DRG.code) %>%
    do(
        glance(lm(Average.Total.Payments ~ Total.Discharges, data=.))
    ) %>%
    arrange(desc(adj.r.squared)) %>%
    head()

## prepare data
myvars <- c("Provider.Id", "DRG.code", "Average.Medicare.Payments",
            "Average.Covered.Charges", "Average.Total.Payments",
            "Total.Discharges", "Provider.State")
df <- cardiac %>% select_(.dots=myvars)

## standard lm
lm(Average.Total.Payments ~ Total.Discharges, data=df)

## lm summary()
lm(Average.Total.Payments ~ Total.Discharges, data=df) %>% summary

## tidy() results
lm(Average.Total.Payments ~ Total.Discharges, data=df) %>% tidy

## use glance() to see fit statistics (1 row/model)
lm(Average.Total.Payments ~ Total.Discharges, data=df) %>% summary %>% glance

## augment() results to see predictions, clusters, etc
lm(Average.Total.Payments ~ Total.Discharges, data=df) %>% augment %>% head

df %>%
    group_by(DRG.code) %>%
    do(
        tidy(lm(Average.Total.Payments ~ Total.Discharges, data=.))
    ) %>%
    head

## rewrite it a bit. note placement of %>%
df %>%
    group_by(DRG.code) %>%
    do(
        lm(Average.Total.Payments ~ Total.Discharges, data=.)
        %>%  tidy
    ) %>%
    head

## augment it with more info
df %>%
    group_by(DRG.code) %>%
    do(
        lm(Average.Total.Payments ~ Total.Discharges, data=.)
        %>% augment %>% tidy
    ) %>%
    head

## example: correlations
df %>%
    group_by(DRG.code) %>%
    do(
        cor.test(.$Average.Total.Payments, .$Average.Medicare.Payments) %>% tidy 
    )  %>%
    head

# assign lm output to new variable `fit`
regressions <- df %>% group_by(DRG.code) %>%
    do(fit = lm(Average.Medicare.Payments ~ Total.Discharges, .))

# ugly output in jupyter. models are saved inside data frame celss
# regressions[1,]

# tidy it up:
regressions %>% tidy(fit) %>% head

df2 <- df %>% filter(Provider.State %in% c("DC", "MD", "VA"))

result <- df2 %>%
    group_by(DRG.code) %>%
    do(
        fit=lm(Average.Total.Payments ~ Total.Discharges + factor(Provider.State), data=.)
    )

#result %>% head

# We can explore the distribution of p-values
result %>% tidy(fit) %>% qplot(p.value, data=.) + facet_wrap(~term)#~term gives four panels

## Which DRG codes give the smallest p-value?
result %>% tidy(fit) %>% filter(term=="Total.Discharges") %>% arrange(p.value) %>% head

## we can sort by a fit statistic
## Question: what is the "statistic" here?
result %>% glance(fit) %>% arrange(desc(adj.r.squared)) %>% head

## you can easily filter the tidy output
result %>% filter(DRG.code==220) %>% tidy(fit) %>% head

#fun(x,y)={if x>y then 1 else 0}
per_code_avg <- cardiac %>% group_by(DRG.code) %>% summarise(avg=mean(Average.Total.Payments)) 
out<-left_join(cardiac,per_code_avg,"DRG.code")%>% mutate (.,y=ifelse(Average.Total.Payments>avg,1,0)) 
out %>% select(y)%>%head
#lapply(out[,"y"], as.numeric) %>% head

#as.numeric(out$y) #%>% head
fit<-glm(data=out,y~Provider.State + Total.Discharges,family='binomial') 

pred<-predict(fit,out, type = "response")

pred %>% head

(t <- table(out$y, round(pred)))

?table

cardiac %>% names

cardiac %>% filter(Provider.Id=="360006")%>%group_by(DRG.code) %>%
    do(lm(Average.Medicare.Payments ~ Total.Discharges, .) %>% tidy ) 

cardiac %>% group_by(DRG.code) %>%    
    do(lm(Average.Medicare.Payments ~ Total.Discharges + Provider.State, .) %>% 
    glance )%>% arrange(desc(r.squared)) %>% select(DRG.code) %>% head

library(modelr)
library(purrr)

cardiac %>%
    ggplot(aes(log(Total.Discharges), Average.Total.Payments)) +
    geom_point(alpha=0.2) 

DRG249  <- cardiac %>%
    filter(DRG.code==249) %>%
    transmute(covered_scaled=scale(Average.Covered.Charges,
                                   center = TRUE, scale=TRUE),
              total_scaled=scale(Average.Total.Payments,
                                 center=TRUE, scale=TRUE))
    
DRG249 %>% head

DRG249 %>%
    ggplot(aes(x=covered_scaled, y=total_scaled)) +
    geom_point() +
    geom_smooth(method="lm")

DRG249_mod <- lm(total_scaled ~ covered_scaled, data=DRG249)

# extract linear trend with add_predictions()...
DRG249 %>%
    add_predictions(DRG249_mod) %>%
    ggplot(aes(covered_scaled, pred)) +
    geom_line()

# ... and the residuals with add_residuals()
DRG249 %>%
    add_residuals(DRG249_mod) %>%
    ggplot(aes(covered_scaled, resid)) +
    geom_line()

## It's not usually this revealing!


