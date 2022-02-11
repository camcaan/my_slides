
library(tidyr)
library(survival)
library(dplyr)


states <- c("Out", "Hospital", "Institution", "Death")

connect <- matrix(0, nrow = 4, ncol = 4, dimnames=list(states, states))

connect[1,2:4] <- 1
connect[2,c(1,3:4)] <- 1
connect[3,c(1,2:4)] <- 1
statefig(c(1,2,1), connect)



states <- c( "Hospital", "Out"  ,"Institution", "Death")

connect <- matrix(0, nrow = 4, ncol = 4, dimnames=list(states, states))

connect[1,2:4] <- 1
connect[2,c(1,3:4)] <- 1
connect[3,c(1,2:4)] <- 1
statefig(c(1,2,1), connect)





df <- read.csv(file = "example_df.csv")
myformat <- "%d/%m/%Y"

df$deathdate <- as.Date(df$deathdate, myformat )

df$admidate <- as.Date(df$admidate, myformat )
df$discharged <- as.Date(df$discharged, myformat )
# baseline df
baseline <- df %>%
  select(id, Female, deathdate, futime,dead) %>% distinct(id, .keep_all =T)

# df with events and duration
dat <- df %>%
  select(id, duration, hosp, out, insti)



# set the range
step1 <- tmerge(baseline,baseline, death = event(futime, dead), id = id)

step2 <- tmerge(step1, dat, Hospitalisation = event(hosp), id = id)

step3 <- tmerge(step2, dat, Institutionalisation = event(insti), id= id)

step4 <- tmerge(step3, dat, Out = event(out), id = id)


# create the event column

step4$endpoint <- with(step4, factor(Hospitalisation + 2*Institutionalisation + 3*Out + 4*death,
                                     0:4, c("Censored", "Hospitalisation", "Institutionalisation","Out","Death")))


step4$endpoint[is.na(step4$endpoint)] <- "Death"

# longer data
dat_longer <- dat %>%
  pivot_longer(cols = hosp:insti,
               names_to = "event",
               values_to = "time",
               values_drop_na = T)

# creating the death column
newdata <- tmerge(baseline, baseline, id = id, death = event(futime, dead))

# endpoints
newdata <- tmerge(newdata, dat_longer, id=id, event = event(time, event))





temp <- with(newdata, ifelse(event == "censor" , 0,
                             ifelse(event == "hosp", 1,
                                    ifelse(event == "out", 2,
                                           ifelse(event == "insti",3,4)))))
newdata$endpoint <- factor(temp, 0:4, c("censor", "hospital", "out","institution",  "death"))


# I want the duration column in the original df to be a tdc
# I am not sure whether I am doing this correctly. Whether the event == "" should be hospital or institution?
# I want this covariate to use for only transition from hospital to institution
newdata <- tmerge(newdata, subset(dat_longer, event == "hosp"),
                  id = id, Longstays = tdc(time, duration))

newdata$Longstays <- coalesce(newdata$Longstays,0)

#
newdata$LongstayCat <- with(newdata, ifelse(Longstays > 8,1,0))
