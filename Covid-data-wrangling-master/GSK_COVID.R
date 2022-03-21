library(tidyverse)
library(plotly)
library(zoo)
library(lubridate)
library(sf)
library(rnaturalearth)
gsk_data <- read.csv("gsk_covid_data.csv")
head(gsk_data)

# covert to data format from ISO
install.packages("ISOweek")
library(ISOweek)
library(remotes)
# install_github('vincentarelbundock/countrycode')
library(countrycode)


gsk_data$Date <-  ISOweek::ISOweek2date(paste0(gsk_data$YearWeekISO, "-1"))



gsk_data$Country <- countrycode(gsk_data$ReportingCountry, origin = "iso2c", destination = "country.name")

gsk_data$Country <- with(gsk_data, ifelse(ReportingCountry == "EL", "Greece", Country))

# Create new variable: number of days
gsk_data <- gsk_data%>%group_by(Country)%>%
  mutate(days = Date-first(Date)+1)%>%ungroup()

gsk_country <- gsk_data %>% group_by(Country) %>% mutate(cumDoses=cumsum(as.numeric(NumberDosesReceived)))

# Aggregate at world level
gsk_world <- gsk_data %>% group_by(Country) %>% summarize(First_dose=cumsum(FirstDose), Second_dose=cumsum(SecondDose))%>%ungroup()
                                                                   

# arrange by decresing order of first does 

gsk_world <- arrange(gsk_world, -First_dose, days)
# Extract specific country: Italy
Italy_GSK_data2<- gsk_data %>% 
  filter(Country=="Italy")%>%
  group_by(Date,Population)%>%
  summarise(people_vaccinated = sum(FirstDose),people_fully_vaccinated = sum(SecondDose))%>%
  select( Date,Population,people_vaccinated,people_fully_vaccinated)


Austria <- gsk_world %>% filter(Country=="Austria")%>%
  select(First_dose, Second_dose, Date,Population)

# SUMMARY STATISTICS
summary(gsk_world)
by(gsk_world$First_dose, gsk_world$Country, summary)
by(gsk_world$Second_dose, gsk_world$Country, summary)

summary(world)
summary(Italy)





### Vaccination progress

# Find out the rows where the second dose data are NAs when first dose were given at the start of the vaccination process
rows <- which(is.na(Italy$SecondDose) & !is.na(Italy$FirstDose))
Italy[rows, "people_fully_vaccinated"] = 0  # Change the NA values to 0




Vaccine <- Italy_GSK_data2%>%
  mutate(`First dose` = round(people_vaccinated/ Population * 100, 3),
         `Fully vaccinated` = round(people_fully_vaccinated/ Population  * 100, 3)) %>%
  select(Date, `First dose`, `Fully vaccinated`) %>%
  drop_na(`First dose`)%>%
  gather("doses", "Population share", -c(Date)) %>%
  mutate(doses = factor(doses))

vac <- ggplot(data = Vaccine, aes(x = Date, y = `Population share`, fill = doses)) +
  geom_area(position = "identity") +
  theme_classic() +
  scale_fill_brewer(palette = "Green") +
  scale_x_date(date_breaks = "2 month") +
  labs(title = "Covid-19 vaccination progress in Austria ",
       x = "Date", y = "Share of Population(%)",
       fill = NULL, caption = "Data: GSK COVID 19 Vaccines") +
  theme(plot.title = element_text(hjust = -0.25), legend.position = "bottom")
# vac +
#   transition_reveal(date) +
#   ease_aes()
ggplotly(vac) %>%
  layout(hovermode = "x", legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1))
