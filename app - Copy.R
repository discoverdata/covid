#-------------------------------------------------------------------------
#  This application is governed by the MIT license. 
#  You can  use, modify and/ or redistribute this code under the terms
#  of MIT license
#
#  Varun Khanna, Melbourne, Australia
#  May 11, 2020
 #-------------------------------------------------------------------------

library("readr")
library("dplyr")
library("stringr")
library("tidyr")
library("purrr")
library("lubridate")
library("ggplot2")
library("plotly")
library("shiny")
library("shinyWidgets")
library("DT")
library("zoo")
library("scales")

# Functions 
#=====================================================================================
# function to rearrange the columns of the data frame. char and then double columns
rearrange_col <- function(df){
  character_col <- keep(df, is.character)
  names(character_col) <- c("Province", "Country", "Country_code","Region", "Sub_region", "Intermediate_region")
  double_col <- keep(df, is.double) %>% select(!ends_with("Code"))
  df <- bind_cols(character_col, double_col)
  return(df)
}

# Standardize country names
standardizeCountrynames <- function(df) {
  df <- df %>% mutate(Country = case_when(Country == "US" ~ "United States", 
                                          Country == "Macau" ~ "Macao SAR", 
                                          Country == "Hong Kong" ~ "Hong Kong SAR",
                                          Country == "Mainland China" ~ "China",
                                          Country == "UK" ~ "United Kingdom",
                                          Country == "Iran (Islamic Republic of)" ~ "Iran",
                                          Country == "Taipei and environs" ~ "Taiwan",
                                          Country == "Viet Nam" ~ "Vietnam",
                                          Country == "Russia" ~ "Russian Federation",
                                          Country == "Congo (Kinshasa)" ~ "Congo",
                                          Country == "Taiwan*" ~ "Taiwan",
                                          Country == "Czechia" ~ "Czech Republic",
                                          Country %in% c("Korea, South","Republic of Korea") ~ "South Korea",
                                          Country == "Republic of Ireland" ~ "Ireland",
                                          Country == "Venezuela (Boliv. Rep. of)" ~ "Venezuela",
                                          Country == "Syrian Arab Republic" ~ "Syria",
                                          Country == "Bolivia (Plurin. State of)" ~ "Bolivia",
                                          Country == "United Rep. of Tanzania" ~ "Tanzania",
                                          Country == "Burma" ~ "Myanmar",
                                          Country == "Maldova" ~ "Republic of Moldova",
                                          Country == "Cabo Verde" ~ "Cape Verde",
                                          Country %in% c("West Bank and Gaza","Palestine", "occupied Palestinian territory") ~ "Palestinian Territory",
                                          Country == "Congo (Brazzaville)" ~ "Congo Dem. Rep.",
                                          Country == "Timor" ~ "Timor-Leste",
                                          TRUE ~ Country), 
                      Date = mdy(Date))
  return(df)
}

# Calculate doubling time of the virus
doublingRate <- function(df){
  # index for Total cases more than 0
  gt0 <- function(x) x > 0
  index_gt0 <- detect_index(df$Total_cases, gt0)
  # index for not going above twice the max value of total cases
  max_value <- max(df$Total_cases)
    toplimit  <- function(x) x >= max_value
  # calculate the twice column
    df <- df %>% mutate(twice = Total_cases * 2)
    index_toplimit <- detect_index(df$twice, toplimit)
  doubleTimeVector <- vector()
  # logic to calculate the doubling rate
  for(i in index_gt0:index_toplimit){
    get_index <- function(x) x > df[i,"twice"]
    doubleTimeVector <- c(doubleTimeVector,detect_index(df[, "Total_cases"], get_index) - i)
  }
  dates <- df[index_gt0:index_toplimit,"Date"]
  doubleTimedf <- bind_cols(Date = dates, DoublingTime = doubleTimeVector)
  return(doubleTimedf)
}
#=====================================================================================

# Load the data
cases <- read_csv("data/cases/time_series_covid19_confirmed_global_iso3_regions12-05-2020.csv")
# Rearrange the columns for cases
cases <- rearrange_col(cases)

deaths <- read_csv("data/deaths/time_series_covid19_deaths_global_iso3_regions12-05-2020.csv")
deaths <- rearrange_col(deaths)

# Recovered data
recovered <- read_csv("data/recovered/time_series_covid19_recovered_global_iso3_regions12-05-2020.csv")
recovered <- rearrange_col(recovered)

# tests
test <- read_csv("data/tests/covid-19-total-confirmed-cases-vs-total-tests-conducted-12-05-2020.csv")
names(test) <- c("Country","Code","Date","Tests","Cases")


df_cases <- gather(cases, key = Date, value = Cases, -c(1:8))
df_deaths <- gather(deaths, key = Date, value = Deaths, -c(1:8))
df_recovered <- gather(recovered, key = Date, value = Recovered, -c(1:8))

# Prepare the data
df <- df_cases %>% 
  mutate(Deaths = df_deaths$Deaths) %>% 
  as.data.frame(stringsAsFactor = FALSE)

# let us give the standard names to countries
df <- standardizeCountrynames(df)

# Let us combine Country and Country_code column
# Use OTH code for missing Country_codes like Grand Princes, Channel Islands
df[is.na(df$Country_code),3] <- "OTH"

df$Country_code <- paste(df$Country, paste0("(",df$Country_code,")"), sep = " ")

# recovered data

df_recovered[is.na(df_recovered$Country_code),3] <- "OTH"
df_recovered <- standardizeCountrynames(df_recovered)
# 
df_recovered <- df_recovered %>% 
  mutate(Country_code = paste(df_recovered$Country, paste0("(",df_recovered$Country_code,")"), sep = " "))

# Test data

# Remove the rows with no codes
test <- test[!is.na(test$Code),]
test <- standardizeCountrynames(test)

test <- test %>% mutate(Country_code = paste(Country,paste0("(",Code,")"), sep = " "))

# Remove world as Country rows
test <- test %>% filter(Country != "World")

# Make all Country_code equal for dates

test_temp <- data.frame(Date = rep(seq(min(test$Date), max(test$Date), by = '1 day'), 
                                   times = length(unique(test$Code))), Code = rep(unique(test$Code), 
                                    each = length(seq(min(test$Date), max(test$Date), by = '1 day'))), 
                                    stringsAsFactors = FALSE)

test <- test_temp %>% left_join(test, by = c("Code","Date"))
# fill the NA values from the last reported
test <- test %>% group_by(Code) %>% fill(Tests) %>% ungroup()
# replace NA with 0
test <- test %>% mutate_at(c("Tests","Cases"), ~replace(., is.na(.),1))

# save character columns for later use
character_col <- keep(df,is.character) %>% 
  distinct(Country_code, .keep_all = TRUE) %>% 
  bind_rows(.,c(Province  = "World", Country = "World", Country_code = "World (WOR)",Region = "World", 
                Sub_region = "World", Intermediate_region = "World"))

# Growth rate of countries
unique_country_codes <- df %>% filter(!is.na(Country_code)) %>% pull(Country_code) %>% unique()

growth_rate <- sapply(unique_country_codes, function(x) 
  df %>% filter(Country_code == x ) %>% group_by(Date, Country_code) %>% 
    summarise(total_cases = sum(Cases) + 1) %>% ungroup() %>% 
    mutate(day_diff = as.integer(Date - lag(Date)), growth_diff = total_cases - lag(total_cases), 
           rate_diff = (growth_diff/day_diff)/lag(total_cases) * 100) %>% 
    pull(rate_diff)) %>% as.data.frame(stringsAsFactor = FALSE)

growth_rate <- mutate(growth_rate, Date = unique(df$Date))

# replace all NA with 0
growth_rate[is.na(growth_rate)] <- 0

growth_rate <- growth_rate %>% gather(key = "Country_code", value = "Rate_of_Growth", -Date)

# Calculate the growth rate of the world
global_data <- df %>% 
  group_by(Date) %>% 
  summarize(total_cases = sum(Cases), 
            deaths = sum(Deaths)) %>% 
  ungroup() %>%  
  mutate(day_diff = as.integer(Date - lag(Date)), 
         growth_diff = total_cases - lag(total_cases),
         rate_diff = (growth_diff/day_diff)/lag(total_cases) * 100)

# add the recoverd data to global data
global_data <- left_join(global_data, df_recovered %>% 
            group_by(Date) %>% 
            summarize(recovered = sum(Recovered)), by = "Date")

# add the test data to the global data
global_data <- left_join(global_data, test %>% 
                           group_by(Date) %>% 
                           summarise(test = sum(Tests)), by = "Date")

# replace NA with 0
global_data <- replace_na(global_data, list(day_diff = 0, growth_diff =  0, rate_diff = 0))

# join global data with the country data
growth_rate <- data.frame(Date = global_data$Date, 
           Country_code = "World (WOR)", 
           Rate_of_Growth = global_data$rate_diff, 
           stringsAsFactors = FALSE) %>% 
  bind_rows(growth_rate) %>% mutate(Rate_of_Growth = round(Rate_of_Growth,2)) %>%
  inner_join(character_col, by = c("Country_code"))


# Country wise data cases and deaths
data <- df %>% group_by(Date,Country_code) %>% 
  summarise(Total_cases = sum(Cases), Deaths = sum(Deaths)) %>%
  left_join(character_col, by = c("Country_code")) %>% ungroup()

# add recoverd country wise data
data <- left_join(data, df_recovered %>% 
            group_by(Date, Country_code) %>% 
            summarise(Recovered = sum(Recovered)), by = c("Date","Country_code"))

# add test country wise data 
data <- left_join(data, test %>%
            group_by(Date, Country_code) %>%
            summarise(Tests = sum(Tests)) %>% ungroup(), by = c("Date","Country_code"))

data <- data.frame(Date = global_data$Date, 
                   Country = "World", 
                   Total_cases = global_data$total_cases,
                   Deaths = global_data$deaths,
                   Recovered = global_data$recovered,
                   Province = "World",
                   Country_code = "World (WOR)", 
                   Region = "World", 
                   Sub_region = "World", Intermediate_region = "World",
                   Tests = global_data$test,
                   stringsAsFactors = FALSE) %>% bind_rows(data)

# Calculate the double rate of each country

doublingTimedf <- data %>% split(.$Country_code) %>%
  map(doublingRate) %>% bind_rows(., .id = "Country_code")

doublingTimedf <- doublingTimedf %>% filter(DoublingTime > 0)

doublingTimedf <- doublingTimedf %>% left_join(character_col, by = "Country_code")


# Calculate Tests vs cases
data <- data %>% mutate(Cases_vs_Tests = Total_cases/Tests) %>% 
  mutate(Cases_vs_Tests = round(Cases_vs_Tests,2))

# Define who_events
who_events <- tribble(
  ~date, ~event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared"
) %>% mutate(date = as.Date(date))


# Active cases
# Replace NA values with NA in Recovered column 
data <- data %>% mutate(Recovered = replace_na(Recovered, 0))
data <- data %>% mutate(Active_cases = Total_cases - (Deaths + Recovered))

# Define UI for application that draws a line plot and table
ui <- navbarPage("Covid 19",selected = "Cases",inverse = TRUE, collapsible = TRUE,
                 tabPanel("Cases",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                # Radio input
                                radioButtons("radio1", label = "Choose confirmed cases", 
                                             choices = list("Total" = "total", "Active" = "active"), selected = "total", inline = TRUE),
                                # Switch Input
                                switchInput(inputId = "switch1", label = "Log Scale", width = "auto", size = 'mini'),
                                # Select Country
                                selectizeInput(inputId = "country1", label = "Choose country:", 
                                               choices = c(sort(unique(data$Country_code))), 
                                               selected = c("United States (USA)", "Italy (ITA)","World (WOR)"), 
                                               multiple = TRUE, options = list(placeholder = "Please select countries below")),
                                
                                sliderInput("slider1", label = h4("Date"),
                                            min = min(df$Date), max = max(df$Date), value = c(min(df$Date), max(df$Date))
                                ),
                                # Regions
                                checkboxGroupInput(inputId = "region1", label = "Filter data by Region:", inline = TRUE,
                                                   choices = c("Africa", "Americas","Asia", "Europe", "Oceania")),
                                # Sub region
                                checkboxGroupInput(inputId = "subregion1", label = "Filter data by Sub region:",
                                                   inline = TRUE,
                                                   choices = c("Southern Asia","Central Asia","Western Asia",
                                                               "Eastern Asia", "South-eastern Asia","Northern Europe",
                                                               "Southern Europe","Western Europe", "Eastern Europe", 
                                                               "Northern Africa", "Sub-Saharan Africa", 
                                                               "Latin America and the Caribbean","Australia and New Zealand",
                                                               "Northern America","Melanesia","Polynesia"))
                                ), # sidebarPanel ends
                              # Output
                              mainPanel(plotlyOutput(outputId = "lineplot1", height = "400", width = "100%"),
                                        DT::dataTableOutput(outputId = "table1", width = "100%")) # mainPanel ends
                              
                            ) # sidebarLayout ends
                          )),
                 tabPanel("Growth", 
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                # Select the rate of growth
                                radioButtons(inputId = "rollingMean", label = "Rate of growth",
                                             choices = c("Daily rate of growth","Mean rate of growth", "Both"),
                                             selected = c("Daily rate of growth"),
                                             inline = TRUE),
                                  # Select Country
                                selectizeInput(inputId = "country2", label = "Choose country:", 
                                               choices = c(sort(unique(data$Country_code))), 
                                               selected = c("United States (USA)", "Italy (ITA)","World (WOR)"), 
                                               multiple = TRUE, options = list(placeholder = "Please select countries below")),
                                
                                sliderInput("slider2", label = h4("Date"),
                                            min = min(df$Date), max = max(df$Date), value = c(min(df$Date), max(df$Date))),
                                
                                # Regions
                                checkboxGroupInput(inputId = "region2", label = "Filter data by Region:",inline = TRUE, 
                                                   choices = c("Africa", "Americas","Asia", "Europe", "Oceania")),
                                # Sub region
                                checkboxGroupInput(inputId = "subregion2", label = "Filter data by Sub region:",
                                                   inline = TRUE,
                                                   choices = c("Southern Asia","Central Asia","Western Asia",
                                                               "Eastern Asia", "South-eastern Asia","Northern Europe",
                                                               "Southern Europe","Western Europe", "Eastern Europe", 
                                                               "Northern Africa", "Sub-Saharan Africa", 
                                                               "Latin America and the Caribbean","Australia and New Zealand",
                                                               "Northern America","Melanesia","Polynesia")
                                                   )
                                
                              ), # SidebarPanel ends
                              # Output
                              mainPanel(plotlyOutput(outputId = "lineplot2", height = "400", width = "100%"),
                                        DT::dataTableOutput(outputId = "table2", width = "100%"))
                            ) # SidebarLayout ends
                            
                          ) # fluidPage ends
                          ), # Growth tabPanel ends here
                 tabPanel("Deaths", 
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                # Radio input
                                radioButtons("radio2", label = "Choose:", 
                                             choices = list("Total Deaths" = "deaths", "Death Rate" = "death rate"), 
                                             selected = "deaths", inline = TRUE),
                                switchInput(inputId = "switch2", label = "Log Scale", width = "auto", size = 'mini'),
                                # Select Country
                                selectizeInput(inputId = "country3", label = "Choose country:", 
                                               choices = c(sort(unique(data$Country_code))), 
                                               selected = c("Italy (ITA)","United States (USA)","World (WOR)"), 
                                               multiple = TRUE, options = list(placeholder = "Please select countries below")),
                                
                                sliderInput("slider3", label = h4("Date"),
                                            min = min(df$Date), max = max(df$Date), value = c(min(df$Date), max(df$Date))),
                                
                                # Regions
                                checkboxGroupInput(inputId = "region3", label = "Filter data by Region:", inline = TRUE,
                                                   choices = c("Africa", "Americas","Asia", "Europe", "Oceania")),
                                
                                # Sub region
                                checkboxGroupInput(inputId = "subregion3", label = "Filter data by Sub region:",
                                                   inline = TRUE,
                                                   choices = c("Southern Asia","Central Asia","Western Asia",
                                                               "Eastern Asia", "South-eastern Asia","Northern Europe",
                                                               "Southern Europe","Western Europe", "Eastern Europe", 
                                                               "Northern Africa", "Sub-Saharan Africa", 
                                                               "Latin America and the Caribbean","Australia and New Zealand",
                                                               "Northern America","Melanesia","Polynesia")
                                )
                                
                              ), # SidebarPanel ends
                              
                              # Output
                              mainPanel(plotlyOutput(outputId = "lineplot3", height = "400", width = "100%"),
                                        DT::dataTableOutput(outputId = "table3", width = "100%"))
                            ) # SidebarLayout ends
                            
                          ) # fluidPage ends
                 ), # Deaths tab panel ends here
                 # Doubling Rate panel
                 tabPanel("Doubling Rate", 
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                # Select Country
                                selectizeInput(inputId = "country4", label = "Choose country:", 
                                               choices = c(sort(unique(doublingTimedf$Country_code))), 
                                               selected = c("Italy (ITA)","United States (USA)","World (WOR)"), 
                                               multiple = TRUE, options = list(placeholder = "Please select countries below")),
                                
                                sliderInput("slider4", label = h4("Date"),
                                            min = min(df$Date), max = max(df$Date), value = c(min(df$Date), max(df$Date))),
                                
                                # Regions
                                checkboxGroupInput(inputId = "region4", label = "Filter data by Region:", inline = TRUE,
                                                   choices = c("Africa", "Americas","Asia", "Europe", "Oceania")),
                                
                                # Sub region
                                checkboxGroupInput(inputId = "subregion4", label = "Filter data by Sub region:",
                                                   inline = TRUE,
                                                   choices = c("Southern Asia","Central Asia","Western Asia",
                                                               "Eastern Asia", "South-eastern Asia","Northern Europe",
                                                               "Southern Europe","Western Europe", "Eastern Europe", 
                                                               "Northern Africa", "Sub-Saharan Africa", 
                                                               "Latin America and the Caribbean","Australia and New Zealand",
                                                               "Northern America","Melanesia","Polynesia")
                                )
                                
                              ), # SidebarPanel ends
                              
                              # Output
                              mainPanel(plotlyOutput(outputId = "lineplot4", height = "400", width = "100%"),
                                        DT::dataTableOutput(outputId = "table4", width = "100%"))
                            ) # SidebarLayout ends
                            
                          ) # fluidPage ends
                 ), # Doubling Rate panel ends here
                 
                 # Recovered
                 tabPanel("Recovered",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                # Radio input
                                radioButtons("radio3", label = "Choose:", 
                                             choices = list("Total Recovered" = "recovered", "Recovery Rate" = "recovery rate"), 
                                             selected = "recovered", inline = TRUE),
                                # Switch Input
                                switchInput(inputId = "switch3", label = "Log Scale", width = "auto", size = 'mini'),
                                # Select Country
                                selectizeInput(inputId = "country5", label = "Choose country:",
                                               choices = c(sort(unique(data$Country_code))),
                                               selected = c("Italy (ITA)","United States (USA)","World (WOR)"),
                                               multiple = TRUE, options = list(placeholder = "Please select countries below")),

                                sliderInput("slider5", label = h4("Date"),
                                            min = min(df$Date), max = max(df$Date), value = c(min(df$Date), max(df$Date))),

                                # Regions
                                checkboxGroupInput(inputId = "region5", label = "Filter data by Region:", inline = TRUE,
                                                   choices = c("Africa", "Americas","Asia", "Europe", "Oceania")),

                                # Sub region
                                checkboxGroupInput(inputId = "subregion5", label = "Filter data by Sub region:",
                                                   inline = TRUE,
                                                   choices = c("Southern Asia","Central Asia","Western Asia",
                                                               "Eastern Asia", "South-eastern Asia","Northern Europe",
                                                               "Southern Europe","Western Europe", "Eastern Europe",
                                                               "Northern Africa", "Sub-Saharan Africa",
                                                               "Latin America and the Caribbean","Australia and New Zealand",
                                                               "Northern America","Melanesia","Polynesia")
                                )

                              ), # SidebarPanel ends

                              # Output
                              mainPanel(plotlyOutput(outputId = "lineplot5", height = "400", width = "100%"),
                                        DT::dataTableOutput(outputId = "table5", width = "100%"))
                            ) # SidebarLayout ends

                          ) # fluidPage ends
                 ), # Recovered panel ends here
                 # Cases vs Test
                 tabPanel("Cases vs Tests",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                # Select Country
                                selectizeInput(inputId = "country6", label = "Choose country:",
                                               choices = c(sort(unique(data$Country_code))),
                                               selected = c("France (FRA)","United States (USA)","World (WOR)"),
                                               multiple = TRUE, options = list(placeholder = "Please select countries below")),
                                
                                sliderInput("slider6", label = h4("Date"),
                                            min = min(df$Date), max = max(df$Date), value = c(min(df$Date), max(df$Date))),
                                
                                # Regions
                                checkboxGroupInput(inputId = "region6", label = "Filter data by Region:", inline = TRUE,
                                                   choices = c("Africa", "Americas","Asia", "Europe", "Oceania")),
                                
                                # Sub region
                                checkboxGroupInput(inputId = "subregion6", label = "Filter data by Sub region:",
                                                   inline = TRUE,
                                                   choices = c("Southern Asia","Central Asia","Western Asia",
                                                               "Eastern Asia", "South-eastern Asia","Northern Europe",
                                                               "Southern Europe","Western Europe", "Eastern Europe",
                                                               "Northern Africa", "Sub-Saharan Africa",
                                                               "Latin America and the Caribbean","Australia and New Zealand",
                                                               "Northern America","Melanesia","Polynesia")
                                )
                                
                              ), # SidebarPanel ends
                              
                              # Output
                              mainPanel(plotlyOutput(outputId = "lineplot6", height = "400", width = "100%"),
                                        DT::dataTableOutput(outputId = "table6", width = "100%"))
                            ) # SidebarLayout ends
                            
                          ) # fluidPage ends
                 ), # Recovered panel ends here
                 tabPanel("About", includeMarkdown("about.Rmd")),
                 tabPanel("Code", pre(includeText("app.R")))
)

# Define server function 
server <- function(input, output) {
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot1 <- renderPlotly({
    # req(input$Date)
    lower_date <- (input$slider1)[1]
    upper_date <- input$slider1[2]
    
    # Plot the data
    subdata <- data %>% filter(Country_code %in% input$country1 
                               | Region %in% input$region1 
                               | Sub_region %in% input$subregion1) %>%
      select(Date, Country_code,Total_cases, Active_cases) %>%
      filter(Date >= lower_date & Date <= upper_date)
    
    # Logic for radio button
    if (input$radio1 == "total")
    {
          subdata <- subdata %>%  rename("Cases" = "Total_cases")
    }
    else{
      
      subdata <- subdata %>% rename("Cases" = "Active_cases")
      }
    
    if (input$switch1){
      # Add one to all Cases before taking a log
      subdata <- subdata %>% mutate(log_cases = log(Cases + 1))
      p <- plot_ly(subdata, x = ~Date, y = ~log_cases, mode = "lines+markers", 
                   type = 'scatter', color = ~Country_code)
      p %>% layout(shapes=list(list(type = 'line', 
                                    x0 = who_events$date[1], 
                                    x1 = who_events$date[1],
                                    y0 = 0, 
                                    y1 = max(subdata$log_cases), 
                                    line = list(width = 2, color = "#1ABBCD", dash = 'dash')), 
                               list(type = 'line', 
                                    x0 = who_events$date[2], 
                                    x1 = who_events$date[2],
                                    y0 = 0, 
                                    y1 = max(subdata$log_cases), 
                                    line = list(width = 2, color = "#1ABBCD", dash = 'dash'))), 
                   annotations = list(list( 
                     x = who_events$date[1],
                     y = max(subdata$log_cases) + max(subdata$log_cases) * 0.1,
                     xref = 'x',
                     yref = 'y',
                     text = who_events$event[1],
                     showarrow = FALSE), list( 
                       x = who_events$date[2],
                       y = max(subdata$log_cases) + max(subdata$log_cases) * 0.1,
                       xref = 'x',
                       yref = 'y',
                       text = who_events$event[2],
                       showarrow = FALSE)), title = paste(str_to_sentence(input$radio1), "coronavirus cases in log scale", sep = " "), 
                       yaxis = list(title = paste(str_to_sentence(input$radio1), "coronavirus cases", sep = " ")), xaxis = list(title = ""))
    }
    else {

    p <- plot_ly(subdata, x = ~Date, y = ~Cases, mode = "lines+markers", 
            type = 'scatter', color = ~Country_code)
    
    p %>% layout(shapes=list(list(type = 'line', 
                                  x0 = who_events$date[1], 
                                  x1 = who_events$date[1],
                                  y0 = 0, 
                                  y1 = max(subdata$Cases), 
                                  line = list(width = 2, color = "#1ABBCD", dash = 'dash')), 
                             list(type = 'line', 
                                  x0 = who_events$date[2], 
                                  x1 = who_events$date[2],
                                  y0 = 0, 
                                  y1 = max(subdata$Cases), 
                                  line = list(width = 2, color = "#1ABBCD", dash = 'dash'))), 
                 annotations = list(list( 
                   x = who_events$date[1],
                   y = max(subdata$Cases) + max(subdata$Cases) * 0.1,
                   xref = 'x',
                   yref = 'y',
                   text = who_events$event[1],
                   showarrow = FALSE), list( 
                     x = who_events$date[2],
                     y = max(subdata$Cases) + max(subdata$Cases) * 0.1,
                     xref = 'x',
                     yref = 'y',
                     text = who_events$event[2],
                     showarrow = FALSE)), title = paste(str_to_sentence(input$radio1), "coronavirus cases till date", sep = " "), 
                     yaxis = list(title = paste(str_to_sentence(input$radio1), "coronavirus cases", sep = " ")), xaxis = list(title = ""))
    }
  })
  
  # Create data table1 for first plot
  output$table1 <- DT::renderDataTable({
    
    lower_date <- (input$slider1)[1]
    upper_date <- input$slider1[2]
    
    if(is.element("total", input$radio1))
    {
      data <- data %>% rename("Cases" = "Total_cases")
    }
    else{
      data <- data %>% rename("Cases" = "Active_cases")
    }
    
    subdata <- data %>% filter(Country_code %in% c(input$country1) 
                               | Region %in% c(input$region1) 
                               | Sub_region %in% c(input$subregion1)) %>%
      select(Date, Country_code,Cases) %>%
      filter(Date >= lower_date & Date <= upper_date) %>% 
      spread(key = Country_code, value = Cases)
    
        # Make the data table
    DT::datatable(subdata, options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
    
  })
  
  # Create a growth plot
  output$lineplot2 <- renderPlotly({
    lower_date <- input$slider2[1]
    upper_date <- input$slider2[2]
    
    subdata1 <- growth_rate %>% filter(Country_code %in% input$country2 
                                       | Region %in% input$region2
                                       | Sub_region %in% input$subregion2) %>% 
      filter(Date >= lower_date & Date <= upper_date) %>% 
      group_by(Country_code) %>%
      mutate(rollingMean = rollapply(Rate_of_Growth, 5, mean, by = 1, align = "right", fill = NA)) %>% 
      mutate(rollingMean = round(rollingMean,2))
      
    if (is.element("Daily rate of growth",input$rollingMean)){
      plot_ly(subdata1, x = ~Date, y = ~Rate_of_Growth, mode = "lines+markers", 
              type = 'scatter', color = ~Country_code) %>% 
        layout(title = "Growth of Covid-19 cases in % per day",
               yaxis = list(title = "Rate of growth in %"), xaxis = list(title = "")) 
    }
    else if(any("Mean rate of growth" ==  input$rollingMean))
    {
      plot_ly(subdata1, x = ~Date, y = ~rollingMean, mode = "lines", type = "scatter",
               color = ~Country_code)%>% 
        layout(title = "Rolling 5-day average growth of Covid-19 cases in %",
               yaxis = list(title = "Rolling mean rate of growth in %"), xaxis = list(title = ""))
    }
    else 
    {
      plot_ly(subdata1, x = ~Date, y = ~Rate_of_Growth, mode = "lines+markers", 
              type = 'scatter', color = ~Country) %>% 
        add_lines(y = ~rollingMean, line = list(color = ~Country_code, width = 2, dash = 'dash'))  %>%
        layout(title = "Growth of Covid-19 cases in % per day",
               yaxis = list(title = "Rate of growth in %"), xaxis = list(title = "")) 
      }
    
    
    })
  
  # Create data table2 for second plot Growth plot
  output$table2 <- renderDataTable({
    # Date slider for growth table
    lower_date <- input$slider2[1]
    upper_date <- input$slider2[2]
    
    if (is.element("Mean rate of growth",input$rollingMean)) {
      
      subdata2 <- growth_rate %>% filter(Country_code %in% input$country2 
                                         | Region %in% input$region2
                                         | Sub_region %in% input$subregion2) %>% 
        filter(Date >= lower_date & Date <= upper_date) %>%
        group_by(Country_code) %>%
        mutate(rollingMean = rollapply(Rate_of_Growth, 5, mean, by = 1, align = "right", fill = NA)) %>% 
        mutate(rollingMean = round(rollingMean,2))%>% ungroup() %>%
        select(Date, Country_code, rollingMean) %>%
        drop_na() %>%
        mutate(rollingMean = paste(rollingMean, "%", sep = "")) %>%
        spread(key = Country_code,value = rollingMean)
        
      DT::datatable(subdata2, options = list(lengthMenu= c(5, 30, 50), pageLength = 10)) 
    }
    else {
      subdata2 <- growth_rate %>% filter(Country_code %in% input$country2 
                                         | Region %in% input$region2
                                         | Sub_region %in% input$subregion2) %>% 
        filter(Date >= lower_date & Date <= upper_date) %>% 
        select(Date, Country_code, Rate_of_Growth) %>% 
        mutate(Rate_of_Growth = paste(Rate_of_Growth, "%", sep = "")) %>%
        spread(key = Country_code,value = Rate_of_Growth)
      
      DT::datatable(subdata2, options = list(lengthMenu= c(5, 30, 50), pageLength = 10)) 
    }
  })
  
  # Create scatterplot object the plotOutput function is expecting for Deaths tab
  output$lineplot3 <- renderPlotly({
    
    lower_date <- (input$slider3)[1]
    upper_date <- input$slider3[2]
    
    # Plot the data
    subdata3 <- data %>% filter(Country_code %in% input$country3 
                                | Region %in% input$region3
                                | Sub_region %in% input$subregion3) %>%
      filter(Date >= lower_date & Date <= upper_date) %>%
      select(Date, Country_code, Total_cases,Deaths)
    
    if (input$radio2 == "death rate")
    {
     subdata3 <- subdata3 %>% mutate(Deaths = (Deaths/Total_cases) * 100)
    }

    if (input$switch2){
      subdata3 <- subdata3 %>% mutate_at(vars(Deaths), ~replace(., is.nan(.), 1)) %>% 
        mutate_at(vars(Deaths), ~replace(., 0, 1)) %>% mutate(log_deaths = log(Deaths)) %>%
        mutate(log_deaths = if_else(log_deaths < 0,0,log_deaths))
      
      plot_ly(subdata3, x = ~Date, y = ~log_deaths, mode = "lines+markers", 
              type = 'scatter', color = ~Country_code) %>% 
        layout(title = paste("Covid-19",str_to_sentence(input$radio2), " in (Log Scale)", sep = " "),
               yaxis = list(title = paste(str_to_sentence(input$radio2),"in Log Scale", sep = " ")), xaxis = list(title = ""))
      }
    else {
      subdata3 <- subdata3 %>% mutate_at(vars(Deaths), ~replace(., is.nan(.), 0))
      if (input$radio2 == "death rate")
      {
        plot_ly(subdata3, x = ~Date, y = ~Deaths, mode = "lines+markers", 
                type = 'scatter', color = ~Country_code) %>% 
          layout(title = paste("Covid-19", str_to_sentence(input$radio2), "till date", sep = " "),
                 yaxis = list(title = paste(str_to_sentence(input$radio2), "in percentage", sep = " ")), xaxis = list(title = ""))
        
      }
      else {
    plot_ly(subdata3, x = ~Date, y = ~Deaths, mode = "lines+markers", 
            type = 'scatter', color = ~Country_code) %>% 
        layout(title = paste("Covid-19", str_to_sentence(input$radio2), "till date", sep = " "),
               yaxis = list(title = paste(str_to_sentence(input$radio2), sep = " ")), xaxis = list(title = ""))

      } 
      }
    
  })
  
  output$table3 <- renderDataTable({
    
    lower_date <- c(input$slider3)[1]
    upper_date <- c(input$slider3)[2]

    subdata3 <- data %>% filter(Country_code %in% input$country3 
                                | Region %in% input$region3
                                | Sub_region %in% input$subregion3) %>% 
      filter(Date >= lower_date & Date <= upper_date) %>%
      select(Date, Country_code, Total_cases , Deaths)
      
      if (input$radio2 == "death rate"){
        subdata3 <- subdata3 %>% mutate(Deaths = round(Deaths/Total_cases, 4)) %>%
          mutate_at(vars(Deaths), ~replace(., is.nan(.), 0)) %>% mutate(Deaths = percent(Deaths, accuracy = 0.01))
      }

      subdata3 <- subdata3 %>% select(Date, Country_code, Deaths) %>% spread(key = Country_code, value = Deaths)
    
    DT::datatable(subdata3, options = list(lengthMenu= c(5, 30, 50), pageLength = 10))
  })
  
  # Create plot for DoublingRate tab
  output$lineplot4 <- renderPlotly({
    
    lower_date <- (input$slider4)[1]
    upper_date <- input$slider4[2]
    
    # Plot the data
    subdata4 <- doublingTimedf %>% filter(Country_code %in% input$country4 
                                | Region %in% input$region4
                                | Sub_region %in% input$subregion4) %>%
      filter(Date >= lower_date & Date <= upper_date)
    
    plot_ly(subdata4, x = ~Date, y = ~DoublingTime, mode = "lines+markers", 
            type = 'scatter', color = ~Country_code) %>% 
      layout(title = "Doubling rate of Covid-19 cases for countries",
             yaxis = list(title = "Doubling rate in days", xaxis = list(title = ""))) 
    
  })
  
  output$table4 <- renderDataTable({
    
    lower_date <- (input$slider4)[1]
    upper_date <- input$slider4[2]
    
    subdata4 <- doublingTimedf %>% filter(Country_code %in% input$country4 
                                | Region %in% input$region4
                                | Sub_region %in% input$subregion4) %>% 
      filter(Date >= lower_date & Date <= upper_date) %>%
      select(Date, Country_code, DoublingTime) %>%
      spread(key = Country_code, value = DoublingTime)
    
    DT::datatable(subdata4, options = list(lengthMenu= c(5, 30, 50), pageLength = 10))
  })
 
  
  # Create Recovered plot and table
  output$lineplot5 <- renderPlotly({

    lower_date <- (input$slider5)[1]
    upper_date <- input$slider5[2]

    # Plot the data
    subdata5 <- data %>% filter(Country_code %in% input$country5
                                | Region %in% input$region5
                                | Sub_region %in% input$subregion5) %>%
      filter(Date >= lower_date & Date <= upper_date)
    
    if(input$radio3 == "recovery rate")
    {
      subdata5 <- subdata5 %>% mutate(Recovered = Recovered/Total_cases * 100)
    }
    
    if (input$switch3){
      subdata5 <- subdata5 %>% mutate_at(vars(Recovered), ~replace(., is.nan(.), 1)) %>% 
        mutate_at(vars(Recovered), ~replace(., 0, 1)) %>% mutate(log_recovered = log(Recovered)) %>%
        mutate(log_recovered = if_else(log_recovered < 0,0,log_recovered))
      
      #subdata5 <- subdata5 %>% mutate(log_recovered = log(Recovered))
      plot_ly(subdata5, x = ~Date, y = ~log_recovered, mode = "lines+markers", 
              type = 'scatter', color = ~Country_code) %>% 
        layout(title = paste("Covid-19",str_to_sentence(input$radio3), " in (Log Scale)", sep = " "),
               yaxis = list(title = paste(str_to_sentence(input$radio3),"in Log Scale", sep = " ")), xaxis = list(title = ""))
     }
    else {
      subdata5 <- subdata5 %>% mutate_at(vars(Recovered), ~replace(., is.nan(.), 0))
      if (input$radio3 == "recovery rate"){
        plot_ly(subdata5, x = ~Date, y = ~Recovered, mode = "lines+markers", 
                type = 'scatter', color = ~Country_code) %>% 
          layout(title = paste("Covid-19", str_to_sentence(input$radio3), "till date", sep = " "),
                 yaxis = list(title = paste(str_to_sentence(input$radio3), "in percentage", sep = " ")), xaxis = list(title = ""))
      }
      else {
      plot_ly(subdata5, x = ~Date, y = ~Recovered, mode = "lines+markers", 
              type = 'scatter', color = ~Country_code) %>% 
        layout(title = paste("Covid-19", str_to_sentence(input$radio3), "till date", sep = " "),
               yaxis = list(title = paste(str_to_sentence(input$radio3), "", sep = " ")), xaxis = list(title = ""))
      }
      } 
    
    
  })

  output$table5 <- renderDataTable({

    lower_date <- c(input$slider5)[1]
    upper_date <- c(input$slider5)[2]


    subdata5 <- data %>% filter(Country_code %in% input$country5
                                | Region %in% input$region5
                                | Sub_region %in% input$subregion5) %>%
      filter(Date >= lower_date & Date <= upper_date) %>%
      select(Date, Country_code, Total_cases, Recovered)

    if (input$radio3 == "recovery rate"){
      subdata5 <- subdata5 %>% mutate(Recovered = round(Recovered/Total_cases, 4)) %>%
        mutate_at(vars(Recovered), ~replace(., is.nan(.), 0)) %>% mutate(Recovered = percent(Recovered, accuracy = 0.01))
    }
    
    subdata5 <- subdata5 %>% select(Date, Country_code, Recovered) %>% spread(key = Country_code, value = Recovered)
    
    DT::datatable(subdata5, options = list(lengthMenu= c(5, 30, 50), pageLength = 10))
  })

  # Create Cases vs Tests plot and table
  output$lineplot6 <- renderPlotly({
    
    lower_date <- (input$slider6)[1]
    upper_date <- input$slider6[2]
    
    # Plot the data
    subdata6 <- data %>% filter(Country_code %in% input$country6
                                | Region %in% input$region6
                                | Sub_region %in% input$subregion6) %>%
      filter(Date >= lower_date & Date <= upper_date) %>% 
      filter(Cases_vs_Tests > 0, Cases_vs_Tests < 1)
    
    plot_ly(subdata6, x = ~Date, y = ~Cases_vs_Tests, mode = "lines+markers",
            type = 'scatter', color = ~Country_code) %>% 
      layout(title = "Confirmed Covid-19 cases vs tests in % for countries",
             yaxis = list(title = "Cases vs Tests %", tickformat = "%"), xaxis = list(title = ""))
    
  })
  
  output$table6 <- renderDataTable({
    
    lower_date <- c(input$slider6)[1]
    upper_date <- c(input$slider6)[2]
    
    
    subdata6 <- data %>% filter(Country_code %in% input$country6
                                | Region %in% input$region6
                                | Sub_region %in% input$subregion6) %>%
      filter(Date >= lower_date & Date <= upper_date) %>%
      filter(Cases_vs_Tests > 0, Cases_vs_Tests < 1) %>%
      select(Date, Country_code, Cases_vs_Tests) %>% mutate(Cases_vs_Tests = percent(Cases_vs_Tests)) %>%
      spread(key = Country_code, value = Cases_vs_Tests) 
    
    DT::datatable(subdata6, options = list(lengthMenu= c(5, 30, 50), pageLength = 10))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)