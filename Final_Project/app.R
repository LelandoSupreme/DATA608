library(plotly)
library(sqldf)
library(dplyr)
library(reshape2)
library(ggplot2)

# Loaded the cdc csv into a data frame called cdc_raw
cdc_raw <- read.csv("https://raw.githubusercontent.com/LelandoSupreme/DATA608/master/Final_Project/Alcohol-Attributable_Deaths_due_to_Excessive_Alcohol_Use.csv", header = TRUE)
# Data_Value_Alt is a string with commas, code below changes it to numeric
cdc_raw$Data_Value_Decimal <- as.numeric(as.character(gsub(",","",cdc_raw$Data_Value_Alt)))
# Filter out inapplicable or redundant rows and select only the colunns used in the visualizations
cdc_filtered <- sqldf("select LocationAbbr, LocationDesc, Condition, Sex, AgeCategory, 
     AgeGroup, Sum(Data_Value_Decimal) as Data_Value_Decimal from cdc_raw where Effect <>
     'Beneficial Effects' and LocationAbbr <> 'US' and ConditionType <> 'All causes' 
     group by LocationAbbr, LocationDesc, Condition, Sex, AgeCategory, AgeGroup")
# Change value for Sex column to 'All' wherever it is 'Overall'
cdc_filtered$Sex <- gsub("Overall","All",cdc_filtered$Sex)
# Change value for Condition column to 'All Causes' wherever it is 'Subtotal all causes'
cdc_filtered$Condition <- gsub("Subtotal all causes","All Causes",cdc_filtered$Condition)
# Code below creates Over21 data frame by summing all ranges + pro-ratiing the 20-34 age range
cdc_over21 <- sqldf("select LocationAbbr, LocationDesc, Condition, Sex, 'Over 21' as 
    AgeCategory, '21+' as AgeGroup, sum(case when AgeGroup <> '20-34' then Data_Value_Decimal 
    else 0.86666667 * Data_Value_Decimal end) as Data_Value_Decimal from cdc_filtered where 
    AgeCategory <> 'Under 21' and AgeGroup <> 'All Ages' group by LocationAbbr, LocationDesc, 
    Condition, Sex")
# Combine cdc_filtered and cdc_over21 to create cdc_pre, will be joined to census data later
cdc_pre <- rbind(cdc_filtered, cdc_over21)
# Takes the '<' out of the AgeGroup column
cdc_pre$AgeGroup <- gsub("<","",cdc_pre$AgeGroup)
# Creates a vector of all of the unique state code values to use for national average line 
# in the 2nd visualization, joined to create Catetesion product in reactive 'selectedData_age21'
all_states <- as.vector(unique(cdc_pre$LocationAbbr))

# Ultimate goal is to get census data into data frame with State, Sex, 
# and AgeGroup in a format which can be merged to cdc_pre
# Loaded the census csv into a data frame called census_raw
census_raw <- read.csv("https://raw.githubusercontent.com/LelandoSupreme/DATA608/master/Final_Project/Census.csv",header = FALSE, skip = 2,
    col.names = c('ID1', 'ID2', 'LocationDesc', 'All_Total', 'X002', 'M_Total', 'X003',
    'F_Total', 'X004', 'X005', 'X006', 'M_U5', 'X007', 'F_U5', 'X008', 'X009', 'X010',
    'M_5to9', 'X011', 'F_5to9', 'X012', 'X013', 'X014', 'M_10to14', 'X015', 'F_10to14',
    'X016', 'X017', 'X018', 'M_15to19', 'X019', 'F_15to19', 'X020', 'X021', 'X022',
    'M_20to24', 'X023', 'F_20to24', 'X024', 'X025', 'X026', 'M_25to29', 'X027',
    'F_25to29', 'X028', 'X029', 'X030', 'M_30to34', 'X031', 'F_30to34', 'X032', 'X033',
    'X034', 'M_35to39', 'X035', 'F_35to39', 'X035', 'X036', 'X037', 'M_40to44', 'X038',
    'F_40to44', 'X039', 'X040', 'X041', 'M_45to49', 'X042', 'F_45to49', 'X043', 'X044',
    'X045', 'M_50to54', 'X046', 'F_50to54', 'X047', 'X048', 'X049', 'M_55to59', 'X050',
    'F_55to59', 'X051', 'X052', 'X053', 'M_60to64', 'X054', 'F_60to64', 'X055', 'X056',
    'X057', 'M_65to69', 'X058', 'F_65to69', 'X059', 'X060', 'X061', 'M_70to74', 'X062',
    'F_70to74', 'X063', 'X064', 'X065', 'M_75to79', 'X066', 'F_75to79', 'X067', 'X068',
    'X069', 'M_80to84', 'X070', 'F_80to84', 'X071', 'X072', 'X073', 'M_85U', 'X074',
    'F_85U', 'X075', 'X076', 'X077', 'X078', 'X079', 'X080', 'X081', 'X082', 'X083',
    'X084', 'X085', 'X086', 'X087', 'X088', 'X089', 'X090', 'X091', 'X092', 'X093',
    'X094', 'X095', 'X096', 'X097', 'X098', 'X099', 'X100', 'X101', 'X102', 'X103',
    'X104', 'X105', 'X106', 'X107', 'X108', 'X109', 'X110', 'X111', 'X112', 'X113',
    'X114', 'X115', 'X116', 'X117', 'X118', 'X119', 'X120', 'X121', 'X122', 'X123',
    'X124', 'X125', 'X126', 'X127', 'X128', 'X129', 'X130', 'X131', 'X132', 'X133',
    'X134', 'X135', 'X136', 'X137', 'X138', 'X139', 'X140', 'X141', 'X142', 'X143',
    'X144', 'X145', 'X146', 'X147', 'X148', 'X149', 'X150', 'X151', 'X152', 'X153',
    'X154', 'X155', 'X156', 'X157', 'X158', 'X159', 'X160', 'X161', 'X162', 'X163',
    'X164', 'X165', 'X166', 'X167', 'X168', 'X169', 'X170', 'X171', 'X172', 'X173',
    'X174', 'X175', 'X176', 'X177'))
# Filtered only the columns needed and computed total ratios to apply to population figures
census_filtered <- sqldf("select LocationDesc, All_Total, M_Total, M_U5, M_5to9,
    M_10to14, M_15to19, M_20to24, M_25to29, M_30to34, M_35to39, M_40to44, M_45to49,
    M_50to54, M_55to59, M_60to64, M_65to69, M_70to74, M_75to79, M_80to84, M_85U,
    M_U5 + M_5to9 + M_10to14 + M_15to19 + M_20to24 + M_25to29 + M_30to34 + M_35to39 +
    M_40to44 + M_45to49 + M_50to54 + M_55to59 + M_60to64 + M_65to69 + M_70to74 +
    M_75to79 + M_80to84 + M_85U as M_Total_Pcts, F_Total, F_U5, F_5to9, F_10to14,
    F_15to19, F_20to24, F_25to29, F_30to34, F_35to39, F_40to44, F_45to49, F_50to54,
    F_55to59, F_60to64, F_65to69, F_70to74, F_75to79, F_80to84, F_85U, F_U5 + F_5to9 +
    F_10to14 + F_15to19 + F_20to24 + F_25to29 + F_30to34 + F_35to39 + F_40to44 +
    F_45to49 + F_50to54 + F_55to59 + F_60to64 + F_65to69 + F_70to74 + F_75to79 +
    F_80to84 + F_85U as F_Total_Pcts from census_raw")
# Because total ratios don't perfectly add up to 100%, code below calculates new ratios
census_filtered <- sqldf("select LocationDesc, All_Total,
    (M_U5 + F_U5) / (M_Total_Pcts + F_Total_Pcts) as A_U5_R,
    (M_5to9 + F_5to9) / (M_Total_Pcts + F_Total_Pcts) as A_5to9_R,
    (M_10to14 + F_10to14) / (M_Total_Pcts + F_Total_Pcts) as A_10to14_R,
    (M_15to19 + F_15to19) / (M_Total_Pcts + F_Total_Pcts) as A_15to19_R,
    (M_20to24 + F_20to24) / (M_Total_Pcts + F_Total_Pcts) as A_20to24_R,
    (M_25to29 + F_25to29) / (M_Total_Pcts + F_Total_Pcts) as A_25to29_R,
    (M_30to34 + F_30to34) / (M_Total_Pcts + F_Total_Pcts) as A_30to34_R,
    (M_35to39 + F_35to39) / (M_Total_Pcts + F_Total_Pcts) as A_35to39_R,
    (M_40to44 + F_40to44) / (M_Total_Pcts + F_Total_Pcts) as A_40to44_R,
    (M_45to49 + F_45to49) / (M_Total_Pcts + F_Total_Pcts) as A_45to49_R,
    (M_50to54 + F_50to54) / (M_Total_Pcts + F_Total_Pcts) as A_50to54_R,
    (M_55to59 + F_55to59) / (M_Total_Pcts + F_Total_Pcts) as A_55to59_R,
    (M_60to64 + F_60to64) / (M_Total_Pcts + F_Total_Pcts) as A_60to64_R,
    (M_65to69 + F_65to69) / (M_Total_Pcts + F_Total_Pcts) as A_65to69_R,
    (M_70to74 + F_70to74) / (M_Total_Pcts + F_Total_Pcts) as A_70to74_R,
    (M_75to79 + F_75to79) / (M_Total_Pcts + F_Total_Pcts) as A_75to79_R,
    (M_80to84 + F_80to84) / (M_Total_Pcts + F_Total_Pcts) as A_80to84_R,
    (M_85U + F_85U) / (M_Total_Pcts + F_Total_Pcts) as A_85U_R,
    M_Total, M_U5 / M_Total_Pcts as M_U5_R,
    M_5to9 / M_Total_Pcts as M_5to9_R, M_10to14 / M_Total_Pcts as M_10to14_R,
    M_15to19 / M_Total_Pcts as M_15to19_R, M_20to24 / M_Total_Pcts as M_20to24_R,
    M_25to29 / M_Total_Pcts as M_25to29_R, M_30to34 / M_Total_Pcts as M_30to34_R,
    M_35to39 / M_Total_Pcts as M_35to39_R, M_40to44 / M_Total_Pcts as M_40to44_R,
    M_45to49 / M_Total_Pcts as M_45to49_R, M_50to54 / M_Total_Pcts as M_50to54_R,
    M_55to59 / M_Total_Pcts as M_55to59_R, M_60to64 / M_Total_Pcts as M_60to64_R,
    M_65to69 / M_Total_Pcts as M_65to69_R, M_70to74 / M_Total_Pcts as M_70to74_R,
    M_75to79 / M_Total_Pcts as M_75to79_R, M_80to84 / M_Total_Pcts as M_80to84_R,
    M_85U / M_Total_Pcts as M_85U_R, F_Total, F_U5 / F_Total_Pcts as F_U5_R,
    F_5to9 / F_Total_Pcts as F_5to9_R, F_10to14 / F_Total_Pcts as F_10to14_R,
    F_15to19 / F_Total_Pcts as F_15to19_R, F_20to24 / F_Total_Pcts as F_20to24_R,
    F_25to29 / F_Total_Pcts as F_25to29_R, F_30to34 / F_Total_Pcts as F_30to34_R,
    F_35to39 / F_Total_Pcts as F_35to39_R, F_40to44 / F_Total_Pcts as F_40to44_R,
    F_45to49 / F_Total_Pcts as F_45to49_R, F_50to54 / F_Total_Pcts as F_50to54_R,
    F_55to59 / F_Total_Pcts as F_55to59_R, F_60to64 / F_Total_Pcts as F_60to64_R,
    F_65to69 / F_Total_Pcts as F_65to69_R, F_70to74 / F_Total_Pcts as F_70to74_R,
    F_75to79 / F_Total_Pcts as F_75to79_R, F_80to84 / F_Total_Pcts as F_80to84_R,
    F_85U / F_Total_Pcts as F_85U_R from census_filtered")
# Sums the ratios from each age range into age ranges aligned with the cdc data
census_filtered <- sqldf("select LocationDesc, All_Total, A_U5_R + A_5to9_R +
    A_10to14_R + A_15to19_R as A_A0to19_R, A_20to24_R + A_25to29_R + A_30to34_R as
    A_A20to34_R, A_35to39_R + A_40to44_R + A_45to49_R as A_A35to49_R, A_50to54_R +
    A_55to59_R + A_60to64_R as A_A50to64_R, A_65to69_R + A_70to74_R + A_75to79_R +
    A_80to84_R + A_85U_R as A_A65U_R, A_U5_R + A_5to9_R + A_10to14_R + A_15to19_R + 
    (A_20to24_R * 0.4) as A_A0to21_R, (A_20to24_R * 0.6) + A_25to29_R + A_30to34_R + 
    A_35to39_R + A_40to44_R + A_45to49_R + A_50to54_R + A_55to59_R + A_60to64_R + 
    A_65to69_R + A_70to74_R + A_75to79_R + A_80to84_R + A_85U_R as A_A21U_R, 1 as 
    A_AAll_Ages_R, M_Total, M_U5_R + M_5to9_R + M_10to14_R + M_15to19_R as M_A0to19_R, 
    M_20to24_R + M_25to29_R + M_30to34_R as M_A20to34_R, M_35to39_R + M_40to44_R + 
    M_45to49_R as M_A35to49_R, M_50to54_R + M_55to59_R + M_60to64_R as M_A50to64_R, 
    M_65to69_R + M_70to74_R + M_75to79_R + M_80to84_R + M_85U_R as M_A65U_R, M_U5_R + 
    M_5to9_R + M_10to14_R + M_15to19_R + (M_20to24_R * 0.4) as M_A0to21_R, (M_20to24_R 
    * 0.6) + M_25to29_R + M_30to34_R + M_35to39_R + M_40to44_R + M_45to49_R + M_50to54_R + 
    M_55to59_R + M_60to64_R + M_65to69_R + M_70to74_R + M_75to79_R + M_80to84_R + M_85U_R 
    as M_A21U_R, 1 as M_AAll_Ages_R, F_Total, F_U5_R + F_5to9_R + F_10to14_R + F_15to19_R 
    as F_A0to19_R, F_20to24_R + F_25to29_R + F_30to34_R as F_A20to34_R, F_35to39_R + 
    F_40to44_R + F_45to49_R as F_A35to49_R, F_50to54_R + F_55to59_R + F_60to64_R as 
    F_A50to64_R, F_65to69_R + F_70to74_R + F_75to79_R + F_80to84_R + F_85U_R as F_A65U_R, 
    F_U5_R + F_5to9_R + F_10to14_R + F_15to19_R + (F_20to24_R * 0.4) as F_A0to21_R, 
    (F_20to24_R * 0.6) + M_25to29_R + F_30to34_R + F_35to39_R + F_40to44_R + F_45to49_R + 
    F_50to54_R + F_55to59_R + F_60to64_R + F_65to69_R + F_70to74_R + F_75to79_R + 
    F_80to84_R + F_85U_R as F_A21U_R, 1 as F_AAll_Ages_R from census_filtered")
# Puts the Sex/AgeGroup columns into rows by melting those columns
census <- melt(census_filtered, id.vars = c("LocationDesc","All_Total","M_Total","F_Total"))
# Translates first part of value into Sex column (A is all, F is Female, M is male)
census$Sex <- ifelse(substr(census$variable, 1, 1) == "A", "All", 
    ifelse(substr(census$variable, 1, 1) == "M", "Male", "Female"))
# Parses out middle of value into AgeGroup string
census$AgeGroup <- substr(census$variable, 4, nchar(as.character(census$variable)) - 2)
# Replaces the 'to' part of the AgeGroup string with a hyphen
census$AgeGroup <- gsub("to","-",census$AgeGroup)
# Replaces the 'U' part of the AgeGroup string with a plus
census$AgeGroup <- gsub("U","+",census$AgeGroup)
# Replaces the 'All_Ages' part of the AgeGroup string with 'All Ages'
census$AgeGroup <- gsub("All_Ages","All Ages",census$AgeGroup)
# Multiplies the ratio by the applicable population number column
census$Population2010 <- ifelse(census$Sex == "All", census$value * census$All_Total, 
    ifelse(census$Sex == "Male", census$value * census$M_Total, census$value * census$F_Total))
# Deletes no longer needed columns
census$variable <- NULL
census$All_Total <- NULL
census$M_Total <- NULL
census$F_Total <- NULL
census$Ratio <- NULL
census$value <- NULL
# Makes sure the population is an integer
census$Population2010 <- round(census$Population2010, 0)

# merge cdc_pre and census to generate cdc_ranges and cdc_age21 data frames
cdc <- merge(cdc_pre, census, by = c("LocationDesc", "Sex", "AgeGroup"))
# Turn off scientic notation
options(scipen = 999)
# Compute PerCapita value for each row
cdc$PerCapita <- cdc$Data_Value_Decimal / cdc$Population2010
# Create data frame to be used for Map visualization and sorted bar chart
cdc_ranges <- sqldf("select * from cdc where AgeCategory = 'Other'")
# Create data frame to be used for Under 21 vs Over 21 visualization
cdc_age21 <- sqldf("select * from cdc where AgeCategory <> 'Other'")


library(shinydashboard)
library(shiny)

## Build ShinyDashboard ##
ui <- dashboardPage(
  dashboardHeader(
    title = "CDC 2010 Four-Year Average Alcohol-Attributable Deaths",
    titleWidth = 600
    ),
  # Three drop-downs to select Cause (Condition), Sex and Age Group and two sort buttons
  dashboardSidebar(
    selectInput('cause', 'Cause', unique(cdc_ranges$Condition), selected='All Causes'), 
    selectInput('sex', 'Sex', unique(cdc_ranges$Sex), selected='All'), 
    selectInput('age', 'Age Group', unique(cdc_ranges$AgeGroup), selected='All Ages'), 
    # Could only create space between drop downs and buttons bu adding blank sidebarPanel objects
    sidebarPanel(width = "230px", style="background-color: #222d32; border: 0px"), 
    sidebarPanel(width = "230px", style="background-color: #222d32; border: 0px"), 
    sidebarPanel(width = "230px", style="background-color: #222d32; border: 0px"), 
    sidebarPanel(width = "230px", style="background-color: #222d32; border: 0px"), 
    sidebarPanel(width = "230px", style="background-color: #222d32; border: 0px"), 
    # Button for toggling sort on 2nd visualization
    actionButton('action1', label = "Toggle Sort", style="background-color: #c49bdf; float: right"), 
    sidebarPanel(width = "230px", style="background-color: #222d32; border: 0px"), 
    sidebarPanel(width = "230px", style="background-color: #222d32; border: 0px"), 
    sidebarPanel(width = "230px", style="background-color: #222d32; border: 0px"), 
    sidebarPanel(width = "230px", style="background-color: #222d32; border: 0px"), 
    sidebarPanel(width = "230px", style="background-color: #222d32; border: 0px"), 
    sidebarPanel(width = "230px", style="background-color: #222d32; border: 0px"), 
    sidebarPanel(width = "230px", style="background-color: #222d32; border: 0px"), 
    sidebarPanel(width = "230px", style="background-color: #222d32; border: 0px"), 
    # Button for toggling sort on 3rd visualization
    actionButton('action2', label = "Toggle Sort", style="background-color: #1bb3f5; float: right")
    ),
  dashboardBody(
    # Three boxes for each visualization in one column; 2 plotly and 1 ggpot
    fluidRow(
      # Chloropleth Map visualization
      box(plotlyOutput("plot1"), width = "100%", height = "auto"), 
      # Sorted bar chart
      box(plotlyOutput("plot2"), title = "Sorted by Per Capita for Selected Cause, Sex and Age", 
          width = "100%", height = "auto", solidHeader = TRUE, 
          style="font-weight: bold; background-color: #c49bdf"), 
      # Under 21 vs Over 21 population pyramid-style
      box(plotOutput("plot3"), title = "Under 21 vs. Over 21 for Selected Cause and Sex", 
          width = "100%", height = "auto", solidHeader = TRUE, 
          style="font-weight: bold; background-color: #1bb3f5")
    )
  ), skin = "purple"
)

server <- function(input, output) {

  selectedData_ranges <- reactive({
    dfSlice <- merge(
      # Merges cdc ranges data frame (filtered by drop down selection w/factor ordered by toggle
      # sort button) and brings in national average for every state (same value for all states)
      cdcSlice <- cdc_ranges %>% 
        filter(Condition == input$cause, Sex == input$sex, AgeGroup == input$age) %>% 
        mutate(LocationAbbr = factor(LocationAbbr, levels = unique(LocationAbbr)[order(PerCapita, 
        decreasing = ifelse((input$action1 %% 2) == 0,TRUE,FALSE))])), 
      # Because the state is a string, not a float, need same value for every state for plotting
      # New field is created called NAPerCapita which is National Avg Per Capita
      avgSlice <- merge(all_states, tempSlice <- cdc_ranges %>% 
        filter(Condition == input$cause, Sex == input$sex, AgeGroup == input$age) %>% 
        group_by(Condition, Sex, AgeGroup) %>% 
        summarise(Data_Value_Dec = sum(Data_Value_Decimal), Population = sum(Population2010)) %>% 
        mutate(NAPerCapita = Data_Value_Dec / Population), by = NULL) %>% 
        select(x, Condition, Sex, AgeGroup, NAPerCapita) %>% rename(LocationAbbr = x), 
      by = c("LocationAbbr", "Condition", "Sex", "AgeGroup"))
  })
  
  selectedData_age21 <- reactive({
    dfSlice <- merge(
      # Merges cdc_age21 with sum of per capitas (for sorting) of both under21 and over21
      ageSlice <- cdc_age21 %>% 
        filter(Condition == input$cause, Sex == input$sex) %>% 
        mutate(PerCapita = ifelse(AgeCategory == "Under 21", -1 * PerCapita, PerCapita)) %>%
        mutate(SrtFactor = ifelse((input$action2 %% 2) == 0,1,-1)), 
      sumPCSlice <- cdc_age21 %>% 
        filter(Condition == input$cause, Sex == input$sex)  %>% 
        group_by(LocationAbbr, Condition, Sex) %>% 
        summarise(TtlPerCapita = sum(PerCapita)), 
      by = c("LocationAbbr","Condition","Sex"))
  })
    
  output$plot1 <- renderPlotly(
    
    # Plotly Chloropleth map visualization
    plot_geo(selectedData_ranges(), locationmode = 'USA-states') %>%
        add_trace(z = ~PerCapita, text = with(selectedData_ranges(), paste(
            "Population", Population2010, "<br>", "Avg Nbr Deaths: ", Data_Value_Decimal)), 
            locations = ~LocationAbbr, color = ~PerCapita, colors = 'Purples') %>% 
        colorbar(title = "Per Capita Deaths") %>%
        layout(
            title = '2010 Four-Year Avg Alcohol-Attributable Deaths by Sex, Age & Cause<br>(Hover for Details)',
            geo = list(
              scope = 'usa',
              projection = list(type = 'albers usa'))
            )
  )
  
  output$plot2 <- renderPlotly(
    
    # Plotly bar chart sorted by per capita
    plot_ly(selectedData_ranges()) %>%
      add_trace(x = ~LocationAbbr, y = ~PerCapita, type = 'bar', color = ~LocationAbbr) %>%
      add_trace(x = ~LocationAbbr, y = ~NAPerCapita, type = 'scatter', mode = 'lines', 
                name = 'National Average', line = list(color = '#000000'), hoverinfo = "text",
                text = paste0("National Avg: ", round(unique(selectedData_ranges()$NAPerCapita),7))) %>%
      layout(xaxis = list(title = 'State', showgrid = FALSE, zeroline = FALSE),
        yaxis = list(side = 'left', title = 'PerCapita', showgrid = FALSE, zeroline = FALSE))
    
  )

  output$plot3 <- renderPlot(
    
    # Under 21 vs Over 21 pyramid by state
    ggplot(data = selectedData_age21(), aes(x = reorder(LocationAbbr, 
      SrtFactor * TtlPerCapita), y = PerCapita, fill = AgeCategory)) + 
      geom_bar(data = subset(selectedData_age21(), AgeCategory == "Under 21"), stat = "identity") + 
      geom_bar(data = subset(selectedData_age21(), AgeCategory == "Over 21"), stat = "identity") + 
      scale_y_continuous(breaks = seq(-0.0012, 0.0012, 0.0004), 
        labels = paste0(as.character(c(seq(0.0012, 0, -0.0004), seq(0.0004, 0.0012, 0.0004)))), 
        limits = c(-1 * max(selectedData_age21()$PerCapita), max(selectedData_age21()$PerCapita))) + 
      coord_flip() + scale_fill_brewer(palette = "Set1") + theme_bw() + 
      xlab("State") + ylab("Per Capita")
  )  
    
}

shinyApp(ui, server)
