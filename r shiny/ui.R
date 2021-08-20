library(shiny)
library(shinydashboard)
library(RColorBrewer)

# Define UI for application that draws a histogram
dashboardPage(
    dashboardHeader(title = "Glassdoor Data Scientist Jobs"),
    dashboardSidebar(sidebarMenu(
        menuItem("Overview", tabName = "US"),
        menuItem("State Details", tabName = "state")
    )),
    dashboardBody(tabItems(
        #state tab
        tabItem(
            tabName = "state",
            fluidRow(column(
                6,
                selectInput(
                    "selectState",
                    "Select State:",
                    choices = c(
                        "AL",
                        "AZ",
                        "CA",
                        "CO",
                        "DC",
                        "FL",
                        "GA",
                        "IL",
                        "MA",
                        "MD",
                        "MI",
                        "MN",
                        "NC",
                        "NJ",
                        "NY",
                        "OH",
                        "OR",
                        "PA",
                        "TX",
                        "VA",
                        "WA",
                        "Remote"
                    ),
                    selected = "CA"
                )
            ),
            column(
                6,
                selectInput(
                    "selectState2",
                    "Select State:",
                    choices = c(
                        "AL",
                        "AZ",
                        "CA",
                        "CO",
                        "DC",
                        "FL",
                        "GA",
                        "IL",
                        "MA",
                        "MD",
                        "MI",
                        "MN",
                        "NC",
                        "NJ",
                        "NY",
                        "OH",
                        "OR",
                        "PA",
                        "TX",
                        "VA",
                        "WA",
                        "Remote"
                    ),
                    selected = "NY"
                )
            )),
            fluidRow(column(
                6, box(title = "Top Hiring Cities",
                       width = NULL,
                       plotOutput("city"))
            ),
            column(
                6, box(title = "Top Hiring Cities",
                       width = NULL,
                       plotOutput("city2"))
            )),
            
            fluidRow(column(
                6, box(title = "Salary Range",
                       width = NULL,
                       plotOutput("salary"))
            ),
            column(
                6, box(title = "Salary Range",
                       width = NULL,
                       plotOutput("salary2"))
            )),
            fluidRow(column(
                6,
                tabsetPanel(
                    tabPanel("Position", plotOutput("position")),
                    tabPanel("Companies", plotOutput("company")),
                    tabPanel("Industry", plotOutput("industry")),
                    tabPanel("Type", plotOutput("type")),
                    tabPanel("Education", plotOutput("education")),
                    tabPanel("Skills", plotOutput("skills")),
                    tabPanel("Data", dataTableOutput("table"))
                )
            ),
            column(
                6,
                tabsetPanel(
                    tabPanel("Position", plotOutput("position2")),
                    tabPanel("Companies", plotOutput("company2")),
                    tabPanel("Industry", plotOutput("industry2")),
                    tabPanel("Type", plotOutput("type2")),
                    tabPanel("Education", plotOutput("education2")),
                    tabPanel("Skills", plotOutput("skills2")),
                    tabPanel("Table", dataTableOutput("table2"))
                )
            ))
        ),
        #US wide tab
        tabItem(
            tabName = "US",
            # fluidRow(leafletOutput("map1")),
            tabsetPanel(
                tabPanel("Jobs", leafletOutput("map1")),
                tabPanel("Salary", leafletOutput("map2"))),
            fluidRow(
                tabsetPanel(
                    tabPanel("City", plotOutput("city_tot")),
                    tabPanel("Position", plotOutput("position_tot")),
                    tabPanel("Company", plotOutput("company_tot")),
                    tabPanel("Industry", plotOutput("industry_tot")),
                    tabPanel("Type", plotOutput("type_tot")),
                    tabPanel("Education", plotOutput("education_tot")),
                    tabPanel("Skills", plotOutput("skills_tot")),
                    tabPanel("Data", dataTableOutput("table_tot"))
                )
            )
        )
    ))
)
