# =========================
# LIBRARIES
# =========================
library(shiny)
library(tidyverse)
library(plotly)
library(readxl)
library(DT)
library(scales)
library(janitor)
library(rsconnect)
# =========================
# LOAD DATA
# =========================
file_path <- "/Users/CHINGNUNGANBA/Desktop/Linkedin/startUp/startup_funding_2021_cleaned.xlsx"

data <- read_excel(file_path) %>%
  clean_names()

# =========================
# CLEAN DATA
# =========================
data <- data %>%
  mutate(
    amount_in_usd = parse_number(as.character(amount_in_usd)),
    month = factor(month,
                   levels = c("Jan","Feb","Mar","Apr","May","Jun",
                              "Jul","Aug","Sep","Oct","Nov","Dec")),
    industry_raw = industry_vertical  
  )

# =========================
# CLEAN CITY
# =========================
clean_city <- function(city){
  city <- str_trim(city)
  city <- str_to_title(city)
  
  case_when(
    city %in% c("New Delhi", "Delhi") ~ "Delhi",
    city %in% c("Gurugram", "Gurgaon") ~ "Gurgaon",
    city %in% c("Bangalore") ~ "Bengaluru",
    city %in% c("Powai","Andheri","Thane") ~ "Mumbai",
    city %in% c("Haryana","Gujarat","Telangana","Telugana") ~ NA_character_,
    TRUE ~ city
  )
}

# =========================
# CLEAN INDUSTRY
# =========================
clean_industry <- function(industry){
  
  industry <- str_to_lower(industry)
  industry <- str_trim(industry)
  
  case_when(
    str_detect(industry, "fintech|financial|banking|finance") ~ "FinTech",
    str_detect(industry, "edtech|education") ~ "EdTech",
    str_detect(industry, "health|biotech|pharma|wellness") ~ "HealthTech",
    str_detect(industry, "e-commerce|retail|d2c|commerce") ~ "E-Commerce",
    str_detect(industry, "ai|ml|data|analytics") ~ "AI / Data",
    str_detect(industry, "logistics|supply") ~ "Logistics",
    str_detect(industry, "food|beverage") ~ "Food & Beverage",
    str_detect(industry, "agri|farming") ~ "AgriTech",
    str_detect(industry, "saas|software|it") ~ "SaaS / IT",
    str_detect(industry, "gaming|sports") ~ "Gaming",
    str_detect(industry, "automotive|mobility|ev") ~ "Automotive",
    TRUE ~ "Other"
  )
}

# APPLY CLEANING
data <- data %>%
  mutate(
    city_clean = clean_city(city),
    industry_clean = clean_industry(industry_vertical)
  ) %>%
  filter(!is.na(city_clean),
         !is.na(amount_in_usd))

# =========================
# SMART FORMAT FUNCTION
# =========================
format_currency <- function(x){
  if(is.na(x)) return("0")
  
  if(x >= 1e9){
    paste0("$", round(x/1e9, 2), "B")
  } else {
    paste0("$", round(x/1e6, 2), "M")
  }
}

# =========================
# KPI CARD
# =========================
kpi_card <- function(title, value_output){
  div(
    style = "
      background: white;
      padding: 20px;
      border-radius: 12px;
      box-shadow: 0 4px 12px rgba(0,0,0,0.08);
      text-align: center;
      height: 140px;
      display: flex;
      flex-direction: column;
      justify-content: center;
    ",
    h5(title, style = "margin-bottom:10px; color:#6c757d;"),
    div(textOutput(value_output),
        style = "font-size:22px; font-weight:bold;")
  )
}

# =========================
# UI
# =========================
ui <- fluidPage(
  
  titlePanel("Startup Funding Dashboard (2021)"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("city", "City",
                  choices = c("All", sort(unique(data$city_clean)))),
      
      selectInput("industry", "Sector (Grouped)",
                  choices = c("All", sort(unique(data$industry_clean))))
    ),
    
    mainPanel(
      
      h4(textOutput("filter_info")),
      br(),
      
      fluidRow(
        column(3, kpi_card("Total Funding", "total_funding")),
        column(3, kpi_card("Startups", "total_startups")),
        column(3, kpi_card("Investors", "total_investors")),
        column(3, kpi_card("Avg Deal", "avg_deal"))
      ),
      
      br(), br(),
      
      plotlyOutput("monthly_trend"),
      br(),
      
      plotlyOutput("top_sectors"),
      br(),
      plotlyOutput("drilldown_other"),
      br(),
      plotlyOutput("top_startups"),
      br(),
      plotlyOutput("top_investors"),
      
      br(),
      DTOutput("table")
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output) {
  
  filtered <- reactive({
    df <- data
    
    if(input$city != "All"){
      df <- df %>% filter(city_clean == input$city)
    }
    
    if(input$industry != "All"){
      df <- df %>% filter(industry_clean == input$industry)
    }
    
    df
  })
  
  output$filter_info <- renderText({
    paste("City:", input$city, "| Sector:", input$industry)
  })
  
  # =========================
  # KPIs (SMART FORMAT)
  # =========================
  output$total_funding <- renderText({
    format_currency(sum(filtered()$amount_in_usd, na.rm = TRUE))
  })
  
  output$total_startups <- renderText({
    n_distinct(filtered()$startup_name)
  })
  
  output$total_investors <- renderText({
    n_distinct(filtered()$investors)
  })
  
  output$avg_deal <- renderText({
    format_currency(mean(filtered()$amount_in_usd, na.rm = TRUE))
  })
  
  # =========================
  # MONTHLY TREND
  # =========================
  output$monthly_trend <- renderPlotly({
    
    df <- filtered() %>%
      group_by(month) %>%
      summarise(total = sum(amount_in_usd, na.rm = TRUE))
    
    ggplotly(
      ggplot(df, aes(month, total, group = 1)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
        labs(title = "Monthly Funding Trend (2021)",
             x = "Month",
             y = "Funding (USD Millions)") +
        theme_minimal()
    )
  })
  
  # =========================
  # SECTORS
  # =========================
  output$top_sectors <- renderPlotly({
    
    df <- filtered() %>%
      group_by(industry_clean) %>%
      summarise(total = sum(amount_in_usd, na.rm = TRUE))
    
    ggplotly(
      ggplot(df,
             aes(total, reorder(industry_clean,total))) +
        geom_col() +
        scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
        labs(title = "Funding by Sector",
             x = "USD (Millions)", y = "") +
        theme_minimal()
    )
  })
  
  # =========================
  # OTHER
  # =========================
  output$drilldown_other <- renderPlotly({
    
    df <- filtered() %>%
      filter(industry_clean == "Other") %>%
      group_by(industry_raw) %>%
      summarise(total = sum(amount_in_usd, na.rm = TRUE)) %>%
      arrange(desc(total)) %>%
      head(15)
    
    ggplotly(
      ggplot(df,
             aes(total, reorder(industry_raw,total))) +
        geom_col() +
        scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
        labs(title = "Inside Other",
             x = "USD (Millions)", y = "") +
        theme_minimal()
    )
  })
  
  # =========================
  # STARTUPS
  # =========================
  output$top_startups <- renderPlotly({
    
    df <- filtered() %>%
      group_by(startup_name) %>%
      summarise(total = sum(amount_in_usd, na.rm = TRUE)) %>%
      arrange(desc(total)) %>%
      head(10)
    
    ggplotly(
      ggplot(df,
             aes(total, reorder(startup_name,total))) +
        geom_col() +
        scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
        labs(title = "Top Startups",
             x = "USD (Millions)", y = "") +
        theme_minimal()
    )
  })
  
  # =========================
  # INVESTORS
  # =========================
  output$top_investors <- renderPlotly({
    
    df <- filtered() %>%
      separate_rows(investors, sep = ",") %>%
      mutate(investors = str_trim(investors)) %>%
      filter(investors != "") %>%
      group_by(investors) %>%
      summarise(total = sum(amount_in_usd, na.rm = TRUE)) %>%
      arrange(desc(total)) %>%
      head(10)
    
    ggplotly(
      ggplot(df,
             aes(total, reorder(investors,total))) +
        geom_col() +
        scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
        labs(title = "Top Investors",
             x = "USD (Millions)", y = "") +
        theme_minimal()
    )
  })
  
  # TABLE
  output$table <- renderDT({
    datatable(filtered(), options = list(pageLength = 10))
  })
}

# RUN
shinyApp(ui, server)

