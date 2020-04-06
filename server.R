thm <- hc_theme(colors = c("#00aba9", "#ff0097", "#a200ff", "#1ba1e2\t", "#f09609"), chart = list(backgroundColor = NULL, divBackgroundImage = NULL))


"%!in%" <- function(x, y) !(x %in% y)

#When publishing, you need to erase the -3600*24 in order to get the real daily number.


server <- function(input, output, session) {
  useSweetAlert()
  
  url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
  
  if(status_code(GET(url)) != 404){
    #Part of the below code to get the data is from ecdc.europa. 
    #download the dataset from the website to a local temporary file
    GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
    
    #read the Dataset sheet into “R”
    data <- read_excel(tf)
    
    #print the uploaded data
    cat('Data has been uploaded.', '\n', 'Last update:', format(Sys.time(), "%Y-%m-%d"), sep = "")
  } else {
    url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time()-3600*24, "%Y-%m-%d"), ".xlsx", sep = "")
    
    #download the dataset from the website to a local temporary file
    GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
    
    #read the Dataset sheet into “R”
    data <- read_excel(tf)
    
    #print the uploaded data
    cat('Data has been uploaded.', '\n', 'Last update:', format(Sys.time()-3600*24, "%Y-%m-%d"), sep = "")
  }
  
  #Sometimes, names of the data change
  names(data) <- c("DateRep", "Day", "Month", "Year", "Cases", "Deaths",                
                   "Countries and territories", "GeoId", "countryterritoryCode", "popData2018")
  
  observe({
    
    countries_list <- data %>% group_by(`Countries and territories`) %>% summarise(total_cases = sum(Cases), total_deaths = sum(Deaths)) %>% filter(total_cases >=100, total_deaths >=10)
    countries_list <- unique(countries_list$`Countries and territories`)
    
    updateSelectizeInput(session, "countries",
                         label = h5("Countries:"),
                         choices = countries_list,
                         selected = NULL,
                         options = list(maxItems = 2)
    )
  })
  
  
  
  
  
  df_kpi <- data %>% group_by(GeoId, `Countries and territories`) %>% summarise(Cases_total = sum(Cases), Deaths_total = sum(Deaths))
  df_kpi$death_rate <- round(df_kpi$Deaths_total / df_kpi$Cases_total,3)*100
  
  observeEvent(input$apply, {
    df_kpi <- df_kpi[df_kpi$Cases_total > input$variation_count,]
    
    if (input$kpi == "Death rate"){
      my_value <- "death_rate"
      my_suffix <- "%"
    }else if(input$kpi == "Number of deaths"){
      my_value <- "Deaths_total"
      my_suffix <- ""
    }else if(input$kpi == "Number of cases"){
      my_value <- "Cases_total"
      my_suffix <- ""
    }
    
    mapdata <- get_data_from_map(download_map_data("custom/world"))
    output$my_map <- renderLeaflet({
      
      isolate(hcmap("custom/world", data = df_kpi, value = my_value,
                    joinBy = c("iso-a2", "GeoId"),  name = "Map",
                    dataLabels = list(enabled = TRUE, format = "{point.name}"),
                    borderColor = "#FAFAFA", borderWidth = 2,
                    tooltip = list(valueDecimals = 2, valueSuffix = my_suffix)) %>% 
                hc_title(text = paste(input$kpi, " across the world", " ")) %>% 
                hc_subtitle(text = paste("The", input$kpi , " is calculated for all the countries with at least <b>", input$variation_count, "</b> cases.", sep = " ")))
    })
  })
  
  observeEvent(input$apply2, {
    
    if(length(input$countries) != 2){
      sendSweetAlert(
        session = session,
        title = "Information",
        text = "Please select 2 countries",
        type = "info"
      )
    }else{
      
      data_cases <- data %>% 
        arrange(`Countries and territories`, DateRep) %>% 
        select(DateRep, `Countries and territories`, Cases) %>%
        gather(key, value, Cases) %>%
        spread(`Countries and territories`, value)
      
      
      data_cases[3:length(data_cases)] <- lapply(data_cases[3:length(data_cases)], function(x) ifelse(is.na(x), 0, x))
      data_cases[3:length(data_cases)] <- lapply(data_cases[3:length(data_cases)], function(x) cumsum(x))
      
      
      mask_1 <- data_cases[,input$countries[1]] > 100
      data_1 <- data_cases[mask_1,input$countries[1]]
      data_1$index <- 1:nrow(data_1)
      
      mask_2 <- data_cases[,input$countries[2]] > 100 
      data_2 <- data_cases[mask_2,input$countries[2]]
      data_2$index <- 1:nrow(data_2)
      
      data_cases_after_100 <- data_1 %>% full_join(data_2, by = 'index') 
      
      data_cases_after_100 <- as.data.frame(data_cases_after_100)
      
      thm2 <- hc_theme(
        chart = list(backgroundColor = "#f6f6f6"),
        plotOptions = list(
          series = list(
            marker = list(enabled = FALSE)
          )
        )
      )
      output$graph_cases_after_100 <- renderHighchart({
        isolate(highchart() %>% 
                  hc_add_theme(thm2) %>% 
                  hc_title(text = 'Evolution of the number of cases after the <b>100th case</b>') %>% 
                  hc_subtitle(text = paste('Comparison between:<b>', input$countries[1], '</b>and<b>', input$countries[2], '</b>', sep = ' ')) %>% 
                  hc_xAxis(categories = data_cases_after_100$index, title = list(text = 'Number of days since the 100th case')) %>% 
                  hc_yAxis(title = list(text = 'Cumulative number of cases')) %>% 
                  hc_add_series(name = input$countries[1], data = data_cases_after_100[,input$countries[1]], color = '#00aba9') %>% 
                  hc_add_series(name = input$countries[2], data = data_cases_after_100[,input$countries[2]], color = '#ff0097')
        )
      })
      
      
      data_deaths <- data %>% 
        arrange(`Countries and territories`, DateRep) %>% 
        select(DateRep, `Countries and territories`, Deaths) %>%
        gather(key, value, Deaths) %>%
        spread(`Countries and territories`, value)
      
      
      data_deaths[3:length(data_cases)] <- lapply(data_deaths[3:length(data_cases)], function(x) ifelse(is.na(x), 0, x))
      data_deaths[3:length(data_cases)] <- lapply(data_deaths[3:length(data_cases)], function(x) cumsum(x))
      
      
      mask_1_death <- data_deaths[,input$countries[1]] > 10
      data_1_death <- data_deaths[mask_1_death,input$countries[1]]
      data_1_death$index <- 1:nrow(data_1_death)
      
      mask_2_death <- data_deaths[,input$countries[2]] > 10 
      data_2_death <- data_deaths[mask_2_death,input$countries[2]]
      data_2_death$index <- 1:nrow(data_2_death)
      
      data_deaths_after_10 <- data_1_death %>% full_join(data_2_death, by = 'index') 
      
      data_deaths_after_10 <- as.data.frame(data_deaths_after_10)
      
      output$graph_deaths_after_10 <- renderHighchart({
        isolate(highchart() %>% 
                  hc_add_theme(thm2) %>% 
                  hc_title(text = 'Evolution of the number of deaths after the <b>10th death</b>') %>% 
                  hc_subtitle(text = paste('Comparison between:<b>', input$countries[1], '</b>and<b>', input$countries[2], '</b>', sep = ' ')) %>% 
                  hc_xAxis(categories = data_deaths_after_10$index, title = list(text = 'Number of days since the 10th death')) %>% 
                  hc_yAxis(title = list(text = 'Cumulative number of deaths')) %>% 
                  hc_add_series(name = input$countries[1], data = data_deaths_after_10[,input$countries[1]], color = '#00aba9') %>% 
                  hc_add_series(name = input$countries[2], data = data_deaths_after_10[,input$countries[2]], color = '#ff0097')
        )
      })
      
      serie_1 <- data %>% 
        filter(`Countries and territories` == input$countries[1]) %>% 
        arrange(DateRep) %>% 
        mutate(sum_cum = cumsum(Cases)) %>% filter(sum_cum >=100) %>%  
        mutate(index = 1:n()) %>% 
        select(Cases,index)
      
      serie_2 <- data %>% 
        filter(`Countries and territories` == input$countries[2]) %>% 
        arrange(DateRep) %>% 
        mutate(sum_cum = cumsum(Cases)) %>% filter(sum_cum >=100) %>%  
        mutate(index = 1:n()) %>% 
        select(Cases,index)
      
      
      serie_graph <- serie_1 %>% full_join(serie_2, by = 'index')
      serie_graph <- as.data.frame(serie_graph)
      output$graph_daily_after_100 <- renderHighchart({
        isolate(highchart() %>% 
                  hc_add_theme(thm2) %>% 
                  hc_title(text = 'Daily number of cases after<b> 100 Cases.</b>') %>% 
                  hc_subtitle(text = paste('Comparison between:<b>', input$countries[1], '</b>and<b>', input$countries[2], '</b>', sep = ' ')) %>% 
                  hc_chart(type = "column") %>% 
                  hc_xAxis(categories = serie_graph$index, title = list(text = 'Number of days since the 100th case')) %>% 
                  hc_yAxis(title = list(text = "Number of cases by day")) %>% 
                  hc_add_series(name = input$countries[1], data = serie_graph$Cases.x, color = '#00aba9') %>% 
                  hc_add_series(name = input$countries[2], data = serie_graph$Cases.y, color = '#ff0097')
        )
      })
      
      
      serie_1_death <- data %>% 
        filter(`Countries and territories` == input$countries[1]) %>% 
        arrange(DateRep) %>% 
        mutate(sum_cum = cumsum(Deaths)) %>% filter(sum_cum >=10) %>%  
        mutate(index = 1:n()) %>% 
        select(Deaths,index)
      
      serie_2_deaths <- data %>% 
        filter(`Countries and territories` == input$countries[2]) %>% 
        arrange(DateRep) %>% 
        mutate(sum_cum = cumsum(Deaths)) %>% filter(sum_cum >=10) %>%  
        mutate(index = 1:n()) %>% 
        select(Deaths,index)
      
      
      serie_graph_deaths <- serie_1_death %>% full_join(serie_2_deaths, by = 'index')
      serie_graph_deaths <- as.data.frame(serie_graph_deaths)
      output$graph_daily_after_10 <- renderHighchart({
        isolate(highchart() %>% 
                  hc_add_theme(thm2) %>% 
                  hc_title(text = 'Daily number of deaths after<b> 10 Deaths.</b>') %>% 
                  hc_subtitle(text = paste('Comparison between:<b>', input$countries[1], '</b>and<b>', input$countries[2], '</b>', sep = ' ')) %>% 
                  hc_chart(type = "column") %>% 
                  hc_xAxis(categories = serie_graph_deaths$index, title = list(text = 'Number of days since the 10th death')) %>% 
                  hc_yAxis(title = list(text = "Number of deaths by day")) %>% 
                  hc_add_series(name = input$countries[1], data = serie_graph_deaths$Deaths.x, color = '#00aba9') %>% 
                  hc_add_series(name = input$countries[2], data = serie_graph_deaths$Deaths.y, color = '#ff0097')
        )
      })
      
    }  
  })
  
  
}