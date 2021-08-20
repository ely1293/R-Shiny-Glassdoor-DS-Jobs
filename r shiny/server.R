library(shiny)
library(spData)
library(tidyverse)
library(sf)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$city <- renderPlot({
        df %>%
            filter(State == input$selectState) %>%
            group_by(Location) %>%
            count() %>%
            arrange(desc(n)) %>%
            head(15) %>%
            ggplot(aes(x = reorder(Location, n), y = n)) +
            geom_col(aes(fill = Location)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "City",
                 y = "# of Jobs")
    })
    
    output$city2 <- renderPlot({
        df %>%
            filter(State == input$selectState2) %>%
            group_by(Location) %>%
            count() %>%
            arrange(desc(n)) %>%
            head(15) %>%
            ggplot(aes(x = reorder(Location, n), y = n)) +
            geom_col(aes(fill = Location)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "City",
                 y = "# of Jobs")
    })
    
    output$salary <- renderPlot({
        df %>%
            pivot_longer(
                cols = SalaryLow:SalaryHigh,
                names_to = "SalaryType",
                values_to = "SalaryAmount"
            ) %>%
            filter(State == input$selectState &
                       SalaryAmount != 0) %>%
            ggplot(aes(x = SalaryAmount, fill = SalaryType)) +
            geom_density(alpha = 0.3) +
            labs(x = "Salary",
                 y = "",
                 fill = "")
    })
    
    output$salary2 <- renderPlot({
        df %>%
            pivot_longer(
                cols = SalaryLow:SalaryHigh,
                names_to = "SalaryType",
                values_to = "SalaryAmount"
            ) %>%
            filter(State == input$selectState2 &
                       SalaryAmount != 0) %>%
            ggplot(aes(x = SalaryAmount, fill = SalaryType)) +
            geom_density(alpha = 0.3) +
            labs(x = "Salary",
                 y = "",
                 fill = "")
    })
    
    output$position <- renderPlot({
        df %>%
            filter(State == input$selectState) %>%
            group_by(JobFunction) %>%
            count() %>%
            arrange(desc(n)) %>%
            head(15) %>%
            ggplot(aes(x = reorder(JobFunction, n), y = n)) +
            geom_col(aes(fill = JobFunction)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "Position",
                 y = "# of Jobs")
    })
    
    output$position2 <- renderPlot({
        df %>%
            filter(State == input$selectState2) %>%
            group_by(JobFunction) %>%
            count() %>%
            arrange(desc(n)) %>%
            head(15) %>%
            ggplot(aes(x = reorder(JobFunction, n), y = n)) +
            geom_col(aes(fill = JobFunction)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "Position",
                 y = "# of Jobs")
    })
    
    output$type <- renderPlot({
        df %>%
            filter(State == input$selectState &
                       JobType != 0) %>%
            group_by(JobType) %>%
            count() %>%
            ggplot(aes(x = reorder(JobType, n), y = n)) +
            geom_col(aes(fill = JobType)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "Type",
                 y = "# of Jobs")
    })
    
    output$type2 <- renderPlot({
        df %>%
            filter(State == input$selectState2 &
                       JobType != 0) %>%
            group_by(JobType) %>%
            count() %>%
            ggplot(aes(x = reorder(JobType, n), y = n)) +
            geom_col(aes(fill = JobType)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "Type",
                 y = "# of Jobs")
    })
    
    output$company <- renderPlot({
        df %>%
            filter(State == input$selectState) %>%
            group_by(Company) %>%
            count() %>%
            arrange(desc(n)) %>%
            head(15) %>%
            ggplot(aes(x = reorder(Company, n), y = n)) +
            geom_col(aes(fill = Company)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "Company",
                 y = "# of Jobs")
    })
    
    output$company2 <- renderPlot({
        df %>%
            filter(State == input$selectState2) %>%
            group_by(Company) %>%
            count() %>%
            arrange(desc(n)) %>%
            head(15) %>%
            ggplot(aes(x = reorder(Company, n), y = n)) +
            geom_col(aes(fill = Company)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "Company",
                 y = "# of Jobs")
    })
    
    output$industry <- renderPlot({
        df %>%
            filter(State == input$selectState &
                       Industry != 0) %>%
            group_by(Industry) %>%
            count() %>%
            arrange(desc(n)) %>%
            head(15) %>%
            ggplot(aes(x = reorder(Industry, n), y = n)) +
            geom_col(aes(fill = Industry)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "Industry",
                 y = "# of Jobs")
    })
    
    output$industry2 <- renderPlot({
        df %>%
            filter(State == input$selectState2 &
                       Industry != 0) %>%
            group_by(Industry) %>%
            count() %>%
            arrange(desc(n)) %>%
            head(15) %>%
            ggplot(aes(x = reorder(Industry, n), y = n)) +
            geom_col(aes(fill = Industry)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "Industry",
                 y = "# of Jobs")
    })
    
    output$education <- renderPlot({
        df %>%
            pivot_longer(cols = BS:PhD,
                         names_to = "Education",
                         values_to = "n_jobs") %>%
            filter(State == input$selectState) %>%
            group_by(Education) %>%
            ggplot(aes(x = reorder(Education, -n_jobs), y = n_jobs)) +
            geom_col(aes(fill = Education)) +
            theme(legend.position = "none") +
            labs(x = "Euducation",
                 y = "# of Jobs")
    })
    
    output$education2 <- renderPlot({
        df %>%
            pivot_longer(cols = BS:PhD,
                         names_to = "Education",
                         values_to = "n_jobs") %>%
            filter(State == input$selectState2) %>%
            group_by(Education) %>%
            ggplot(aes(x = reorder(Education, -n_jobs), y = n_jobs)) +
            geom_col(aes(fill = Education)) +
            theme(legend.position = "none") +
            labs(x = "Euducation",
                 y = "# of Jobs")
    })
    
    output$skills <- renderPlot({
        df %>%
            pivot_longer(
                cols = Python:Hive,
                names_to = "Skills",
                values_to = "n_jobs"
            ) %>%
            filter(State == input$selectState) %>%
            group_by(Skills) %>%
            ggplot(aes(x = reorder(Skills, n_jobs), y = n_jobs)) +
            geom_col(aes(fill = Skills)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "Skills",
                 y = "# of Jobs")
    })
    
    output$skills2 <- renderPlot({
        df %>%
            pivot_longer(
                cols = Python:Hive,
                names_to = "Skills",
                values_to = "n_jobs"
            ) %>%
            filter(State == input$selectState2) %>%
            group_by(Skills) %>%
            ggplot(aes(x = reorder(Skills, n_jobs), y = n_jobs)) +
            geom_col(aes(fill = Skills)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "Skills",
                 y = "# of Jobs")
    })
    
    output$table <- renderDataTable({
        df$url <-
            paste0("<a href='",
                   df$url,
                   "' target='_blank'>",
                   df$Role,
                   "</a>")
        state_df <- df %>%
            filter(State == input$selectState)
        datatable(
            select(state_df, c(Company:url)),
            escape = FALSE,
            options = list(autoWidth = TRUE, scrollX = TRUE)
        )
    })
    
    output$table2 <- renderDataTable({
        df$url <-
            paste0("<a href='",
                   df$url,
                   "' target='_blank'>",
                   df$Role,
                   "</a>")
        state_df2 <- df %>%
            filter(State == input$selectState2)
        datatable(
            select(state_df2, c(Company:url)),
            escape = FALSE,
            options = list(autoWidth = TRUE, scrollX = TRUE)
        )
    })
    
    mapData <- us_states
    
    #Map
    output$map1 <- renderLeaflet({
        state_tot <- group_by(df, FullState) %>%
            summarise(count = n())
        state_tot <-
            left_join(state_tot, mapData, c("FullState" = "NAME"))
        pal <-
            colorNumeric(palette = "YlGnBu", domain = state_tot$count)
        
        map <- leaflet() %>%
            addTiles() %>%
            setView(lng = -93.85,
                    lat = 37.45,
                    zoom = 4) %>%
            addPolygons(
                data = state_tot$geometry,
                fillColor = pal(state_tot$count),
                fillOpacity = .7,
                color = "grey",
                weight = 1,
                popup = paste0(
                    '<b>State: </b>',
                    state_tot$FullState,
                    '<br>',
                    '<b>Jobs: </b>',
                    state_tot$count,
                    '<br>'
                )
            ) %>%
            addLegend(pal = pal,
                      values = state_tot$count,
                      position = "bottomleft")
        
    })
    
    output$map2 <- renderLeaflet({
        salary_state <- df %>%
            group_by(FullState) %>%
            summarize(SalaryMean = mean(SalaryAvg, na.rm = TRUE))
        salary_state <-
            left_join(salary_state, mapData, c("FullState" = "NAME"))
        pal <-
            colorNumeric(palette = "YlGnBu", domain = salary_state$SalaryMean)

        map <- leaflet() %>%
            addTiles() %>%
            setView(lng = -93.85,
                    lat = 37.45,
                    zoom = 4) %>%
            addPolygons(
                data = salary_state$geometry,
                fillColor = pal(salary_state$SalaryMean),
                fillOpacity = .7,
                color = "grey",
                weight = 1,
                popup = paste0(
                    '<b>State: </b>',
                    salary_state$FullState,
                    '<br>',
                    '<b>Mean Salary: </b>',
                    round(salary_state$SalaryMean, 0),
                    '<br>'
                )
            ) %>%
            addLegend(pal = pal,
                      values = salary_state$SalaryMean,
                      position = "bottomleft")

    })
    
    output$city_tot <- renderPlot({
        df %>%
            group_by(Location) %>%
            count() %>%
            arrange(desc(n)) %>%
            head(20) %>%
            ggplot(aes(x = reorder(Location, n), y = n)) +
            geom_col(aes(fill = Location)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "City",
                 y = "# of Jobs")
    })
    
    output$position_tot <- renderPlot({
        df %>%
            filter(JobFunction != 0 ) %>%
            group_by(JobFunction) %>%
            count() %>%
            arrange(desc(n)) %>%
            head(15) %>%
            ggplot(aes(x = reorder(JobFunction, n), y = n)) +
            geom_col(aes(fill = JobFunction)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "Position",
                 y = "# of Jobs")
    })
    
    output$company_tot <- renderPlot({
        df %>%
            group_by(Company) %>%
            count() %>%
            arrange(desc(n)) %>%
            head(20) %>%
            ggplot(aes(x = reorder(Company, n), y = n)) +
            geom_col(aes(fill = Company)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "Company",
                 y = "# of Jobs")
    })
    
    output$industry_tot <- renderPlot({
        df %>%
            filter(Industry != 0) %>%
            group_by(Industry) %>%
            count() %>%
            arrange(desc(n)) %>%
            ggplot(aes(x = reorder(Industry, n), y = n)) +
            geom_col(aes(fill = Industry)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "Industry",
                 y = "# of Jobs")
    })
    
    output$type_tot <- renderPlot({
        df %>%
            filter(JobType != 0) %>%
            count() %>%
            ggplot(aes(x = reorder(JobType, n), y = n)) +
            geom_col(aes(fill = JobType)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "Type",
                 y = "# of Jobs")
    })
    
    output$education_tot <- renderPlot({
        df %>%
            pivot_longer(cols = BS:PhD,
                         names_to = "Education",
                         values_to = "n_jobs") %>%
            group_by(Education) %>%
            ggplot(aes(x = reorder(Education, -n_jobs), y = n_jobs)) +
            geom_col(aes(fill = Education)) +
            theme(legend.position = "none") +
            labs(x = "Euducation",
                 y = "# of Jobs")
    })
    
    output$skills_tot <- renderPlot({
        df %>%
            pivot_longer(
                cols = Python:Hive,
                names_to = "Skills",
                values_to = "n_jobs"
            ) %>%
            group_by(Skills) %>%
            ggplot(aes(x = reorder(Skills, n_jobs), y = n_jobs)) +
            geom_col(aes(fill = Skills)) +
            coord_flip() +
            theme(legend.position = "none") +
            labs(x = "Skills",
                 y = "# of Jobs")
    })
    
    output$table <- renderDataTable({
        df$url <-
            paste0("<a href='",
                   df$url,
                   "' target='_blank'>",
                   df$Role,
                   "</a>")
        datatable(
            select(df, c(Company:url)),
            escape = FALSE,
            options = list(autoWidth = TRUE, scrollX = TRUE)
        )
    })
    
    
    
})
