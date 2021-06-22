library(shiny)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)
library(fst)
library(plotly)
library(viridis)

# Load data 
pred_agg_startup <- read.fst("pat_pred_top_startups_16_0.9.fst") 

# Define UI 
ui <- fluidPage(
    htmltools::htmlDependency("jquery", "3.5.1",
                              src = c(href = "https://code.jquery.com/"),
                              script = "jquery-3.5.1.min.js"),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),

    # Choose startup and model
    fluidRow(  id = "startup_id", 
               pickerInput(
                   inputId = "startup_pat", 
                   label = "Choose a firm", 
                   choices = sort(unique(pred_agg_startup$organization_ctry)), 
                   selected = c("moderna (US)", "therapeuticsmd (US)"), 
                   options = list(
                       # `max-options` = 8,
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3",
                       `count-selected-text` = "Company",
                       `deselect-all-text` = "Deselect all",
                       `select-all-text` = "Select all",
                       `none-selected-text` = 'No company selected'), 
                   multiple = TRUE),
               pickerInput(
                   inputId = "startup_model", 
                   label = "Choose a model", 
                   choices = unique(pred_agg_startup$model)[!(unique(pred_agg_startup$model) %in% "past")], 
                   selected = sort(unique(pred_agg_startup$model)), 
                   options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3",
                       `count-selected-text` = "Model",
                       `deselect-all-text` = "Deselect all",
                       `select-all-text` = "Select all",
                       `none-selected-text` = 'No model selected',`live-search`=TRUE
                   ), 
                   multiple = TRUE),
    # create plot
    mainPanel(plotlyOutput("startup_plot")))
)




# Define server 
server <- function(input, output, session) {
    
    desc <- reactive({ifelse(session$clientData$pixelratio > 2, "2020\n(today)", "2020 (today)")})
    

    
    
    # make the plot for startups
    dat_set_startup <-  reactive({filter(pred_agg_startup, pub_year > 2016 & model %in% c(input$startup_model) & organization_ctry %in% input$startup_pat)})
    
    
    output$startup_plot <- renderPlotly({
        if(nrow(dat_set_startup())!= 0){   
            
           p_1 <-  ggplotly(
                ggplot(data = filter(dat_set_startup()), aes(x = as.factor(model), y = rel_class, color = as.factor(organization), shape = as.factor(model), group = as.factor(interaction(organization, model)), label = Firm)) +
                    geom_point(data = filter(dat_set_startup(), pub_year > 2016), aes(x = as.factor(model), y = rel_class, color = as.factor(organization), shape = as.factor(model), group = as.factor(interaction(organization, model))), size = 2, alpha = 0.7) +
                    scale_color_viridis(discrete = T, begin = 0, end = 0.9) +
                    xlab("") +
                    ylab("Share of top patents") +
                    theme_bw() +
                    theme(axis.title = element_text(face="bold",size = 10),
                          legend.title = element_blank()) +
                        scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)),  
                    # geom_vline(xintercept = 2015, linetype="dotted"),
                dynamicTicks = FALSE, tooltip = c("label")) %>% config(displayModeBar = F) %>%
               layout(xaxis = list(fixedrange=T),
                      yaxis = list(fixedrange=T),
                      legend = list(orientation = "h", y = -0.3, showlegend = F)) 
           
           lapply(seq(1, length(p_1$x$data)), function(y) p_1$x$data[[y]]$showlegend <<- FALSE)
           lapply(seq(1, c(length(input$startup_model))*length(input$startup_pat), length(input$startup_model)), function(y) p_1$x$data[[y]]$showlegend <<- TRUE)
           lapply(seq(1, c(length(input$startup_model))*length(input$startup_pat), length(input$startup_model)), function(y) p_1$x$data[[y]]$name <<- gsub(paste(c("Mean", "[//(//)]", "\\,"), collapse = "|"), "", p_1$x$data[[y]]$name))
           
           
           p_1
        
        } else {}
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
