library(shiny)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)
library(fst)
library(plotly)
library(viridis)

# Load data 
pred_agg_firm <- read.fst("pat_pred_top_firm_16_0.9.fst") 

# Define UI 
ui <- fluidPage(
    htmltools::htmlDependency("jquery", "3.5.1",
                              src = c(href = "https://code.jquery.com/"),
                              script = "jquery-3.5.1.min.js"),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),

    
   # Choose organization and model
   fluidRow(
              column = 12,
              id = "firm_id", 
              pickerInput(
                   inputId = "firm_pat", 
                   label = "Choose a firm", 
                   choices = sort(unique(pred_agg_firm$organization_ctry)), 
                   selected = c("novartis (CH)", "hoffmann la roche (CH)", "pfizer (US)", "allergan (US)", "eli lilly (US)"), 
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
                   inputId = "model", 
                   label = "Choose a model",
                   choices = unique(pred_agg_firm$model)[!(unique(pred_agg_firm$model) %in% "past")], 
                   selected = "Mean of all models", 
                   options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3",
                       `count-selected-text` = "Model",
                       `deselect-all-text` = "Deselect all",
                       `select-all-text` = "Select all",
                       `none-selected-text` = 'No model selected'), 
                   multiple = TRUE),
    # create plot
   mainPanel( 
   plotlyOutput("firm_pat_plot")))
)




# Define server 
server <- function(input, output, session) {
    
    desc <- reactive({ifelse(session$clientData$pixelratio > 2, "2020\n(today)", "2020 (today)")})
    
    # Firm-pat-plot
    dat_set <-  reactive({filter(pred_agg_firm, pub_year > 2005 & model %in% c("past", input$model) & organization_ctry %in% input$firm_pat)})
    
    # make the plot for firms
    output$firm_pat_plot <- renderPlotly({
        if(nrow(dat_set())!= 0){   
     p <-    ggplotly(
                ggplot(data = filter(dat_set()), aes(x = pub_year, y = rel_class, color = as.factor(organization), shape = as.factor(model), group = as.factor(interaction(organization, model)), label = Firm)) +
                    geom_line(data = filter(dat_set(), pub_year < 2016), aes(x = pub_year, y = rel_class, color = as.factor(organization), linetype = as.factor(organization), shape = as.factor(model), group = as.factor(interaction(organization, model)))) +
                    geom_point(data = filter(dat_set(), pub_year == 2017), aes(x = pub_year, y = rel_class, color = as.factor(organization), shape = as.factor(model), group = as.factor(interaction(organization, model))), size = 2, alpha = 0.5) +
                    scale_color_viridis(discrete = T, begin = 0, end = 0.9) +
                    theme_bw() +
                    xlab("year") +
                    ylab("Share of top patents") +
                    ggplot2::annotate(geom="text", x=2010, y=round(max(dat_set()$rel_class), 2) + 0.04, label="Past observations",
                             color="black") + 
                    ggplot2::annotate(geom="text", x=2017, y=round(max(dat_set()$rel_class), 2) + 0.04, label="Predictions",
                             color="black") + 
                    scale_x_continuous(limits = c(2007, 2018), breaks = c(2005, 2010, 2015, 2017), labels = c("2005", "2010", "2015", desc())) +
                    scale_y_continuous(breaks = seq(0, round(max(dat_set()$rel_class), 2) + 0.05, 0.05), limits = c(0, max(dat_set()$rel_class + 0.05))) +  
                    # geom_vline(xintercept = 2015, linetype="dotted") +
                    theme(axis.title = element_text(face="bold",size = 10),
                          legend.title = element_blank()),
                dynamicTicks = F, tooltip = c("label")) %>% config(displayModeBar = F) %>%
                layout(xaxis = list(fixedrange=T),
                       yaxis = list(fixedrange=T),
                       legend = list(orientation = "h", y = -0.3)) 
    
     lapply(seq(1, length(p$x$data)), function(y) p$x$data[[y]]$showlegend <<- FALSE)
     lapply(seq(1, length(input$firm_pat)), function(y) p$x$data[[y]]$showlegend <<- TRUE)
     lapply(seq(1, length(input$firm_pat)), function(y) p$x$data[[y]]$name <<- gsub(paste(c("past", "[//(//)]", "\\,"), collapse = "|"), "", p$x$data[[y]]$name))

     p
            
        } else {}
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
