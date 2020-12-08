library(shiny)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)
library(fst)
library(plotly)
library(viridis)
require(dplyr)

# Load data 
pred_agg_firm <- read.fst("pat_pred_top_firm_16_0.9.fst") 
pred_agg_firm <- filter(pred_agg_firm, model %in% c("past", "C_3_model_2", "C_3_model_3", "C_3_model_4", "C_3_model_5", "C_3_prob", "model_three"))
pred_agg_firm <- mutate(pred_agg_firm, info = paste0("Firm: ", organization, "\nModel: ", model))

# Define some functions

# Define UI 
ui <- fluidPage(
    htmltools::htmlDependency("jquery", "3.5.1",
                              src = c(href = "https://code.jquery.com/"),
                              script = "jquery-3.5.1.min.js"),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    
    
    # Choose organization and model
    fluidPage(fluidRow(
        column(6,
               pickerInput(
                   inputId = "firm_pat", 
                   # label = "Auswahl von Ländern", 
                   choices = sort(unique(pred_agg_firm$organization)), 
                   selected = c("novartis", "hoffmann la roche", "pfizer"), 
                   options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3",
                       `count-selected-text` = "Company",
                       `deselect-all-text` = "Deselect all",
                       `select-all-text` = "Select all",
                       `none-selected-text` = 'No company selected'), 
                   multiple = TRUE)),
        column(6,
               pickerInput(
                   inputId = "model", 
                   # label = "Auswahl von Modellen", 
                   choices = sort(unique(pred_agg_firm$model)), 
                   selected = sort(unique(pred_agg_firm$model)), 
                   options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3",
                       `count-selected-text` = "Model",
                       `deselect-all-text` = "Deselect all",
                       `select-all-text` = "Select all",
                       `none-selected-text` = 'No model selected',`live-search`=TRUE
                   ), 
                   multiple = TRUE))
    )),
    # create plot
    mainPanel(plotlyOutput("firm_pat_plot")))
# Define server 
server <- function(input, output) {
    
    # Coutry-pat-plot
    dat_set <-  reactive({filter(pred_agg_firm, pub_year > 2005 & model %in% c("past", input$model) & organization %in% input$firm_pat)})
    
    # make the plot
    output$firm_pat_plot <- renderPlotly({
        if(nrow(dat_set())!= 0){   
            
            p <-  ggplotly(
                ggplot(data = filter(dat_set()), aes(x = pub_year, y = rel_class, color = as.factor(organization), shape = as.factor(model), group = as.factor(interaction(organization, model)), label = info)) +
                    geom_line(data = filter(dat_set(), pub_year < 2019), aes(x = pub_year, y = rel_class, color = as.factor(organization), shape = as.factor(model), group = as.factor(interaction(organization, model)))) +
                    # geom_text(data = filter(dat_set(), pub_year > 2016), aes(x = pub_year, y = rel_class, color = as.factor(organization), shape = as.factor(model), group = as.factor(interaction(organization, model)), size = 1, label = organization), position = position_nudge(y = -0.005)) +
                    geom_point(data = filter(dat_set(), pub_year > 2016), aes(x = pub_year, y = rel_class, color = as.factor(organization), shape = as.factor(model), group = as.factor(interaction(organization, model))), size = 2) +
                    scale_color_viridis(discrete = T, begin = 0, end = 0.8) +
                    xlab("Jahr") +
                    ylab("Anteil Top-Patente") +
                    scale_x_continuous(breaks = c(2005, 2010, 2015)) +  
                    scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +  
                    geom_vline(xintercept = 2016, linetype="dotted") +
                    theme(legend.title = element_blank(), legend.position = "none"),
                dynamicTicks = FALSE, height = 480, width=800,  tooltip = c("label")) %>% config(displayModeBar = F) %>%
                layout(xaxis = list(fixedrange=F)) %>%
                layout(yaxis = list(fixedrange=F)) 
            
            p
        } else {}
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
