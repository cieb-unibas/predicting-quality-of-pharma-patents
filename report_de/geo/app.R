library(shiny)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)
library(fst)
library(plotly)
library(viridis)
require(dplyr)

# Load data 
pred_agg_ctry <- read.fst("pat_pred_ctry_16.fst") 
pred_agg_ctry <- rename(pred_agg_ctry, iso = country, country = Land)
pred_agg_ctry <- filter(pred_agg_ctry, model %in% c("past", "C_3_model_2", "C_3_model_3", "C_3_model_4", "C_3_model_5", "C_3_prob", "model_three"))

# Define some functions




# Define UI 
ui <- fluidPage(
    htmltools::htmlDependency("jquery", "3.5.1",
                              src = c(href = "https://code.jquery.com/"),
                              script = "jquery-3.5.1.min.js"),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    
    
# Choose country and model
   fluidPage(fluidRow(
                     column(6,
                            pickerInput(
                                inputId = "ctry_pat", 
                                # label = "Auswahl von Ländern", 
                                choices = sort(unique(pred_agg_ctry$country)), 
                                selected = c("China", "Germany", "Switzerland", "United States"), 
                                options = list(
                                    `actions-box` = TRUE, 
                                    size = 10,
                                    `selected-text-format` = "count > 3",
                                    `count-selected-text` = "country",
                                    `deselect-all-text` = "Alle abwählen",
                                    `select-all-text` = "Alle auswählen",
                                    `none-selected-text` = 'Kein Land ausgewählt'), 
                                multiple = TRUE)),
                     column(6,
                            pickerInput(
                                inputId = "model", 
                                # label = "Auswahl von Modellen", 
                                choices = sort(unique(pred_agg_ctry$model)), 
                                selected = c("past", "C_3_model_2"), 
                                options = list(
                                    `actions-box` = TRUE, 
                                    size = 10,
                                    `selected-text-format` = "count > 3",
                                    `count-selected-text` = "Modell",
                                    `deselect-all-text` = "Alle abwählen",
                                    `select-all-text` = "Alle auswählen",
                                    `none-selected-text` = 'Kein Modell ausgewählt',`live-search`=TRUE
                                ), 
                                multiple = TRUE))
                 )),
     # create plot
     mainPanel(plotlyOutput("ctry_pat_plot")))

# Define server 
server <- function(input, output) {
    
    # Coutry-pat-plot
    dat_set <-  reactive({filter(pred_agg_ctry, pub_year > 2005 & model %in% c("past", input$model) & country %in% input$ctry_pat)})
    
    # make the plot
    output$ctry_pat_plot <- renderPlotly({
        if(nrow(dat_set())!= 0){   
            
            p <-  ggplotly(
                ggplot(data = filter(dat_set()), aes(x = pub_year, y = rel_top_past_year_5, color = as.factor(country), shape = as.factor(model), group = as.factor(interaction(country, model)), label = country)) +
                    geom_line(data = filter(dat_set(), pub_year < 2019), aes(x = pub_year, y = rel_top_past_year_5, color = as.factor(country), shape = as.factor(model), group = as.factor(interaction(country, model)))) +
                    geom_text(data = filter(dat_set(), pub_year == 2012), aes(x = pub_year, y = rel_top_past_year_5, color = as.factor(country), shape = as.factor(model), group = as.factor(interaction(country, model)), size = 1, label = country), position = position_nudge(y = -0.005)) +
                    geom_point(data = filter(dat_set(), pub_year == 2019), aes(x = pub_year, y = rel_top_past_year_5, color = as.factor(country), shape = as.factor(model), group = as.factor(interaction(country, model))), size = 2) +
                    scale_color_viridis(discrete = T, begin = 0, end = 0.8) +
                    xlab("Jahr") +
                    ylab("Anteil Top-Patente") +
                    scale_x_continuous(breaks = c(2005, 2010, 2015)) +  
                    scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2), limits = c(0, max(dat_set()$rel_top_past_year_5 + 0.01))) +  
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
