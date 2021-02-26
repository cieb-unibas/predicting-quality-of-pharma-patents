library(shiny)
library(ggplot2)
library(shinyWidgets)
library(fst)
library(plotly)
library(viridis)

# Load data 
pred_agg_ctry  <- read.fst("pat_pred_top_ctry_16_0.9.fst")
pred_agg_regio <- read.fst("pat_pred_top_regio_16_0.9.fst")

# Define UI 
ui <- fluidPage(
    htmltools::htmlDependency("jquery", "3.5.1",
                              src = c(href = "https://code.jquery.com/"),
                              script = "jquery-3.5.1.min.js"),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    
    
# Choose country and model
   fluidRow(column = 12,
            id = "ctry_pat", 
                            pickerInput(
                                inputId = "ctry_pat_input", 
                                label = "Choose a country",
                                choices = unique(sort(pred_agg_ctry$country)), 
                                selected = c("China", "Germany", "Switzerland", "United States", "Japan", "United Kingdom"), 
                                options = list(
                                    # `max-options` = 8,
                                    `actions-box` = TRUE, 
                                    size = 10,
                                    `selected-text-format` = "count > 3",
                                    `count-selected-text` = "Country",
                                    `deselect-all-text` = "Deselct all",
                                    `select-all-text` = "Select all",
                                    `none-selected-text` = 'No country selected'), 
                                multiple = TRUE),
                            pickerInput(
                                inputId = "model", 
                                label = "Choose a model",
                                choices = unique(pred_agg_ctry$model)[!(unique(pred_agg_ctry$model) %in% "past")], 
                                selected = "Mean of all models", 
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

mainPanel(plotlyOutput("ctry_pat_plot", width = "100%"))),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),  

# Choose regio and model
fluidRow( column = 12,
          id = "regio_pat",
           pickerInput(
               inputId = "regio_pat_input",
               label = "Choose a region",
               choices = sort(unique(pred_agg_regio$regio)), 
               selected = c("CH-Northwestern Switzerland", "CH-Lake Geneva Region", "CN-Shanghai", "US-California", "US-Massachusetts", "US-Connecticut"), 
               options = list(
                   # `max-options` = 8,
                   `actions-box` = TRUE, 
                   size = 10,
                   `selected-text-format` = "count > 3",
                   `count-selected-text` = "Region",
                   `deselect-all-text` = "Deselct all",
                   `select-all-text` = "Select all",
                   `none-selected-text` = 'No region selected'), 
               multiple = TRUE),
           pickerInput(
               inputId = "regio_model", 
               label = "Choose a model", 
               choices = unique(pred_agg_regio$model)[!(unique(pred_agg_regio$model) %in% "past")], 
               selected = "Mean of all models", 
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
mainPanel(plotlyOutput("regio_pat_plot", width = "100%")),
br(),
br(),
br(),
br(),
br(),
br(),
br(),
br()))


# Define server 
server <- function(input, output, session) {

    desc <- reactive({ifelse(session$clientData$pixelratio > 2, "2020\n(today)", "2020 (today)")})

    # Coutry-pat-plot
    dat_set <-  reactive({filter(pred_agg_ctry, pub_year > 2005 & model %in% c("past", input$model) & country %in% input$ctry_pat_input)})
    
    # make the plot
    output$ctry_pat_plot <- renderPlotly({
        if(nrow(dat_set())!= 0){   
            
        p <-  ggplotly(
                ggplot(data = filter(dat_set()), aes(x = pub_year, y = rel_class, color = as.factor(country), shape = as.factor(model), group = as.factor(interaction(country, model)), label = Country)) +
                    geom_line(data = filter(dat_set(), pub_year < 2016), aes(x = pub_year, y = rel_class, color = as.factor(country), linetype = as.factor(country), group = as.factor(interaction(country, model)))) +
                    geom_point(data = filter(dat_set(), pub_year == 2017), aes(x = pub_year, y = rel_class, color = as.factor(country), shape = as.factor(model), group = as.factor(interaction(country, model)), size = ges_pred_pat), alpha = 0.5) +
                    scale_color_viridis(discrete = T, begin = 0, end = 0.9) +
                    xlab("year") +
                    ylab("Share of top patents") +
                    ggplot2::annotate(geom="text", x=2010, y=round(max(dat_set()$rel_class), 2) + 0.04, label="Past observations",
                             color="black") + 
                    ggplot2::annotate(geom="text", x=2017, y=round(max(dat_set()$rel_class), 2) + 0.04, label="Predictions",
                             color="black") + 
                    scale_x_continuous(limits = c(2007, 2018), breaks = c(2005, 2010, 2015, 2017), labels = c("2005", "2010", "2015", desc())) +
                    scale_y_continuous(breaks = seq(0, round(max(dat_set()$rel_class), 2) + 0.05, 0.05), limits = c(0, max(dat_set()$rel_class + 0.05))) +  
                    # geom_vline(xintercept = 2015, linetype="dotted") +
                    theme_bw() +
                    theme(axis.title = element_text(face="bold",size = 10),
                          legend.title = element_blank(),
                          legend.position = "bottom"),
                dynamicTicks = F, tooltip = c("label")) %>% config(displayModeBar = F) %>%
                layout(xaxis = list(fixedrange=T)) %>%
                layout(yaxis = list(fixedrange=T),
                       legend = list(orientation = "h", y = -0.3)) 
            
            lapply(seq(length(input$ctry_pat_input) + 1, (1+length(input$model))*length(input$ctry_pat_input)), function(y) p$x$data[[y]]$showlegend <<- FALSE)
            lapply(seq(1, length(input$ctry_pat_input)), function(y) p$x$data[[y]]$name <<- gsub(paste(c("past", "[//(//)]", "\\,"), collapse = "|"), "", p$x$data[[y]]$name))
            
            p
        } else {}
    })
    
    
    # regio-pat-plot
    dat_set_regio <-  reactive({filter(pred_agg_regio, pub_year > 2005 & model %in% c("past", input$regio_model) & regio %in% input$regio_pat_input)})
    
    # make the plot
    output$regio_pat_plot <- renderPlotly({
        if(nrow(dat_set_regio())!= 0){   
            
        p_1 <- ggplotly(
                ggplot(data = filter(dat_set_regio()), aes(x = pub_year, y = rel_class, color = as.factor(regio), shape = as.factor(model), group = as.factor(interaction(regio, model)), label = Regio)) +
                    geom_line(data = filter(dat_set_regio(), pub_year < 2016), aes(x = pub_year, y = rel_class, color = as.factor(regio), linetype = as.factor(regio), group = as.factor(interaction(regio, model)))) +
                    geom_point(data = filter(dat_set_regio(), pub_year == 2017), aes(x = pub_year, y = rel_class, color = as.factor(regio), shape = as.factor(model), group = as.factor(interaction(regio, model)), size = ges_pred_pat), alpha = 0.5) +
                    scale_color_viridis(discrete = T, begin = 0, end = 0.9) +
                    xlab("year") +
                    ylab("Share of top patents") +
                    ggplot2::annotate(geom="text", x=2010, y=round(max(dat_set_regio()$rel_class), 2) + 0.04, label="Past observations",
                               color="black") + 
                    ggplot2::annotate(geom="text", x=2017, y=round(max(dat_set_regio()$rel_class), 2) + 0.04, label="Predictions",
                             color="black") + 
                    scale_x_continuous(limits = c(2007, 2018), breaks = c(2005, 2010, 2015, 2017), labels = c("2005", "2010", "2015", desc())) +
                    scale_y_continuous(breaks = seq(0, round(max(dat_set_regio()$rel_class), 2) + 0.05, 0.05), limits = c(0, max(dat_set_regio()$rel_class + 0.05))) +  
                    # geom_vline(xintercept = 2015, linetype="dotted") +
                    theme_bw() +
                    theme(axis.title = element_text(face="bold",size = 10),
                          legend.title = element_blank()),
                dynamicTicks = F, tooltip = c("label")) %>% config(displayModeBar = F) %>%
                layout(xaxis = list(fixedrange=T)) %>%
                layout(yaxis = list(fixedrange=T),
                      legend = list(orientation = "h", y = -0.32, showlegend = F)) 
       
        lapply(seq(length(input$regio_pat_input) + 1, (1+length(input$regio_model))*length(input$regio_pat_input)), function(y) p_1$x$data[[y]]$showlegend <<- FALSE)
        lapply(seq(1, length(input$regio_pat_input)), function(y) p_1$x$data[[y]]$name <<- gsub(paste(c("past", "[//(//)]", "\\,"), collapse = "|"), "", p_1$x$data[[y]]$name))
        
        p_1
         
            
        } else {}
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
