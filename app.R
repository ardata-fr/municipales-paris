library(shiny)
library(shinyWidgets)
library(ggplot2)
library(ggiraph)
library(flextable)

source("Rscripts/flextable_tooltip.R")
source("Rscripts/summary_data.R")

# get data ---- 
paris_arr <- readRDS(file = "data/paris_arr.RDS")
nuances_col <- readRDS(file = "data/nuances_col.RDS")
scores <- readRDS(file = "data/scores.RDS")
summ_detail <- readRDS(file = "data/summ_detail.RDS")
flextables_tour_secteur <- readRDS(file = "data/flextables_tour_secteur.RDS")


# for scales colors ----
scale_values <- setNames(nuances_col$fill, nuances_col$nuance)
scale_labels <- setNames(nuances_col$lib_nuance, nuances_col$nuance)
scale_data_id <- setNames(nuances_col$nuance, nuances_col$nuance)
scale_barplot_interactive <- scale_fill_manual_interactive(
    values = scale_values, labels = scale_labels, data_id = scale_data_id)

# base map ----
CarteSimple <- ggplot() +
    geom_sf(data = paris_arr, colour = "black", fill="transparent", size = .2) + 
    theme_void()

# ui ---–
ui <- fluidPage(
    fluidRow(
        column(width = 6, 
               radioGroupButtons(
                   inputId = "tour", label = "Choose Tour 1 or Tour 2:", selected = "Tour 1",
                   choices = c("Tour 1", "Tour 2"))
        ),
        column(width = 6, 
               sliderInput(inputId = "gradient", label = "gradient max for the map", 
                           min = .15, max = 1, value = .6)
        )
    ),
    fluidRow(
        column(width = 7, girafeOutput(outputId = "summary", height = "500px")),
        column(width = 5, girafeOutput(outputId = "map", height = "500px"))
    )
)

# server ---–
server <- function(input, output, session) {
    
    output$summary <- renderGirafe({
        
        dat <- summ_detail[summ_detail$TOUR %in% tour(),]
        
        gg <- ggplot(dat, aes(x = x, y = score, fill = nuance, tooltip = tooltip, data_id = nuance)) + 
            geom_col_interactive() + coord_polar(theta = "y") + 
            scale_barplot_interactive +
            theme_void() + 
            theme(plot.caption = element_text_interactive(
                      tooltip = "https://parisdata.opendatasoft.com/explore",
                      data_id = "caption_id",
                      onclick = "window.open(\"https://parisdata.opendatasoft.com/explore\")",
                      hover_css = "fill:magenta;cursor:pointer;"
                  )) + 
            labs(title = "Scores par nuance", x = "", y = "",
                 subtitle = "Municipales Paris 2020",
                 caption = "source: parisdata") 

        girafe(ggobj = gg, width_svg = 6, height_svg = 6, options = list(
            opts_hover(reactive = TRUE, css = "stroke:black;cursor:pointer;"),
            opts_hover_inv(css = "opacity:.3;"),
            opts_hover_key(css = "cursor:pointer;stroke:black;"),
            opts_selection(type = "none"), 
            opts_selection_key(type = "single", css = "stroke:black;stroke-width:3px;"), 
            opts_tooltip(offy = -50),
            opts_toolbar(saveaspng = FALSE)
        ))
    })
    
    observe({
        if(isTruthy(input$summary_hovered)){
            session$sendCustomMessage(type = 'summary_key_set', message = input$summary_hovered)
        } 
    })
    
    tour <- reactive({
        return(which(c("Tour 1", "Tour 2") %in% input$tour))
    })
    nuance <- reactive({
        req(input$summary_key_selected)
        input$summary_key_selected
    })
    
    output$map <- renderGirafe({
        req(nuance())
        
        dat <- scores[scores$TOUR %in% tour() & scores$nuance %in% nuance(),]
        
        # add tooltips ----
        ft_tooltips <- flextables_tour_secteur[flextables_tour_secteur$TOUR %in% tour(),]
        formula_highlight <- as.formula(sprintf("~ nuance %%in%% '%s'", nuance()))
        ft_tooltips$ft <- lapply(ft_tooltips$ft, function(ft, f){
            ft <- color(ft, i = f, color = "yellow")
            as.character(flextable::htmltools_value(ft))
        }, f = formula_highlight)
        dat$tooltip <- ft_tooltips$ft[match( dat$secteur, ft_tooltips$secteur)]
        
        gmap <- CarteSimple +
            geom_sf_interactive(
                mapping = aes(data_id = secteur, tooltip = tooltip, fill = score_pct),
                data = dat, colour = "white") +
            scale_fill_gradientn(colors = rev(c("#000004FF", "#BB3754FF", "#FCFFA4FF")), 
                                 limits = c(0, input$gradient), na.value = "#000004FF") +
            labs(title = nuance(), x = "", y = "", subtitle = "Scores en %") 

        girafe(ggobj = gmap, width_svg = 6, height_svg = 6, options = list(
            opts_hover(css = "opacity:.3;stroke:wheat;cursor:pointer;"),
            opts_tooltip(offx = 20, offy = -50),
            opts_selection(type = "none")
        ))
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
