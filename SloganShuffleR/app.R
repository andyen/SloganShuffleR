library(shiny)
`%>%` <- magrittr::`%>%`

nouns <- readr::read_file("nouns.txt")
verbs <- readr::read_file("verbs.txt")

nvn_template <- '<span class="noun">{noun1}</span> <span class="black">{verb}</span> <span class="noun">{noun2}!</span>'

nnn_template <- '<span class="noun">{noun1},</span> <span class="noun black">{noun2},</span> <span class="noun">{noun3}!</span>'


# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(tags$style(HTML(readr::read_file("style.css")))),

    # Application title
    titlePanel("Worker Union Slogan Generator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton("generateSlogan", "Generate Slogan!"),
            hr(),
            radioButtons(
                "pattern",
                "Slogan Schema:",
                choices = list(
                    "[noun]  [verb]  [noun]!" = "nvn",
                    "[noun],  [noun],  [noun]!" = "nnn"
                )
            ),
            textAreaInput("nouns", "Noun Pool:", value=nouns),
            textAreaInput("verbs", "Verb Pool:", value=verbs)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            htmlOutput("slogan")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observeEvent(input$generateSlogan, {
        nouns <- input$nouns %>% 
            htmltools::htmlEscape() %>% 
            stringr::str_split("\n") %>% 
            unlist()
        verbs <- input$verbs %>% 
            htmltools::htmlEscape() %>% 
            stringr::str_split("\n") %>% 
            unlist()
        
        if (input$pattern == "nvn") {
            slogan <- glue::glue(
                nvn_template,
                noun1 = sample(nouns, 1),
                verb = sample(verbs, 1),
                noun2 = sample(nouns, 1)
            )
        } else if (input$pattern == "nnn") {
            slogan <- glue::glue(
                nnn_template,
                noun1 = sample(nouns, 1),
                noun2 = sample(nouns, 1),
                noun3 = sample(nouns, 1)
            )   
        } else {
            stop("Pattern not recognized!")
        }
        
        output$slogan <- renderText(HTML(slogan))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
