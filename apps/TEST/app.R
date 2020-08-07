ui <- navbarPage("Navbar!",
                 tabPanel("Plot", sidebarLayout(sidebarPanel(
                   radioButtons("yaxis1", "y-axis", c("speed"="speed", "dist"="dist"),
                                selected = "speed"
                   )),
                   mainPanel( plotOutput("plot"),
                              textOutput("test2")))),  # for input checking
                 
                 tabPanel("Summary", sidebarLayout(sidebarPanel(
                   radioButtons("yaxis2", "grouping-var", c("speed"="speed", "dist"="dist")
                   )),
                   mainPanel(
                     verbatimTextOutput("summary"),
                     textOutput("test1")
                   )))
)

server <- function(input, output, session) {
  
}
shinyApp(ui, server)