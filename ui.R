library(shiny)
library(shinyWidgets)
library(shinyjs)
library(xtable)
library(shinyMatrix)
library(plotly)

shinyUI(
  fluidPage(
    theme = "bootstrap.css",
    title = "Ken Web Portfolio",
    tags$head(tags$script(src="leon_new.js"),
              tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/gsap/2.1.3/TweenMax.min.js"),
              tags$style(type='text/css', 
                         ".nav-tabs {font-size: 25px} "),
              tags$link(rel = "stylesheet", type = "text/css", href = "test.css"),
              tags$div(HTML(
                "<style>
                  canvas {
                    padding: 0;
                    margin: auto;
                    display: block;
                  }
                  div.
                </style>"
              )),
              tags$script(HTML("
                  let leon, canvas, ctx;
                  const sw = 1000;
                  const sh = 200;
                  const pixelRatio = 2;
                  function init() {
                    canvas = document.createElement('canvas');
                    document.body.insertBefore(canvas,document.body.childNodes[0]);
                    ctx = canvas.getContext('2d');
                    canvas.width = sw * pixelRatio;
                    canvas.height = sh * pixelRatio;
                    canvas.style.width = sw + 'px';
                    canvas.style.height = sh + 'px';
                    ctx.scale(pixelRatio, pixelRatio);
                    leon = new LeonSans({
                        text: 'Comp-Neuro: Ken Wang',
                        color: ['#000000'],
                        size: 80,
                        weight: 270,
                        align: 'center'
                    });
                    requestAnimationFrame(animate);
                    let i, total = leon.drawing.length;
                        for (i = 0; i < total; i++) {
                        TweenMax.fromTo(leon.drawing[i], 4, {
                          value: 0
                        }, {
                          delay: i * 0.05,
                          value: 1,
                          ease: Power4.easeOut
                        });
                     }
                  }

                  function animate(t) {
                      requestAnimationFrame(animate);
                      ctx.clearRect(0, 0, sw, sh);
                      const x = (sw - leon.rect.w) / 2;
                      const y = (sh - leon.rect.h) / 2;
                      leon.position(x, y);
                      leon.draw(ctx);
                  }
                  window.onload = () => {
                      init();
                  };
              "
              ))
    ),
    tabsetPanel(
      tabPanel("About",
         br(),
         fluidRow(
           align="center",
           column(
             width = 8,
             offset = 2,
             tags$h2("Welcome",
                     align = "left",
                     style = "font-weight: bold; font-family: candara;")
           )
         ),
         fluidRow(
           align = "center",
           column(
             width = 8,
             offset = 2,
             tags$h4("Hi everyone! Welcome to the CIS Web Portfolio dedicated for my 
           individual major! My name is Ken Wang,
           St. Olaf Class of 2021. I'm a triple major in Computational Neuroscience,
           Biology, and Mathematics. I also have concentrations in Mathematical 
           Biology as well as Statistics and Data Science.",
                     align = "justify",
                     style = "font-family: candara;")
           )
         ),
         fluidRow(
           align="center",
           column(
             width = 8,
             offset = 2,
             tags$h4("Computational Neuroscience, or Comp-Neuro for short, 
                     is an interdisciplinary discipline that studies the nervous system
                     using computational approaches, including mathematics, statistics, 
                     computer simulations, and abstraction. It 
                     is a rather new field that emerged a few decades ago and is 
                     coherent by itself. One of the ultimate goals of computational
                     neuroscience is to be able to explain the everyday experience
                     of conscious life.",
                     align = "justify",
                     style = "font-family: candara;")
           )
         ),
         br(),
         tags$div(id = "button-div",
              actionButton(inputId = "button2", label = " Wiki Link about Comp-Neuro", class = "btn-wiki",
                           icon = icon("expand"),
              onclick = "window.open('https://en.wikipedia.org/wiki/Computational_neuroscience','_blank')",
              style = "font-family: candara; font-size:120%")
         ),
         br(),
         fluidRow(
           align="center",
           column(
             width = 8,
             offset = 2,
             tags$h2("The Portfolio",
                     align = "left",
                     style = "font-weight: bold; font-family: candara;")
           )
         ),
         fluidRow(
           align="center",
           column(
             width = 8,
             offset = 2,
             tags$h4("If you want to know more about the major, check out my major 
             proposal under 'The Major' tab. The proposal contains a detailed description
             for the field of Comp-Neuro, the guiding questions, coursework, my rationale
             for the major, a senior project proposal and consultation information with
             the librarian.",
                     align = "justify",
                     style = "font-family: candara;")
           )
         ),
         fluidRow(
           align="center",
           column(
             width = 8,
             offset = 2,
             tags$h4("The 'Simulation' tab contains a sample of my senior project.
             Feel free to try it out!
             A detailed description of the project explaning the simulations
             can be found in the 'Senior
             Integrative Projects' section in my major proposal.",
                     align = "justify",
                     style = "font-family: candara;")
           )
         ),
         fluidRow(
           align="center",
           column(
             width = 8,
             offset = 2,
             tags$h4("The 'Presentation' tab has slides from my senior presentation.",
                     align = "justify",
                     style = "font-family: candara;")
           )
         ),
         fluidRow(
           align="center",
           column(
             width = 8,
             offset = 2,
             tags$h4("If you have any questions, feel free to email me at 
                     wang35@stolaf.edu.",
                     align = "justify",
                     style = "font-family: candara;")
           )
         ),
         br(),
         fluidRow(
           align="center",
           column(
             width = 8,
             offset = 2,
             tags$h2("Acknowledgements",
                     align = "left",
                     style = "font-weight: bold; font-family: candara;")
           )
         ),
         fluidRow(
           align="center",
           column(
             width = 8,
             offset = 2,
             tags$h4("I want to thank the Biology and MSCS faculty members, especially
                     professors from the neuroscience department, for helping me throughout
                     my 4 years at St. Olaf.
                     I want to specifically thank Professor Kevin Crisp for being the
                     major advisor. Thank you for introducing me to the field of computational
                     neuroscience, helping me design the major and complete 
                     all the required courses as well as guiding me through the senior project.
                     ",
                     align = "justify",
                     style = "font-family: candara;")
           )
         ),
         fluidRow(
           align = "center",
           column(
             width = 8,
             offset = 2,
             tags$h4(
               "I also want to thank Susan Carlson, Karil Kucera, and their colleagues
               at CIS for helping me navigate the application process and
               create a successful major.",
               align = "justify",
               style = "font-family: candara;"
             )
           )
         ),
         br(),
         br(),
         fluidRow()
      ),
      tabPanel("The Major",
         uiOutput("proposal")
      ),
      tabPanel("Simulation",
               fluidRow(
                 column(
                   1,
                   align = "center",
                   br(),
                   br(),
                   dropdown( # drop down button
                     tags$h2("Parameters",
                             align = "center"), # title
                     br(),
                     sliderInput(inputId = "number",
                                 label = "# of Neurons",
                                 min = 3, max = 40,
                                 value = 25),
                     br(),
                     pickerInput( # column selection for patient treatment studies
                       inputId = "cell_type", # input id
                       label = "Type of Cells",  # label
                       choices = c("Excitatory","Inhibitory","Both"), # choices
                       selected = "Both",
                       options = list(
                         size = 10, # size of the selection box
                         `selected-text-format` = "count > 2" # how to show selected
                       ),
                       multiple = FALSE # allow multiple selections
                     ),
                     br(),
                     sliderInput(inputId = "duration",
                                 label = "# of Timestep",
                                 min = 50, max = 500,
                                 value = 50),
                     br(),
                     sliderInput(inputId = "thresh",
                                 label = "Cell threshold",
                                 min = -5, max = 5,
                                 value = 0),
                     br(),
                     radioButtons(inputId = "results_toggle",
                                  label = "Results Display",
                                  choices = c("Matrix","Plot","Both"),
                                  selected = "Matrix",
                                  inline = TRUE),
                     br(),
                     radioButtons(inputId = "starting_cond",
                                  label = "Starting Condition",
                                  choices = c("random","input"),
                                  selected = "random",
                                  inline = TRUE),
                     br(),
                     conditionalPanel(
                       condition = "input.starting_cond == 'input'",
                       uiOutput("input_condition"),
                       br(),
                     ),
                     fluidRow(
                       actionButton(
                         inputId = "plot",
                         label = "Run Simulation",
                         style = "font-size: 140%"
                       ),
                       align = "center"
                     ),
                     style = "unite",
                     circle = TRUE, status = "danger",
                     icon = icon("gear"), width = "200px",
                     tooltip = tooltipOptions(title = "Set up a simulation!") # style for drop down button
                   )
                 ),
                 column(10,
                        offset = 2,
                    conditionalPanel(
                      condition = "input.results_toggle != 'Plot'",
                      fluidRow(
                        tags$h2("Network Matrix",
                                align = "center"), # title
                      ),
                      br(),
                      uiOutput("results"),
                      br(),
                    ),
                    conditionalPanel(
                      condition = "input.results_toggle != 'Matrix'",
                      fluidRow(
                        tags$h2("Simulation Results",
                                align = "center"), # title
                      ),
                      br(),
                      plotlyOutput("plot_results")
                    ),
                    br(),
                    br()
                 )
               )
      ),
      tabPanel("Presentation",
        uiOutput("presentation")
      )
    )
  )
)
