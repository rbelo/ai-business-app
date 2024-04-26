ui <- fluidPage(
  # Application title
  titlePanel(paste0("AI Impact on Business: Exploring Prediction and Judgment")),
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      radioGroupButtons(inputId = "typeofdata",
                        label = h3("Data"),
                        choices = c("Generate Data" = "generate.data",
                                    "Load from File" = "load.data"),
                        ),
   ## radioButtons("typeofdata", "Choose whether to generate data or load from a file:",
   ##             c("Generate data" = "generate.data",
   ##               "Load data from file" = "load.data"),
   ##             inline = TRUE),
     conditionalPanel(condition = "input.typeofdata == 'generate.data'",
     h4("Generate data using the parameters below:"),
      ## actionButton("generate.data.button", "Generate Data"),
     numericInput("n.obs",
                   label = "Observations",
                   value = 10000,
                   min = 100,
                   max = 50000),
     fluidRow(
      column(6,
      sliderInput("p.outcome.group.a",
                  label = "% of Positives",
                  value = c(20),
                  min = 0, max = 100)
      ),
      column(6,
      sliderInput("auc.group.a",
                  label = "Model Quality (AUC)",
                  value = c(0.7),
                  min = 0.5, max = 1, step = 0.01)
      )),
    downloadButton("downloadData", "Download Generated Data"),
     ), # conditional panel
     conditionalPanel(condition = "input.typeofdata == 'load.data'",
      h4("Upload a CSV file:"),
      p("The file must contain at least the following columns"),
      tags$ol(
        tags$li("score - model prediction (e.g., probability)"),
        tags$li("outcome - target variable"),
        tags$li("group - group to which the observation belongs (protected variable, e.g., gender)")),
      p("For Uplift Analysis, the file must further contain"),
      tags$ol(
        tags$li("score_uplift - uplift model prediction"),
        tags$li("treated - whether the unit was treated"),
        tags$li("complier_status")),
      fileInput('target_upload', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
            )),
     ), # conditional panel
      hr(),
     h3("Analysis"),
      tabsetPanel(id = "analysis.param.tabs",
        tabPanel(h5("Benefit/Cost"), id = "t1",
     fluidRow(),
     h4("Benefit/ Cost Analysis"),
     switchInput(inputId = "upliftanalysis",
                 label = "Uplift Analysis",
                 onLabel = "Yes",
                 offLabel = "No",
                 value = FALSE,
                 ## disabled = TRUE,
                 labelWidth = "100px"),
     p("Benefit of targeting an obervation of the following types when compared with not targeting."),
     conditionalPanel(condition = "input.upliftanalysis == ''",
     fluidRow(
       column(6,
         numericInput("tp.benefit",
                label = "True Positive",
                value = 1)),
       column(6,
        numericInput("fp.benefit",
                label = "False Positive",
                value = -1))),
     fluidRow(
       column(6,
         numericInput("fn.benefit",
                label = "False Negative",
                value = 0)),
       column(6,
        numericInput("tn.benefit",
                label = "True Negative",
                value = 0))),
     fluidRow(
       column(6,
         numericInput("fixed.cost",
                label = "Fixed Cost",
                value = 0))),

     ), # conditional panel
     conditionalPanel(condition = "input.upliftanalysis != ''",
     p("WARNING: Uplift Analyis is not fully implemented yet"),
     fluidRow(
       column(6,
         numericInput("susceptible.benefit",
                label = "Susceptible",
                value = 4)),
       column(6,
        numericInput("surething.benefit",
                label = "Sure Thing",
                value = -1))),
     fluidRow(
       column(6,
         numericInput("lostcause.benefit",
                label = "Lost Cause",
                value = -1)),
       column(6,
        numericInput("donotdisturb.benefit",
                label = "Do Not Disturb",
                value = -6))),
     conditionalPanel(condition = "input.typeofdata == 'generate.data'",
     fluidRow(
       column(6,
      sliderInput("p.treated",
                  label = "% Treated",
                  value = c(0),
                  min = 0, max = 100),
      sliderInput("corr.uplift.outcome",
                  label = "Corr. Uplift/Outcome",
                  value = c(0),
                  min = -1, max = 1, step = 0.1),
      ),
       column(6,
      sliderInput("ate",
                  label = "Avg. Treat. Effect (ATE)",
                  value = c(0),
                  min = -1, max = 1, step = 0.1),
      sliderInput("auc.uplift",
                  label = "AUC Uplift",
                  value = c(0.5),
                  min = 0.5, max = 1, step = 0.01),
      )),
     ), # conditional panel
     ), # conditional panel
        ),
        tabPanel(h5("Fairness"), id = "t2",
     h4("Fairness Analysis"),
     switchInput(inputId = "fairnessanalysis",
                 label = "Fairness Analysis",
                 onLabel = "Yes",
                 offLabel = "No",
                 labelWidth = "120px"),
     conditionalPanel(condition = "input.fairnessanalysis != ''",
     conditionalPanel(condition = "input.typeofdata == 'generate.data'",
     fluidRow(
      column(12,
      sliderInput("p.group.b",
                  label = "% Obs. Group B",
                  value = c(0),
                  min = 0, max = 95)
      )),
     fluidRow(
      column(6,
      sliderInput("p.outcome.group.b",
                  label = "% of Positives Group B",
                  value = c(20),
                  min = 0, max = 100)
      ),
      column(6,
      sliderInput("auc.group.b",
                  label = "AUC Group B",
                  value = c(0.6),
                  min = 0.5, max = 1, step = 0.01)
      ))
     ), # conditional panel
      checkboxGroupInput("profit.constraints", "Fairness Constraints:",
            choices = list("Unconstrained",
                           "Independence (PP)",
                           "Separation (TPR)",
                           "Separation (FPR)",
                           "Sufficiency (PPV)",
                           "Sufficiency (NPV)",
                           "Unaware"),
            selected = list(),
            inline = FALSE
      ),
     ), # conditional panel
        )),
           width = 4
    ),

    mainPanel(
      tabsetPanel(id = "tabs",
        tabPanel("Data", id = data, uiOutput("data.info")),
        tabPanel("ROC", id = "roc", {plotlyOutput("roc.plot", width="800px", height="500px")}),
        tabPanel("Cumulative Response", id = "cum.resp", {plotlyOutput("cum.resp.plot", width="800px", height="500px")}),
        tabPanel("Lift", id = "lift.curve", {plotlyOutput("lift.curve.plot", width="800px", height="500px")}),
        tabPanel("Profit", id = "profit", {uiOutput("profit.plot")}),
        ## tabPanel("Profit", id = "profit", {plotlyOutput("profit.plot", width="800px", height="500px")}),
        ## tabPanel("ProfitUplift", id = "profit.uplift", {plotlyOutput("profit.uplift.plot", width="800px", height="500px")}),
        ## conditionalPanel(condition = "input.fairnessanalysis != ''",
        tabPanel("Fairness Stats", id = "stats", htmlOutput("fairness.stats")),
        ## ), # conditional panel
        ## tabPanel("ROC Stats", id = "roc.stats", htmlOutput("roc.stats"))
          )
    )
  )
)

ui
