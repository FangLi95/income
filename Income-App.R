# Load packages
library(shiny)
library(tidyverse)
library(e1071)
library(caret)
library(randomForest)
dat <- read.csv("income_evaluation.csv", stringsAsFactors = T, na.strings = " ?")
dat <- na.omit(dat)

train_index <- sample(1:nrow(dat), nrow(dat)*0.7, replace = T)
train <- dat[train_index, ]
test <- dat[-train_index, ]


# Define UI
ui <- fluidPage(
  titlePanel("Income classification"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("ntree", "n tree: ", min = 1, max=500, value=100),
      sliderInput("mtry", "mtry", min=1, max=10, value=5),
      selectInput("node", "node size", choices = seq(5, 30, 5), selected = 5)
    ),
    # Output: Description, lineplot, and reference
    mainPanel(
      tabsetPanel(
        tabPanel("Algorithm", 
                 h2("Introduction"),
                 p("Data Set Information:

Extraction was done by Barry Becker from the 1994 Census database. A set of reasonably clean records was extracted using the following conditions: ((AAGE>16) && (AGI>100) && (AFNLWGT>1)&& (HRSWK>0))

Prediction task is to determine whether a person makes over 50K a year.

Attribute Information:

Listing of attributes:

50K, <=50K.age: continuous.
workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
fnlwgt: continuous.
education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
education-num: continuous.
marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
sex: Female, Male.
capital-gain: continuous.
capital-loss: continuous.
hours-per-week: continuous.
native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands."),
                 p("Random forest is actually a special bagging method that uses decision trees as a model in bagging. First, use the bootstrap method to generate m training sets. Then, for each training set, construct a decision tree. When the node finds the features to split, not all the features can be found to maximize the index (such as information gain) , But randomly extract a part of the features from the features, find the optimal solution among the extracted features, apply it to the node, and split. The random forest method has bagging, that is, the idea of integration, which is actually equivalent to sampling both samples and features (if the training data is regarded as a matrix, as it is common in practice, then it is a row and All columns are sampled), so overfitting can be avoided.

The method of the prediction stage is the strategy of bagging, voting by classification, and regression to the mean.One parameter is ntree.Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.Another one is mtry.Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)")
        ),
        tabPanel("Result", verbatimTextOutput("summary"),
                 plotOutput("plot"))
      )
    )
  )
)

# Define server function
server <- function(input, output, session) {
  
  model <- reactive({
    mod <- randomForest(income~., train, ntree=input$ntree, mtry=input$mtry, 
                        nodesize=as.integer(input$node))
    mod
  })
  
  output$summary <- renderPrint({
    mod <- model()
    preds <- predict(mod, test)
    print(confusionMatrix(preds, test$income))
  })
  
  output$plot <- renderPlot({
    mod <- model()
    varImpPlot(mod)
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)