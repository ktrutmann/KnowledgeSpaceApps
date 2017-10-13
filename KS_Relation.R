# App to illustrate and practice the relationship between surmise relations
# and the resulting knowledge structure
#
# Created by Kevin Trutmann (k.trutmann@gmail.com)
# with code by Martin Losert and Prof. Juergen Heller


#### Header ####

library(shiny)
library(shinydashboard)
library(igraph)
library(pks)
library(relations)


#### Functions ####

# Create the relationship
createRel <- function(a) {

  # Transitive Closure
  for (i in 1:nrow(a)){
    for (j in 1:ncol(a)){
      if (a[i, j] == 1) a[i, ] <- 1 - (1 - a[j, ]) * (1 - a[i, ]) # 1 if any of them contains a 1
    }
  }
  return(a)
}


# Generate the Predecessor relations
prePlot <- function(a, title = "Precedence Relation", showMat = TRUE){

  n <- nrow(a)
  aa <- as.matrix(a)

  # Check wether any items are identical
  identical <- a * t(a)

  # If yes, melt the matrix
  tbd <- c() # rows and collumns to be deleted
  for(i in 1:n){
    for(j in 1:i){ # Only scan the lower triangular matrix
      if(identical[i,j] == 1){
        aa[ , i] <- 1 - (1 - aa[ , j]) * (1 - aa[ , i])
        aa[ , j] <- aa[ , i]
        aa[i, ]  <- 1 - (1 - aa[j, ]) * (1 - aa[i, ])
        aa[j, ]  <- aa[i, ]
        tbd <- c(tbd, j)
      }
    }
  }
  if(length(tbd) > 0)
    a <- aa[-tbd, -tbd] # Delete the melted rows and collumns, if there's anything to delete

  # Do all this only if there are still more than 1 items left in a:
  if(class(a) == "matrix"){
    diag(a) <- 0
    n2 <- nrow(a)

    transitive <- ifelse(any(diag(createRel(a)) == 1), FALSE, TRUE) #If any item is a "predecessor to itself", transitivity is violated

    d <- b <- createRel(a)

    # Transitive Reduction
    for(i in 1:n2){
      for(j in c(1:n2)[-i]){
        if(b[j,i]==1) d[j,] <- d[j,]*(1-b[i,])
      }
    }

    ed <- NULL # ed will be the edges of the graph
    for(i in 1:n2){
      for(j in 1:n2){
        if(d[i,j]==1)
          ed <- c(ed,i,j)
      }
    }

    # Create the corect labels for the items:
    labels <- letters[1:n]
    keep <- rep(TRUE, n)
    for(i in 1:n){
      for(j in 1:i){
        if(identical[i, j]){
          labels[i] <- paste0(labels[j], labels[i])
          keep[j] <- FALSE
        }
      }
    }

    #Only use each letter once per label
    labels <- sapply(labels, function(x) paste0(unique(strsplit(x, split = "")[[1]]), collapse = ""))

    g1 <- graph(edges=ed, n=n2, directed=T )
    V(g1)$label <- labels[keep]
    coord <- -layout_with_sugiyama(g1)$layout[, c(2,1)] #Switched around so it displays horizontaly

  } else { # What to do if only one item remains
    g1 <- graph(edges=NULL, n=1)
    V(g1)$label <- "abcde"
    coord <- t(as.matrix(-layout_with_sugiyama(g1)$layout[c(2,1)], nrow = 2))
  }

  E(g1)$color <- 'black'
  V(g1)$color <- ifelse(transitive, 'orange', 'red')

  # The actual Plotting:
  # Split the plot if the matrix is to be displayed
  if(showMat &  class(a) == "matrix")
    par(mar = c(3, 0.5, 3, 0.5), mfrow = c(1, 2))

  # Plot the hasse diagram
  plot(g1, layout = coord, vertex.frame.color = 'white', vertex.size = 30, main = title)
  # Write a warning if transitivity is violated:
  if(!transitive)
    mtext("Transitivity violated!", side = 1, line = 1, col = "red")

  # Plot the matrix:
  if(showMat &  class(a) == "matrix"){
    plot(which(t(createRel(a)[nrow(a):1,] + diag(nrow(a))[nrow(a):1,]) == 1, arr.ind = TRUE), axes = FALSE,
         xlim = c(1,nrow(a)), ylim = c(1, nrow(a)), pch = 4, col = 'orange', cex = 2)
    axis(3, at = 1:nrow(a), labels = labels[keep], tick = FALSE, cex.axis = 1.2)
    axis(2, at = nrow(a):1, labels = labels[keep], tick = FALSE, cex.axis = 1.2)
  }
}


# Generate the plot of the knowledge structure
structPlot <- function(a, title = "Quasi-Ordinal Knowledge Space") {
  a <-createRel(a)

  K.0 <- expand.grid(rep(list(0:1), 5))   # initially create all response patterns
  retain <- rep(TRUE, nrow(K.0))

  for (i in 1:nrow(K.0)) {
    for (j in 1:5) {
      if (K.0[i, j] == 1) { # If, for example, item two was solved
        if (any(a[, j] > K.0[i, ])) { # Check whether all the precedessor items of that item were also solved in that state
          retain[i] <- FALSE # If not, discard it
          break
        }
      }
    }
  }
  K <- K.0[retain, ]

  a <- t(K)
  n <- ncol(a)
  b = diag(0,n)

  for(i in 1:n){
    for(j in 1:n){
      if(sum(a[ , i] * a[ , j]) == sum(a[ , i]))
        b[i, j] = 1 # if the sum of the i-th times j-th collumn is equal to the sum of the i-th collumn, write a 1
                    # This is to detect precedence relations, as only items contained in both will produce a 1 and
                    # if that intersection is equal to the first state, it must come before the second.
    }
  }

  diag(b)<-0 # Each Item is also a "precessor to itself", but screw that.
  d <- b
  for(i in 1:n){
    for(j in c(1:n)[-i]){
      if(b[j,i]==1) d[j,]=d[j,]*(1-b[i,])  # Only keep a 1 if the state is the "closest" predecessor
    }
  }
  ed <- NULL # ed will be the edges of the graph
  for(i in 1:n) for(j in 1:n) if(d[i,j]==1) ed <- c(ed,i,j)

  g1 <- graph( edges=ed, n=n, directed=T )
  l <- list("0")
  for(i in 2:(n-1)) l[[i]] <- paste(c(c("a","b","c","d","e")[a[,i]*c(1:5)]),collapse = '') # Choose which lables to use for the vertecies
  l[[n]] <- c("Q")
  V(g1)$label <- l
  coord = layout_with_sugiyama(g1)$layout
  E(g1)$color <- 'black'
  V(g1)$color <- 'orange'
  par(mar = c(0,0,3,0))
  plot(g1,layout=-coord, vertex.frame.color="white", vertex.size = 25, edge.arrow.size = .8, main = title)

}


#### Quiz Solutions ####
Q1Solution <- matrix(c(0,1,0,0,0,
                       0,0,0,0,0,
                       0,1,0,1,0,
                       0,0,0,0,0,
                       1,1,1,1,0), nrow = 5)

Q2Solution <- matrix(c(0,0,0,0,0,
                       1,0,0,0,0,
                       1,1,0,0,1,
                       1,1,0,0,1,
                       1,0,0,0,0), nrow = 5)

Q3Solution <- matrix(c(0,0,1,0,0,
                       0,0,1,0,0,
                       0,0,0,0,0,
                       1,0,0,0,0,
                       1,0,0,0,0), nrow = 5)


#### UI ####
ui <- dashboardPage(
  dashboardHeader(title = "Precedence Relation and Corresponding Quasi-Ordinal Knowledge Space",
                  titleWidth = "100%"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    skin = "red",
    tabsetPanel(
      {tabPanel("Theory",
                fluidPage(
                  fluidRow(
                    column(1),
                    column(10,
                           withMathJax(),
                           h3("Precedence Relation"),
                           p("Imagine you are a student who just learned how to solve simple equations for x. Given that you have this knowledge,
                             it is reasonable to assume you are also able to understand the four basic operations, addition, subtraction, division and
                             multiplication. Also, if you are able to solve these problems in written form, you probably know how to write
                             (at least you know how to write numbers and the letter 'x'). Hence, we may surmise that a student who masters the item
                              'solving for x' will also master items involving 'basic arithmetic operations' only."),
                           p("In knowledge space theory a relation such as this one, in which one problem precedes another one,
                             is known as a 'precedence relation'. It is usually written as $$a \\preccurlyeq b$$
                             Precedence relations may be represented graphically, as in the following diagram:"),
                           fluidRow(column(12, align = "center", plotOutput("examplePlot", width = 500, height = 300))),
                           br(),
                           p("From this plot you can gather that items \\(a\\) and \\(b\\) are required to master \\(c\\). \\(e\\) in turn
                             cannot be mastered unless you learned \\(c\\) and \\(d\\) first."),
                           p("An important property of a precedence relation is transitivity. This means that, for example, if \\(a\\) precedes \\(b\\),
                             and \\(b\\) precedes \\(c\\), then \\(a\\) must also precede \\(c\\)."),
                           p("If precedence holds in both directions for any two items, then these items are in fact equivalent with respect to the
                             precedence relation, and they are called 'equally informative'. Whenever items \\(a\\) and \\(b\\) are equally informative,
                             then $$a \\preccurlyeq b, \\quad and \\quad b \\preccurlyeq a,$$ and we write $$a \\sim b.$$"),
                           p("Technically speaking, the precedence relations form 'quasi orders' on the domain \\(Q\\), which are the reflexive and
                             transitive binary relations on \\(Q\\)."),
                           h3("Quasi-Ordinal Knowledge Space"),
                           p("In celebrated result by Garrett Birkhoff (1937), it is shown that the quasi orders on \\(Q\\) are in one-to-one correspondence
                             to particular knowledge structures, which are closed under union and intersection, and are called
                             'quasi-ordinal knowledge spaces'. This means that each precedence relation defines a unique quasi-ordinal knowledge space,
                             and each quasi-ordinal knowledge space corresponds to a unique precedence relation."),
                           p("Givem a precedence relation \\(\\preccurlyeq\\) defined on the domain \\(Q\\), the corresponding quasi-ordinal knowledge space
                             \\(\\mathcal{K}\\) is defined by
                             $$K \\in \\mathcal{K} \\quad iff \\quad (p \\preccurlyeq q,~~ q \\in K ~~implies~~ p \\in K).$$"),
                           p("Notice that the more constraints a precedence relation puts on the order of the items, the fewer knowledge states will be consistent
                             with these constraints, and the smaller the corresponding quasi-ordinal knowledge space is."),
                           h3("Play and Test Tabs"),
                           p("The one-to-one correspondence between precedence relations and quasi-ordinal knowledge spaces on a domain
                             $$Q = \\{a, b, c, d, e\\}.$$
                             is illustrated in the next tab, where you can define your own precedence relation.
                             You may play around with the relation and observe the effect on the corresponding quasi-ordinal knowledge space,
                             which is illustrated on the right.
                             Notice that, whatever the relation looks like, the induced knowledge structure is closed unter union and intersection. This
                             means that with any two knowledge states their union as well as their intersection is a knowledge state, too."),
                           p("Finally, you may take the quiz in the third tab to test your knowledge, whenever you feel ready to do so."),
                           br(), br()
                           )))
                           )}, # End Theory Panel

      {tabPanel("Play",
                fluidPage(
                  br(),
                  p("Click the checkboxes to select the items that precede the item specified on top of the column.
                    The diagram below illustrates the precedence relation you define.
                    To its right you can see a matrix that not only shows the paiwise relationships you selected, but also those
                    implied by reflexivity (i.e. \\(p \\preccurlyeq p\\) for all \\(p \\in Q\\)), and transitivity. This is known as the
                    'reflexive transitive closure' of the relation you specified.
                    If the relation you select violates transitivity, a warning will be displayed.
                    Finally, the diagram to the right depicts the induced quasi-ordinal knowledge space.
                    Observe how the number of knowledge states decreases when you increase the number of constraints
                    in the precedence relation."),
                  br(),

                  fluidRow(
                    column(8,
                           column(2, checkboxGroupInput("preA", "a", list("b", "c", "d", "e"))),
                           column(2, checkboxGroupInput("preB", "b", list("a", "c", "d", "e"))),
                           column(2, checkboxGroupInput("preC", "c", list("a", "b", "d", "e"))),
                           column(2, checkboxGroupInput("preD", "d", list("a", "b", "c", "e"))),
                           column(1, checkboxGroupInput("preE", "e", list("a", "b", "c", "d"))),
                           column(2, br(), br(), br(), br(), br(), actionButton("resetPre", "Reset All")),

                           plotOutput("prePlot", width = "100%", height = 250)
                    ),
                    column(4,
                           plotOutput("structPlot", width = "100%")
                    )
                  )
                  )
                )}, # End Play Panel

      tabPanel("Test",
               uiOutput("quizUI")
      ) # End Test Panel

      ) # End tabsetPanel Container
                    ) # End dashboardBody
      ) # End UI


#### Server ####
server <- function(input, output, session) {

  # The matrix where the proposed relations are stored, as well as other values
  values <- reactiveValues(preChosen = matrix(rep(0, 25), nrow = 5),
                           quizAnsw1 = matrix(rep(0, 25), nrow = 5),
                           quizAnsw2 = matrix(rep(0, 25), nrow = 5),
                           quizAnsw3 = matrix(rep(0, 25), nrow = 5),
                           quizPage = "start",
                           quizCor = c(NA, NA, NA)
  )

  # Put input names into a vector to iterate over:
  preChecks <- paste0("input$pre", LETTERS[1:5])
  Q1Checks  <- paste0("input$Q1", LETTERS[1:5])
  Q2Checks  <- paste0("input$Q2", LETTERS[1:5])


  # If anything happens to any of the checkboxes
  observeEvent(c(eval(parse(text = c(preChecks, Q1Checks, Q2Checks)))), {
    for (i in 1:5) {
      for(j in 1:5) {
        # Get the values into matrix form:
        values$preChosen[i, j] <- ifelse(letters[i] %in% eval(parse(text = preChecks[j])), 1, 0)
        values$quizAnsw1[i, j] <- ifelse(letters[i] %in% eval(parse(text = Q1Checks[j])), 1, 0)
        values$quizAnsw2[i, j] <- ifelse(letters[i] %in% eval(parse(text = Q2Checks[j])), 1, 0)
      }
    }
  }, ignoreNULL = FALSE)


  # Reset buttons
  observeEvent(input$resetPre, {
    for (l in LETTERS[1:5])
      updateCheckboxGroupInput(session, paste0("pre", l), selected = character(0))
  })

  observeEvent(c(input$resetQ1, input$resetAll), {
    for (l in LETTERS[1:5])
      updateCheckboxGroupInput(session, paste0("Q1", l) , selected = character(0))
  })

  observeEvent(c(input$resetQ2, input$resetAll), {
    for (l in LETTERS[1:5])
      updateCheckboxGroupInput(session, paste0("Q2", l) , selected = character(0))
  })

  observeEvent(c(input$resetQ3, input$resetAll), {
    updateCheckboxGroupInput(session, "Q3A", selected = character(0))
    updateCheckboxGroupInput(session, "Q3B", selected = character(0))
  })


  #### Plots ####

  # The plot for the introduction example:
  output$examplePlot <- renderPlot(
    prePlot(matrix(c(0,0,0,0,0,
                     0,0,0,0,0,
                     1,1,0,0,0,
                     0,0,0,0,0,
                     1,1,1,1,0), nrow = 5), showMat = FALSE))

  # Surmise relations Plot in the play panel
  output$prePlot <- renderPlot(prePlot(values$preChosen))

  # Creates the structure-plot in the play panel:
  output$structPlot <- renderPlot(structPlot(values$preChosen))

  # Create the plot for quizz question 1:
  output$Q1Plot <- renderPlot(prePlot(Q1Solution, "Target Precedence Relation", showMat = FALSE))

  # Create the relations plot for quizz question 2:
  output$Q2PrePlot <- renderPlot(prePlot(values$quizAnsw2))

  # Create the structure plot for quizz question 1:
  output$Q2StrPlot <- renderPlot(structPlot(Q2Solution, "Target Knowledge Structure"))

  # Create the structure plot for quizz question 1:
  output$Q3PrePlot <- renderPlot(prePlot(Q3Solution, showMat = FALSE))


  #### Quiz layout: ####

  output$quizUI <- renderUI({

    if(values$quizPage == "start") {

      values$quizCor <- c(NA, NA, NA)

      return({
        fluidPage(
          br(),
          p("Here you can test your knowledge. Please press the start button to go on."),
          actionButton("startQuiz", "Start!")
          )
      })
    }

    if(values$quizPage == "q1") {

      if(is.na(values$quizCor[1])){
        Q1Nav <- actionButton("goToQ2", "Okay, Next Question")
      } else {
        Q1Nav <- actionButton("goToScore", "Check Results")
      }

      return({
        fluidPage(
          br(),
          p("Below you see a diagram depicting a precedence relations.
            Try to recreate this relation by clicking the appropriate checkboxes.
            You will receive feedback at the end of the quiz."),
          fluidRow(
            column(6,
                   column(2, checkboxGroupInput("Q1A", "a", list("b", "c", "d", "e"))),
                   column(2, checkboxGroupInput("Q1B", "b", list("a", "c", "d", "e"))),
                   column(2, checkboxGroupInput("Q1C", "c", list("a", "b", "d", "e"))),
                   column(2, checkboxGroupInput("Q1D", "d", list("a", "b", "c", "e"))),
                   column(1, checkboxGroupInput("Q1E", "e", list("a", "b", "c", "d"))),
                   column(2, br(), br(), br(), br(), br(), actionButton("resetQ1", "Reset All")),

                   plotOutput("Q1Plot", width = "75%", height = 250)
            )
          ),
          br(), br(), br(), br(), br(), br(), br(),
          Q1Nav # Navigation Button
          )
      })
    }

    if(values$quizPage == "q2") {

      if(is.na(values$quizCor[2])){
        Q2Nav <- actionButton("goToQ3", "Okay, Next Question")
      } else {
        Q2Nav <- actionButton("goToScore", "Check Results")
      }

      return({
        fluidPage(
          br(),
          p("To the right you can see a quasi-ordinal knowledge space corresponding to a specific precedence relation.
            Try to create the diagram showing the precedence relation that induces the given quasi-ordinal knowledge space."),
          fluidRow(
            column(6,
                   column(2, checkboxGroupInput("Q2A", "a", list("b", "c", "d", "e"))),
                   column(2, checkboxGroupInput("Q2B", "b", list("a", "c", "d", "e"))),
                   column(2, checkboxGroupInput("Q2C", "c", list("a", "b", "d", "e"))),
                   column(2, checkboxGroupInput("Q2D", "d", list("a", "b", "c", "e"))),
                   column(1, checkboxGroupInput("Q2E", "e", list("a", "b", "c", "d"))),
                   column(2, br(), br(), br(), br(), br(), actionButton("resetQ2", "Reset All")),

                   plotOutput("Q2PrePlot", width = "100%", height = 250)
            ),
            column(6,
                   plotOutput("Q2StrPlot", width = "100%")
            )
          ),
          Q2Nav # Navigation Button
          )
      })
    }

    if(values$quizPage == "q3") {
      return({
        fluidPage(
          withMathJax(),
          br(),
          p("Select the knowledge states \\(K\\) consistent with the prcedence relation \\(\\preccurlyeq\\) plotted in the diagram below,
            such that
            $$p \\preccurlyeq q,~~ q \\in K  ~~implies~~ p \\in K.$$."),
          fluidRow(
            column(6,
                   column(2, checkboxGroupInput("Q3A", "", list("{abce}", "{ab}", "{ebd}", "{acd}"))),
                   column(2, checkboxGroupInput("Q3B", "", list("{cbe}", "{bc}", "{abc}", "{cd}")))
            )
          ),

          plotOutput("Q3PrePlot", width = "30%", height = 250),

          actionButton("goToScore", "Okay, Show Results")
        )
      })
    }

    if(values$quizPage == "score") {

      # Check whether the corect solution was given:
      if(all(Q1Solution == createRel(values$quizAnsw1))){
        Q1cor <- span(style="color:green", p("Correct, congratulations!"))
        values$quizCor[1] <- TRUE
      } else {
        Q1cor <- list(span(style="color:red", p("Sorry, wrong...")),
                      actionButton("startQuiz", "Try Again"))
        values$quizCor[1] <- FALSE
      }

      # Check whether the corect solution was given:
      if(all(Q2Solution == createRel(values$quizAnsw2))){
        Q2cor <- span(style="color:green", p("Correct, congratulations!"))
        values$quizCor[2] <- TRUE
      } else {
        Q2cor <- list(span(style="color:red", p("Sorry, wrong...")),
                      actionButton("goToQ2", "Try Again"))
        values$quizCor[2] <- FALSE
      }

      # Check whether the corect solution was given:
      if(all(c("{abce}", "{acd}", "{bc}", "{abc}") %in% c(input$Q3A,input$Q3B)
             & !any(c("{ab}", "{ebd}", "{cbe}", "{cd}") %in% c(input$Q3A, input$Q3B)))){
        Q3cor <- span(style="color:green", p("Correct, congratulations!"))
        values$quizCor[3] <- TRUE
      } else {
        Q3cor <- list(span(style="color:red", p("Sorry, wrong...")),
                      actionButton("goToQ3", "Try Again"),
                      br())
        values$quizCor[3] <- FALSE
      }


      # Write a last feedback sentence, depending on whether everything was solved correctly:
      if(all(values$quizCor)) {
        quizFeedback <- p("Congratulations, you were able to solve all three problems!")
      } else {
        quizFeedback <- p("For the first two problems, it may be helpful to switch back to the 'play' tab.
                          You will be able to try out your solution, and then can return to the quiz where you left of.")
      }


      # Giving the actual feedback
      return({
        fluidPage(
          br(), p("Question 1:"),
          Q1cor,
          br(), p("Question 2:"),
          Q2cor,
          br(), p("Question 3:"),
          Q3cor,
          br(),
          quizFeedback,
          br(),

          actionButton("resetAll", "Restart the Quiz")
        )
      })
      }

    })


  #### Navigation: ####
  observeEvent(input$startQuiz, values$quizPage <- "q1")
  observeEvent(input$goToQ2, values$quizPage <- "q2")
  observeEvent(input$goToQ3, values$quizPage <- "q3")
  observeEvent(input$goToScore, values$quizPage <- "score")
  observeEvent(input$resetAll, values$quizPage <- "start")

  } # End Server

#### Run App ####
shinyApp(ui = ui, server = server)
