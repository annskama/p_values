# Libraries required
library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(plotly)
library(tidyverse)
library(bslib)
library(stringr)
library(readr)
library(shinyWidgets)
library(markdown)

<<<<<<< HEAD
# Define UI for application (several tabs)
=======
# Define UI for application (several pages)
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
ui <- navbarPage("P-value in Psychology",   # title

            # set the theme and adjust some colors
            theme = bs_theme(version = 4, bootswatch = "journal",
                             bg="#37474f",fg="#e8eaf6",
                             primary = "#ea80fc"),

<<<<<<< HEAD
            # first tab, the text is downloaded from Rmd_file@github
            tabPanel("Summary",
                     fluidPage(column(10,
                                      offset = 2,
                                      markdown(readLines("https://raw.githubusercontent.com/annskama/p_values/main/Summary.Rmd")))
=======
            # first page, the text is downloaded from Rmd file
            tabPanel("Summary",
                     fluidPage(column(10,
                                      offset = 2,
                                      uiOutput("summary"))
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
                     )
            ),


<<<<<<< HEAD
            # second tab
=======
            # second page
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
            tabPanel("P-value Distribution",

            fluidPage(

              # input widgets for the plot, arranged in columns
              fluidRow(
                # some design for sliders
                chooseSliderSkin(color = "#ea80fc"),

                column(7,
                       offset = 1,

<<<<<<< HEAD
                       # output with the density plot
=======
                       # density plot
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
                       plotOutput("distPlot"),

                       # input for x axis scale slider
                       sliderInput("scale",
                                  "Please select the p-values range (x axis scale)",
                                  min = 0,
                                  max = 1,
                                  value = c(0,0.1),
                                  width = "90%"),
                       # input for the year slider
                       sliderInput("years",
                                   "Please select the year",
                                  min = 1985,
                                  max = 2013,
                                  value = 2013,
                                  # year format (to get rid of comma in Y,YYY)
                                  sep = "",
                                  # code for animation
                                  animate = animationOptions(interval = 1000, loop = FALSE,
                                                             playButton = icon('play', "fa-2x"),
                                                             pauseButton = icon('pause', "fa-2x")),
                                  width = "90%")),

                column(3,
                       offset = 1,
                       br(),
                       # input for selection which p-values to report (all or only exact values)
                       selectInput("p_filter",
                                   label = "Show p-values reported",
                                   choices = c("all", "only as exact values"),
                                   selected = "all",
                                   multiple = FALSE), # not allow more than one selection
                       br(),
<<<<<<< HEAD
                       # text with key take-home messages
                       h4("Key patterns:"),
                       h6("- No clear change tendency for all reported p-values year-over-year"),
                       h6("- Much smoother distribution for only exactly reported p-values"),
                       h6("- No spikes for calculated p-value distribution in later years")

=======
                       h4("Key patterns:"),
                       h6("- No clear change tendency year-over-year"),
                       h6("- Much smoother distribution for only exactly reported p-values"),
                       h6("- No spikes for calculated p-value distribution in later years")
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
        ))
)),
          # third tab
          tabPanel("Accuracy in Reporting",

                   fluidPage(
                     fluidRow(
<<<<<<< HEAD
                       # output with the plot
                       column(6,
=======
                       # output
                       column(4,
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
                              offset = 1,
                              plotOutput("accuracyDist")),

                       # input widgets
                       column(4,
                              offset=1,
<<<<<<< HEAD
                              # selection of p-value reporting type: exact value or below threshold
=======
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
                              selectInput("compare",
                                          label = "Please select p-values to show",
                                          choices = c("equal to threshold","below threshold"),
                                          selected = "equal to threshold",
                                          multiple = FALSE),

<<<<<<< HEAD
                              # selection of the threshold with three options
=======
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
                              selectInput("threshold",
                                          label = "Please select the threshold",
                                          choices = c(0.05, 0.01, 0.001),
                                          selected = 0.05,
                                          multiple = FALSE),
                              # text output with the accuracy ratio
                              textOutput("accuracy"),
                              br(),
<<<<<<< HEAD

                              # key take-home messages
=======
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
                              h4("Key patterns:"),
                              h6("- Inaccuracy ratio in 5-14% range"),
                              h6("- The inaccuracy in reporting is higher for 0.05 threshold")
                              ))

                   )

        ))

<<<<<<< HEAD
# server part
=======
# Server part
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
server <- function(input, output) {

  # get the data from github
  link <- "https://raw.githubusercontent.com/annskama/p_values/main/data/150211FullFile_AllStatcheckData_Automatic1Tail.csv"
  my_data <- read.csv(link, sep = ";")

<<<<<<< HEAD
  # remove not used in this project variables
  my_data <- my_data[, -c(2:7,11:18)]
  # rename column names and change variables' class
=======
  # Remove not used in this project variables
  my_data <- my_data[, -c(2:7,11:18)]
  # Rename column names and change variables' class
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
  colnames(my_data) <- c("n", "comparison", "p_report", "p_compute", "year")
  my_data$p_report <- as.numeric(gsub(",",".", my_data$p_report))
  my_data$p_compute <- as.numeric(gsub(",",".", my_data$p_compute))
  my_data$year <- parse_number(my_data$year)

<<<<<<< HEAD
  # clean the dataset from missing values in variables used in analysis
  my_data <- my_data[!is.na(my_data$p_report) & !is.na(my_data$p_compute) &
                       !is.na(my_data$comparison) &!is.na(my_data$year),]

=======
  # Clean the dataset from missing values in variables used in analysis
  my_data <- my_data[!is.na(my_data$p_report) & !is.na(my_data$p_compute) &
                       !is.na(my_data$comparison) &!is.na(my_data$year),]

  # get the text for Summary tab
  output$summary <- renderUI({
      markdown(readLines("https://raw.githubusercontent.com/annskama/p_values/main/Introduction.Rmd"))
        })

>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
  # p-value distribution (density) plot
  output$distPlot <- renderPlot({

    p <- my_data %>%
      # filter the dataset for the year and p-values (all or exact values based on p_filter)
      filter(year == input$years) %>%
      filter(if (input$p_filter == "only as exact values") comparison == "=" else TRUE) %>%
      ggplot() +
          # add two plots on the same graph - for reported and computed p-values
          geom_density(aes(x=p_compute,
                               fill="Calculated"), na.rm=TRUE, alpha=0.5)+
          geom_density(aes(x=p_report, fill="Reported"),
                         na.rm=TRUE, alpha=0.5) +
          # add three vertical lines for thresholds 0.05, 0.01 and 0.001
          geom_vline(xintercept = 0.05, linetype="dotted",
                     color = "white", linewidth=0.7) +
          geom_vline(xintercept = 0.01, linetype="dotted",
                     color = "white", linewidth=0.7) +
          geom_vline(xintercept = 0.001, linetype="dotted",
                     color = "white", linewidth=0.7) +
          # add the scale of x axis based on the relevant input
          # set the limits via the coordinate system to adjust the axis without changing
          # the dataset drawn (that is not shown data points are not deleted)
          coord_cartesian(xlim = c(input$scale[1],input$scale[2]),
                          ylim = c(0,60), expand = TRUE) +
<<<<<<< HEAD
          # titles for the axes
=======
          # titles for the axis
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
          xlab("P-values")+ylab("Density")+
          # title for the graph
          ggtitle(paste("Comparison of reported and calculated p-values, year", input$years))+
          # some designing
          theme(plot.title = element_text(hjust = 0.5, size = 15, face="bold"),
                axis.title = element_text(size=13)) +
          scale_fill_manual(values = c("#ea80fc", "#eceff1"), name="Type of p-value")
    p

    })


  # report accuracy graph
<<<<<<< HEAD
  output$accuracyDist <- renderPlot({

    q <- my_data %>%
      # filter the dataset to get only equal to or below smth p-values reported
=======

  output$accuracyDist <- renderPlot({

    q <- my_data %>%
      # filter the dataset to get only "below smth" p-values reported
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
      filter(if (input$compare == "equal to threshold") comparison == "=" else comparison == "<") %>%
      # and specify this "smth" based on the input
      filter(p_report == as.numeric(input$threshold)) %>%
      # take the variables we need for the graph
      select(n, p_report, p_compute, year) %>%
      # rearrange the dataset from a wide format to a long one
      pivot_longer(cols = c("p_report", "p_compute"),
                   names_to = "p_type",
                   values_to = "values") %>%
      # make a graph
      ggplot(aes(x = p_type, y = values, group = n)) +
      geom_point(na.rm = TRUE, size = 0.1) +
        geom_line(linewidth=0.5, aes(color= year))+
        # add horizontal line to show the selected threshold
        geom_hline(yintercept = as.numeric(input$threshold), linetype="dotted",
                   color = "#7b1fa2", linewidth=0.7)+
<<<<<<< HEAD
        # axes and graph titles
=======
        # axis and graph titles
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
        xlab("") + ylab("P-values") +
        ggtitle(paste("Calculated P-values for p",
                      (if (input$compare == "equal to threshold") "=" else "<"),
                input$threshold))+
        # some designing
        theme(plot.title = element_text(hjust = 0.5, size = 15, face="bold"),
              axis.title = element_text(size=13))+
        # replace the names of variables to a more user friendly wording
        scale_x_discrete(labels=c("p_compute" = "Calculated",
                                  "p_report" = if (input$compare == "equal to threshold") "Reported"
                                  else "Reported Threshold"))+
        ylim(0,1)
    q

      })

  # text with accuracy ratio
  output$accuracy <- renderText({

    my_data %>%
<<<<<<< HEAD
      # filter the dataset
      filter(if (input$compare == "equal to threshold") comparison == "=" else comparison == "<") %>%
      filter(p_report == as.numeric(input$threshold)) %>%
      # different wording depending on "equal"/"below" selection
      {if (input$compare == "equal to threshold")
        paste0("For p-values reported as equal to ", input$threshold,
               ",\n", "in ",
               # p_compute should be rounded to either 2 or 3 decimal points
               # depending on the threshold (0.05/0.01 or 0.001)
=======
      filter(if (input$compare == "equal to threshold") comparison == "=" else comparison == "<") %>%
      filter(p_report == as.numeric(input$threshold)) %>%
      {if (input$compare == "equal to threshold")
        paste0("For p-values reported as equal to ", input$threshold,
               ",\n", "in ",
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
               round(nrow(.[round(.$p_compute,
                                  (if(input$threshold=="0.001") 3 else 2))  <= as.numeric(input$threshold),]) / nrow(.)*100,0),
               "% cases the relevant calculated p-value (after rounding to the ",
               if(input$threshold=="0.001") "3rd " else "2nd ", "decimal point) does not exceed ",
               input$threshold, ", and in ",
               round(nrow(.[round(.$p_compute,
                                  (if(input$threshold=="0.001") 3 else 2)) == as.numeric(input$threshold),]) / nrow(.)*100,0),
               "% cases it equals to ",
               input$threshold, ".")

        else
<<<<<<< HEAD
          # wording for "below threshold" option
=======
>>>>>>> 7026cae42297b203687973ea858632ff97da6a75
          paste0("For p-values reported below ", input$threshold,
                 ":\n", "in ",
                 round(nrow(.[.$p_compute < as.numeric(input$threshold),]) / nrow(.)*100,0),
                 "% cases the relevant calculated p-value falls below ", input$threshold, ".")
      }
  })


}
# adjust the theme of graphs in line with the theme of the apps
thematic::thematic_shiny()
# run the application
shinyApp(ui = ui, server = server)

