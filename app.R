# Libraries required:
library(shiny)
#library(here)
library(ggplot2)
library(dplyr)
library(magrittr)
library(plotly)
library(tidyverse)
library(bslib)
library(stringr)
library(readr)
#install.packages("shinyWidgets")
library(shinyWidgets)

link <- "https://raw.githubusercontent.com/annskama/p_values/main/data/150211FullFile_AllStatcheckData_Automatic1Tail.csv"
my_data <- read.csv(link, sep = ";")

#my_data <- read.csv(here("data/150211FullFile_AllStatcheckData_Automatic1Tail.csv"),
#                          sep = ";", dec=".", header = TRUE, row.names=NULL)

# Remove not used in this project variables
my_data <- my_data[, -c(2:7,11:18)]
# Rename column names and change variables' class
colnames(my_data) <- c("n", "comparison", "p_report", "p_compute", "year")
my_data$p_report <- as.numeric(gsub(",",".", my_data$p_report))
my_data$p_compute <- as.numeric(gsub(",",".", my_data$p_compute))
my_data$year <- parse_number(my_data$year)

# Clean the dataset from missing values in variables used in analysis
my_data <- my_data[!is.na(my_data$p_report) & !is.na(my_data$p_compute) &
                     !is.na(my_data$comparison) &!is.na(my_data$year),]

# Define UI for application (several pages)
ui <- navbarPage("P-value in Psychology",   # title

            # set theme (choose theme at: https://bootswatch.com/3/ )
            theme = bs_theme(version = 4, bootswatch = "journal",
                             bg="#37474f",fg="#e8eaf6",
                             primary = "#ea80fc"),

            # first page, the text is downloaded from Rmd file
            tabPanel("About",
                     fluidPage(column(10,
                                      offset = 2,
                                      includeMarkdown("Introduction.Rmd")))
            ),

            # second page
            tabPanel("P-value Distribution",

            fluidPage(

              # density plot
              plotOutput("distPlot"),

              # input widgets for the plot, arranged in columns
              fluidRow(
                # some design for sliders
                chooseSliderSkin(color = "#ea80fc"),

                column(6,
                       offset = 1,
                       # input for x axis scale slider
                       sliderInput("scale",
                                  "Please select the p-values range (x axis scale)",
                                  min = 0,
                                  max = 1,
                                  value = c(0,0.1),
                                  width = "150%"),
                       # input for the year slider
                       sliderInput("years",
                                   "Please select the year",
                                  min = 1985,
                                  max = 2013,
                                  value = 2013,
                                  # code for animation
                                  animate = animationOptions(interval = 1000, loop = FALSE,
                                                             playButton = icon('play', "fa-2x"),
                                                             pauseButton = icon('pause', "fa-2x")),
                                  width = "150%")),

                column(3,
                       offset = 1,
                       # input for selection which p-values to report (all or only exact values)
                       selectInput("p_filter",
                                   label = "Show p-values reported",
                                   choices = c("all", "only as exact values"),
                                   selected = "all",
                                   multiple = FALSE) # not allow more than one selection
        ))
)),
          # third page
          tabPanel("Accuracy in Reporting",

                   fluidPage(
                     fluidRow( height=10,


                       # output
                       column(6,
                              offset = 1,
                              plotOutput("accurracyDist")),
                              #plotOutput("errorDist")),

                       # input widget (selection of threshold with three options)
                       column(4,
                              selectInput("threshold",
                                          label = "Please select the threshold",
                                          choices = c(0.05, 0.01, 0.001),
                                          selected = 0.05,
                                          multiple = FALSE),
                              # text output with the accuracy ratio
                              textOutput("accuracy")))

                   )

        ))

# Server logic
server <- function(input, output) {

  # p-value distribution (density) plot
  output$distPlot <- renderPlot({
        # generate dataset for the specified year
        temp_df <- my_data[my_data$year==input$years,]

        # check for all values to report or only the exact ones
        if (input$p_filter == "only as exact values") {
          temp_df <- temp_df[temp_df$comparison =="=",]}
        else {}

        # draw the density plot for a given year and p-value subset (all or exact values)
        # add two plots on the same graph - for reported and computed p-values
        p <- ggplot(data=temp_df)
        p + geom_density(aes(x=p_compute,
                               fill="Computed"), na.rm=TRUE, alpha=0.5)+
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
          # the dataset drawn (that is not drawn data points are not deleted)
          coord_cartesian(xlim = c(input$scale[1],input$scale[2]),
                          ylim = c(0,60), expand = TRUE) +
          # titles for the axises
          xlab("p-values")+ylab("Density")+
          # title for the graph
          ggtitle(paste("Comparison of reported and computed p-values, year", input$years))+
          # some designing
          theme(plot.title = element_text(hjust = 0.5, size = 15, face="bold"),
                axis.title = element_text(size=13)) +
          scale_fill_manual(values = c("#ea80fc", "#eceff1"), name="Type of p-value")

    })


  # report accuracy graph
  output$accurracyDist <- renderPlot({
      # generate subdataset with only "below smth" p-values reported
      my_temp1 <- my_data[my_data$comparison=="<",]
      # and specify this "smth" based on the input
      my_temp1 <- my_temp1[my_temp1$p_report == as.numeric(input$threshold),]
      # calculate the share of correctly reported values
      correct_share <- nrow(my_temp1[my_temp1$p_compute <as.numeric(input$threshold),])/nrow(my_temp1)*100

      # rearrange the dataset from a wide format to a long one
      vars <- c("n", "p_report", "p_compute", "year")
      my_temp1 <- select(my_temp1, all_of(vars))
      my_temp1 <- pivot_longer(data =my_temp1, cols=c("p_report", "p_compute"),
                              names_to='p_type',
                              values_to='values')

      # draw the plot
      q <- ggplot(data = my_temp1,
                  mapping = aes(x=p_type, y=values, group = n))
      q + geom_point(na.rm = TRUE, size=0.1) +
        geom_line(linewidth=0.5, aes(color= year))+
        # add horizontal line to show the selected threshold
        geom_hline(yintercept = as.numeric(input$threshold), linetype="dotted",
                   color = "#7b1fa2", linewidth=0.7)+
        # axis and graph titles
        xlab("") + ylab("p-values") +
        ggtitle(paste("Calculated P-values for p<", input$threshold))+
        # some designing
        theme(plot.title = element_text(hjust = 0.5, size = 15, face="bold"),
              axis.title = element_text(size=13))+
        # replace the names of variables to a more user friendly wording
        scale_x_discrete(labels=c("p_compute" = "Calculated",
                                  "p_report" = "Reported threshold"))+
        ylim(0,1)

      }, height = 600, width = 500)

  # text with accuracy ratio
  output$accuracy <- renderText({

    # filter the dataset again
    my_temp1 <- my_data[my_data$comparison=="<",]
    my_temp1 <- my_temp1[my_temp1$p_report == as.numeric(input$threshold),]
    # and calculate the share of correctly reported values
    paste0("P-values below ",input$threshold, " were reported correctly in ",
             round(nrow(my_temp1[my_temp1$p_compute <as.numeric(input$threshold),])/nrow(my_temp1)*100,0),
                                         "% cases")

      })

}
# Adjust the theme of graphs in line with the theme of the apps
thematic::thematic_shiny()
# Run the application
shinyApp(ui = ui, server = server)
