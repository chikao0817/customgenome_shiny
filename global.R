#rm(list = ls())


## loading library for analysis in background

## bioconductor
library("karyoploteR")
library("GenomicAlignments")
library("VariantAnnotation")

## github devtools
library("bs4Dash")
library("reactable")
library("igvShiny")


### UI server basic loading library ###

## cran

library("magrittr")
library("dplyr")
library("dtplyr")
library("tidyr")
library("stringr")
library("tibble")
library("cyphr")
library("qs")
library("data.table")
library("lubridate")

## shiny

library("shiny")
library("colourpicker")
library("waiter")
library("shinyWidgets")
library("shinycssloaders")
library("shinyjqui")
library("shinyalert")
library("shinylogs")
library("shinyjs")
library("shinybusy")
library("sortable")
library("shinyFeedback")
library("fresh")
library("cicerone")

## cran plot and table
library("RColorBrewer")
library("gplots")
library("ggplot2")
library("plotly")
library("ggrepel")
library("cowplot")
library("echarts4r")
library("showtext")
library("DT")
library("rhandsontable")
library("openxlsx")
library("wordcloud2")

## BSgenome

library("BSgenome.Osativa.ENSEMBLPLANTS.IRGSP1.0")
library("BSgenome.Slycopersicum.SolGenomicsNetwork.SL3.0")
library("BSgenome.Slycopersicum.SolGenomicsNetwork.SL4.0")

######## shiny logs ##########

## scientific notation
options(scipen=20)

version_log = read.delim("data/Version_log.txt",header = T,sep = "\t",stringsAsFactors = FALSE,check.names = FALSE)

genome_info = read.csv("data/Genome_Info.csv")
genome_length_info = read.csv("data/Genome_Chr_length_info.csv",check.names = F)

##############################
###### idle stop signal ######
timeoutSeconds = 1800 # 30 min

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
Shiny.setInputValue('timeOut', '%ss')
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)



####################################
######      load data     ##########

## sodium encrypt key


### get shinyproxy username and usergroup ###
# userName = Sys.getenv("SHINYPROXY_USERNAME")
# userGroup = Sys.getenv("SHINYPROXY_USERGROUPS")

userName = "igvShinyTest"
userGroup = "Test"


## ggrepel random seed fixed
options(ggrepel.max.overlaps = Inf)
set.seed(42)

options(java.parameters = "-Xmx4g")


## reactable darktheme

reactable_darktheme <- reactableTheme(
  style = list(".dark-mode &" = list(color = "#fff", background = "#333A40")),
  cellStyle = list(".dark-mode &" = list(borderColor = "rgba(255, 255, 255, 0.15)")),
  #stripedColor = "rgba(255, 255, 255, 0.04)",
  headerStyle = list(".dark-mode &" = list(borderColor = "rgba(255, 255, 255, 0.15)")),
  #rowStripedStyle = list(".dark-mode &" = list(backgroundColor = "#69726E")),
  paginationStyle = list(".dark-mode &" = list(borderColor = "#333A40")),
  searchInputStyle =  list(".dark-mode &" = list(background = "rgba(255, 255, 255, 0.04)")),
  rowHighlightStyle = list(".dark-mode &" = list(background = "rgba(255, 255, 255, 0.04)")),
  pageButtonHoverStyle = list(".dark-mode &" = list(background = "rgba(255, 255, 255, 0.08)")),
  pageButtonActiveStyle = list(".dark-mode &" = list(background = "rgba(255, 255, 255, 0.1)"))
)


## showtext install
### google font
font_add(family = 'Helvetica',
         regular = "data/font_family/Helvetica/Helvetica.ttf",
         bold = "data/font_family/Helvetica/Helvetica-Bold.ttf" )

font_family_chioce = font_families()
names(font_family_chioce) = font_families()

names(font_family_chioce)[which(font_family_chioce=="Helvetica")] = "Helvetica (Default)"

#####################################
####### loading function  ###########

### bs4PopOverUI edit trigger hover ###
bs4PopoverUIH = function (tag, content, title, placement = c("top", "bottom",
                                                             "left", "right"))
{
  placement <- match.arg(placement)
  tag <- shiny::tagAppendAttributes(tag, `data-container` = "body",
                                    `data-toggle` = "popover", `data-placement` = placement,
                                    `data-trigger` = "hover",
                                    `data-content` = content, title = title)
  tagId <- tag$attribs$id
  shiny::tagList(shiny::singleton(shiny::tags$head(shiny::tags$script("$(function () {\n           // enable all popovers\n           $('[data-toggle=\"popover\"]').popover();\n          });\n          "))),
                 tag)
}



#######################################
########### Shiny Module ##############


#### ggplot zoom download and edit theme text CallModule
#### add div if too long plot

ggplotZoomDownloadUI <- function(id, height = "400px",width = "100%",divheight = NULL,divwidth = NULL,spinnercolor = "#3C54EC") {
  ns <- NS(id)
  if(is.null(divheight)&is.null(divwidth)){
    tagList(
      plotOutput(ns('plot'),height = height,width = width) %>% withSpinner(color=spinnercolor, type = 8) ,
      bs4PopoverUIH(
        actionBttn(ns('plot_zoom'),icon = icon("search"),style = "simple",color = "primary"),
        title = "Zoom, Edit and Download",
        content = "Zoom, Edit text and Download Resizable Plot",
        placement = "right"
      ))
  }else if(!is.null(divheight)&!is.null(divwidth)){
    tagList(
      div(style = paste0('width:',divwidth,'px;height:',divheight,'px;overflow-y: scroll;overflow-x: scroll;'), withSpinner(plotOutput(ns('plot'),height = height,width = width), type = 8,color=spinnercolor)),
      bs4PopoverUIH(
        actionBttn(ns('plot_zoom'),icon = icon("search"),style = "simple",color = "primary"),
        title = "Zoom, Edit and Download",
        content = "Zoom, Edit text and Download Resizable Plot",
        placement = "right"
      ))
  }else if(!is.null(divheight)){
    tagList(
      div(style = paste0('height:',divheight,'px;overflow-y: scroll'),withSpinner(plotOutput(ns('plot'),height = height,width = width), type = 8 ,color=spinnercolor)),
      bs4PopoverUIH(
        actionBttn(ns('plot_zoom'),icon = icon("search"),style = "simple",color = "primary"),
        title = "Zoom, Edit and Download",
        content = "Zoom, Edit text and Download Resizable Plot",
        placement = "right"
      ))
  }else if(!is.null(divwidth)){
    tagList(
      div(style = paste0('width:',divwidth,'px;overflow-x: scroll;'),withSpinner(plotOutput(ns('plot'),height = height,width = width), type = 8 ,color=spinnercolor)),
      bs4PopoverUIH(
        actionBttn(ns('plot_zoom'),icon = icon("search"),style = "simple",color = "primary"),
        title = "Zoom, Edit and Download",
        content = "Zoom, Edit text and Download Resizable Plot",
        placement = "right"
      ))
  }
}


#### SelectInput conflict

ggplotZoomDownload <- function(input, output, session, plotFun,filename,action = dark_mode_action,
                               showheight = "800px",showwidth = "800px",fileres = 96,spinnercolor = "#3C54EC") {
  
  output$'plot' <- renderPlot({
    plotFun()
  })
  
  Modal <- function() {
    ns <- session$ns
    modalDialog(size = "l",easyClose = TRUE,{
      
      ## ggplot function edit theme
      output$'plot_md' <- renderPlot({
        plot = plotFun()  + 
          { if(input$'plot_md_plot_edit')
            theme(
              ## add familty
              text = element_text(family = input$'plot_md_theme_fontfamily'),
              # plot title 
              plot.title =  element_text(size = input$'plot_md_plot_title_size',
                                         hjust = input$'plot_md_plot_title_hjust'),
              # axis x
              axis.title.x = element_text(size = input$'plot_md_xaxis_title_size'),
              axis.text.x = element_text(size = input$'plot_md_xaxis_text_size',
                                         angle = input$'plot_md_xaxis_text_angle', 
                                         vjust = input$'plot_md_xaxis_text_vjust', 
                                         hjust = input$'plot_md_xaxis_text_hjust'),
              # axis y
              axis.title.y = element_text(size = input$'plot_md_yaxis_title_size'),
              axis.text.y = element_text(size = input$'plot_md_yaxis_text_size',
                                         angle = input$'plot_md_yaxis_text_angle', 
                                         vjust = input$'plot_md_yaxis_text_vjust', 
                                         hjust = input$'plot_md_yaxis_text_hjust'),
              # legend
              legend.title = element_text(size = input$'plot_md_legend_title_size'),
              legend.text = element_text(size = input$'plot_md_legend_text_size'),
              legend.position = input$'plot_md_legend_position'
            )
          } + 
          { if(input$'plot_md_xaxis_title_remove')
            theme(
              # axis x title remove
              axis.title.x = element_blank())
          } + 
          { if(input$'plot_md_yaxis_title_remove')
            theme(
              # axis y title remove
              axis.title.y = element_blank())
          } + 
          { if(input$'plot_md_xaxis_text_remove')
            theme(
              # axis x text remove
              axis.text.x = element_blank())
          } + 
          { if(input$'plot_md_yaxis_text_remove')
            theme(
              # axis y text remove
              axis.text.y = element_blank())
          } +
          ## grey theme
          { if(input$'plot_md_plot_grey')
            scale_colour_grey(start = input$'plot_md_plot_grey_startcolor',end = input$'plot_md_plot_grey_endcolor')
          } +
          { if(input$'plot_md_plot_grey')
            scale_fill_grey(start = input$'plot_md_plot_grey_startcolor',end = input$'plot_md_plot_grey_endcolor')
          } + 
          ## grey continuous
          { if(input$'plot_md_plot_grey_continuous')
            scale_colour_gradient(low = gray.colors(1,start = input$'plot_md_plot_grey_continuous_startcolor'), 
                                  high = gray.colors(1,start = input$'plot_md_plot_grey_continuous_endcolor'))
          } +
          { if(input$'plot_md_plot_grey_continuous')
            scale_fill_gradient(low = gray.colors(1,start = input$'plot_md_plot_grey_continuous_startcolor'), 
                                high = gray.colors(1,start = input$'plot_md_plot_grey_continuous_endcolor'))
          }
        
        ### showtext
        if(input$'plot_md_plot_edit'){
          showtext_begin()
          print(plot)
          showtext_end()
        }else{
          print(plot)
        }
      })
      
      output$'plot_download' = downloadHandler(
        filename = filename, 
        content = function(file){
          # device <- function(..., width, height) {
          #   grDevices::png(..., width = input$'plot_md_size'$width/72, height = input$'plot_md_size'$height/72,
          #                  res = fileres, units = "in")
          # }
          plot = {
            ## download plot function setting
            plotFun() + 
              {if(input$'plot_md_plot_edit')
                theme(
                  ## add familty
                  text = element_text(family = input$'plot_md_theme_fontfamily'),
                  # plot title
                  plot.title =  element_text(size = input$'plot_md_plot_title_size',
                                             hjust = input$'plot_md_plot_title_hjust'),
                  # axis x
                  axis.title.x = element_text(size = input$'plot_md_xaxis_title_size'),
                  axis.text.x = element_text(size = input$'plot_md_xaxis_text_size',
                                             angle = input$'plot_md_xaxis_text_angle', 
                                             vjust = input$'plot_md_xaxis_text_vjust', 
                                             hjust = input$'plot_md_xaxis_text_hjust'),
                  # axis y
                  axis.title.y = element_text(size = input$'plot_md_yaxis_title_size'),
                  axis.text.y = element_text(size = input$'plot_md_yaxis_text_size',
                                             angle = input$'plot_md_yaxis_text_angle', 
                                             vjust = input$'plot_md_yaxis_text_vjust', 
                                             hjust = input$'plot_md_yaxis_text_hjust'),
                  # legend
                  legend.title = element_text(size = input$'plot_md_legend_title_size'),
                  legend.text = element_text(size = input$'plot_md_legend_text_size'),
                  legend.position = input$'plot_md_legend_position')
              } + 
              { if(input$'plot_md_xaxis_title_remove')
                theme(
                  # axis x title remove
                  axis.title.x = element_blank())
              } + 
              { if(input$'plot_md_yaxis_title_remove')
                theme(
                  # axis y title remove
                  axis.title.y = element_blank())
              } + 
              { if(input$'plot_md_xaxis_text_remove')
                theme(
                  # axis x text remove
                  axis.text.x = element_blank())
              } + 
              { if(input$'plot_md_yaxis_text_remove')
                theme(
                  # axis y text remove
                  axis.text.y = element_blank())
              } +
              ## grey theme
              { if(input$'plot_md_plot_grey')
                scale_colour_grey(start = input$'plot_md_plot_grey_startcolor',end = input$'plot_md_plot_grey_endcolor')
              } +
              { if(input$'plot_md_plot_grey')
                scale_fill_grey(start = input$'plot_md_plot_grey_startcolor',end = input$'plot_md_plot_grey_endcolor')
              } + 
              ## grey continuous
              { if(input$'plot_md_plot_grey_continuous')
                scale_colour_gradient(low = gray.colors(1,start = input$'plot_md_plot_grey_continuous_startcolor'), 
                                      high = gray.colors(1,start = input$'plot_md_plot_grey_continuous_endcolor'))
              } +
              { if(input$'plot_md_plot_grey_continuous')
                scale_fill_gradient(low = gray.colors(1,start = input$'plot_md_plot_grey_continuous_startcolor'), 
                                    high = gray.colors(1,start = input$'plot_md_plot_grey_continuous_endcolor'))
              }
          }
          
          png(file, width = input$'plot_md_size'$width/72, height = input$'plot_md_size'$height/72,
              res = fileres, units = "in")
          if(input$'plot_md_plot_edit'){
            showtext_begin()
            print(plot)
            showtext_end()
          }else{
            print(plot)
          }
          dev.off()
        })
      
      output$'jquiresize' = renderPrint({
        cat(sprintf('Plot height: %s, width: %s\n',
                    input$'plot_md_size'$height,
                    input$'plot_md_size'$width
        ))
      })
      
      
      ### modal content ###
      # use global input$'dark_mode'
      columnstyle = ifelse({action()},"background-color:#3e617c;","background-color:#c3d7e6;")
      
      fluidPage(
        fluidRow(
          column(width = 9,
                 verbatimTextOutput(ns('jquiresize')),
                 jqui_resizable(plotOutput(ns('plot_md'),height = showheight,width = showwidth)) %>% withSpinner(color=spinnercolor, type = 8)
          ),
          column(width = 3,style = "background-color:#c3d7e6;",
                 h4("Plot Text editor"),
                 br(),
                 switchInput(ns("plot_md_plot_edit"),label = "Activate theme edit",value = FALSE,
                             onLabel = "ON",offLabel = "Off",onStatus = "primary",offStatus = "danger",handleWidth = 200),
                 pickerInput(ns('plot_md_theme_fontfamily'), label="Font familty",
                             choices = font_family_chioce, selected = "Helvetica"),
                 numericInput(ns('plot_md_plot_title_size'), label="plot title size",
                              min = 5, max = 80,value = 25, step = 1),
                 numericInput(ns('plot_md_plot_title_hjust'), label="plot title text hjust",
                              min = 0, max = 5,value = 0, step = 0.1),
                 hr(),
                 h6("Click dropdown Button"),
                 h6("edit x y axis text, legend or grey scheme"),
                 # X axis
                 dropdownButton(
                   ##content
                   fluidRow(
                     column(width = 6,
                            prettyCheckbox(
                              inputId = ns('plot_md_xaxis_title_remove'),
                              label = "remove x title", value = FALSE,
                              icon = icon("check"),
                              status = "primary"
                            )
                     ),
                     column(width = 6,
                            prettyCheckbox(
                              inputId = ns('plot_md_xaxis_text_remove'),
                              label = "remove x text", value = FALSE,
                              icon = icon("check"),
                              status = "primary"
                            )
                     )
                   ),
                   numericInput(ns('plot_md_xaxis_title_size'), label="x title size",
                                min = 5, max = 80,value = 20, step = 1),
                   numericInput(ns('plot_md_xaxis_text_size'), label="x text size",
                                min = 5, max = 50,value = 15, step = 1),
                   numericInput(ns('plot_md_xaxis_text_angle'), label="x text angle",
                                min = 0, max = 360,value = 0, step = 1),
                   numericInput(ns('plot_md_xaxis_text_vjust'), label="x text vjust",
                                min = 0, max = 5,value = 0, step = 0.1),
                   numericInput(ns('plot_md_xaxis_text_hjust'), label="x text hjust",
                                min = 0, max = 5,value = 0, step = 0.1),
                   label = "x-axis  text edit",
                   icon = icon("cog"),
                   status = "primary",
                   circle = FALSE
                 ),
                 br(),
                 # Y axis
                 dropdownButton(
                   ##content
                   fluidRow(
                     column(width = 6,
                            prettyCheckbox(
                              inputId = ns('plot_md_yaxis_title_remove'),
                              label = "remove y title", value = FALSE,
                              icon = icon("check"),
                              status = "primary"
                            )
                     )
                     , column(width = 6,
                              prettyCheckbox(
                                inputId = ns('plot_md_yaxis_text_remove'),
                                label = "remove y text", value = FALSE,
                                icon = icon("check"),
                                status = "primary"
                              )
                     )),
                   numericInput(ns('plot_md_yaxis_title_size'), label="y title size",
                                min = 5, max = 80,value = 20, step = 1),

                   numericInput(ns('plot_md_yaxis_text_size'), label="y text size",
                                min = 5, max = 50,value = 15, step = 1),
                   numericInput(ns('plot_md_yaxis_text_angle'), label="y text angle",
                                min = 0, max = 360,value = 0, step = 1),
                   numericInput(ns('plot_md_yaxis_text_vjust'), label="y text vjust",
                                min = 0, max = 5,value = 0, step = 0.1),
                   numericInput(ns('plot_md_yaxis_text_hjust'), label="y text hjust",
                                min = 0, max = 5,value = 0, step = 0.1),
                   label = "y-axis  text edit",
                   icon = icon("cog"),
                   status = "danger",
                   circle = FALSE
                 ),
                 hr(),
                 dropdownButton(
                   ##content
                   numericInput(ns('plot_md_legend_title_size'), label="legend title size",
                                min = 5, max = 80,value = 22, step = 1),
                   numericInput(ns('plot_md_legend_text_size'), label="legend text size",
                                min = 5, max = 50,value = 15, step = 1),
                   pickerInput(ns('plot_md_legend_position'), label="legend position",
                               choices = c("right","left","bottom","top","none"),selected = "right"),
                   label = "legend text edit",
                   icon = icon("tools"),
                   status = "warning",
                   circle = FALSE
                 ),
                 hr(),
                 dropdownButton(
                   ##content
                   materialSwitch(
                     inputId = ns("plot_md_plot_grey"),
                     label = "Discrete value Grey ",
                     value = FALSE,
                     status = "success"
                   ),
                   fluidRow(
                     column(width = 6,
                            numericInput(ns('plot_md_plot_grey_startcolor'), label="start grey color",
                                         min = 0, max = 1,value = 0.2, step = 0.1)),
                     column(width = 6,
                            numericInput(ns('plot_md_plot_grey_endcolor'), label="end grey color",
                                         min = 0, max = 1,value = 0.8, step = 0.1))
                   ),
                   materialSwitch(
                     inputId = ns("plot_md_plot_grey_continuous"),
                     label = "Continuous value Grey ",
                     value = FALSE,
                     status = "success"
                   ),
                   fluidRow(
                     column(width = 6,
                            numericInput(ns('plot_md_plot_grey_continuous_startcolor'), label="start grey color",
                                         min = 0, max = 1,value = 0.2, step = 0.1)),
                     column(width = 6,
                            numericInput(ns('plot_md_plot_grey_continuous_endcolor'), label="end grey color",
                                         min = 0, max = 1,value = 0.8, step = 0.1))
                   ),
                   label = "Convert Grey scheme",
                   icon = icon("palette"),
                   status = "success",
                   circle = FALSE
                 )
          )
        )
      )
    },
    footer = tagList(
      downloadBttn(ns('plot_download'),
                   label = "Download Plot (Resize result)",
                   style = "bordered",
                   color = "primary",size = "sm"),
      HTML('<button class="action-button bttn bttn-bordered bttn-sm bttn-primary bttn-no-outline" id="sample_corplot_md_cancel" type="button" data-dismiss="modal">Cancel</button>')
    )
    )}
  
  observeEvent(input$'plot_zoom',
               showModal(Modal())
  )
}


