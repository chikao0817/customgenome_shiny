##------------ Biotools ddRAD ---------------##
################# UI Mainpage #################

### fresh create theme

mytheme <- create_theme(
  bs4dash_vars(
    navbar_light_color = "#BEC5CB",
    navbar_light_active_color = "#FFF",
    navbar_light_hover_color = "#FFF"
  ),
  bs4dash_yiq(
    contrasted_threshold = 10,
    text_dark = "#FFF", 
    text_light = "#272c30"
  ),
  bs4dash_layout(
    sidebar_width = "320px"
  ),
  bs4dash_sidebar_light(
    bg = "#C3D1CF", 
    color = "#5C5D5F",
    hover_color = "#FFF",
    submenu_bg = "#DEDEDE", 
    submenu_color = "#FFF", 
    submenu_hover_color = "#FFF"
  ),
  bs4dash_status(
    primary = "#829EB0", 
    danger = "#BF616A",
    warning = "#BCB471",
    success = "#7AA291",
    light = "#96A2A2"
  ),
  bs4dash_color(
    navy = "#447277",
    lightblue = "#8A9DBA",
    purple = "#7C609B",
    fuchsia = "#98987A",
    maroon = "#877584",
    teal = "#60847E",
    lime = "#A5DB9D"
    
  )
)


###


dashboardPage( 
  ## preloader use waiter package
  preloader = list(html = tagList(spin_terminal(),
                                  h4("loading...")
                                  ), color = "#405452"),
  
  fullscreen = TRUE,
  scrollToTop = TRUE,
  ### Header  
  header = dashboardHeader(
    ## Navbar
    status = "success",
    title = dashboardBrand(
    title =  shiny::tags$span("igvShiny_TestApp", 
                  style = "font-weight: 400; color: white; font-size: 26px"),
    color = "teal",
    opacity = 0.8)
  ),
  ### 
  
  ###### Siderbar  ######
  sidebar	= dashboardSidebar(
    skin = "light", 
  ## User panel
    sidebarUserPanel(
      name = a("igvShiny_TestApp",style = "font-size: 130%;")
    ),
  
  ## Siderbar Menu 
  bs4SidebarMenu(
    id = "sidebar_menu",
    bs4SidebarMenuItem(
      "_1. Genome Select",
      tabName = "basic_genome_info",
      icon = icon("question-circle")
    ),
    bs4SidebarMenuItem(
      "_2. Genome Viewer",
      tabName = "genome_viewer",
      icon = icon("chart-bar")
    ),
    bs4SidebarMenuItem(
      "_  Version Log",
      tabName = "version_log",
      icon = icon("book")
    ))
  ),
  
  ###### Body ###### 
  body = dashboardBody(
    ##------ fresh theme --------##
    use_theme(mytheme),

    ##------ Body setting -------##
    # dark datatable setting
    uiOutput('datatable_darkcontrol'),
    # idel control
    shiny::tags$script(inactivity),
    # head tag
    shiny::tags$head(shiny::tags$link(rel = "icon", type = "image/png", href = "favicon.png")),
    # echarts4r theme
    # e_theme_register(echarts_dark_theme$options, name = "dark_theme"),
    useShinyalert(),
    # shinyJS
    useShinyjs(),
    shinyjs::extendShinyjs(text = "shinyjs.clickpin = function(){$('#controlbarPin').click()}",functions = c("clickpin")),
    # include shinyFeedback
    useShinyFeedback(), 
    # Add cutstom css to wider Modal
    includeCSS(path = "www/custom.css"),
    # Add cutstom javascript to Busy disable button
    includeScript('www/shinycustom.js'),
    # Cicerone
    use_cicerone(),
    # Waiter
    useWaiter(),
    
    ## sliderbar 
    chooseSliderSkin(skin = "Shiny",color = "#669999"),
    #setSliderColor(c("#669999","#669999"),c(1,2)),
    
    ##------ Body content -------##
    tabItems(

      ##--- Page 1 Basic Info & Genome Select ---####
      tabItem(
        tabName = "basic_genome_info",
        ### Info Table and Select Control
        fluidRow(
          box(
            title = "Genome Info Table",
            status  = "teal",
            solidHeader = TRUE,
            width = 9, height = 600, elevation = 2,
            DTOutput('basic_genome_info_DT')
          ),
          box(
            title = "Select Genome Control Panel",
            background = "teal",
            width = 3, height = 600, elevation = 2,
            uiOutput('basic_genome_info_genome_select_ui'),
            uiOutput('basic_genome_info_genome_ref_select_ui'),
            hr(),
            shiny::actionButton('basic_genome_info_select_check',label ="Check Genome", icon("play-circle"),width = "100%",
                                style="color: #fff; background-color: #BD9D9D; border-color: #BD9D9D;", class = "bttn-simple" )
          )
        ),
        ### Genome Info karyoplote
        fluidRow(
          box(
            id = "basic_genome_info_karyoplote",
            title = "Genome Information plot",
            status  = "teal",
            solidHeader = TRUE,
            width = 12, height = 800, elevation = 2,
            plotOutput('basic_genome_info_karyoplote_plot',height = 750,width = 900),
            ##
            sidebar = boxSidebar(
              id = "basic_genome_info_karyoplote_sidebar",
              background = "#5D655E",
              startOpen = TRUE,easyClose = FALSE,
              width = 30,
              column(width = 10,
                     h5("karyoplote Parameter:"),
                     uiOutput('basic_genome_info_karyoplote_chr_ui'),
                     radioGroupButtons(
                       'basic_genome_info_karyoplote_addtrack',
                       label = "Add Plot Track",
                       selected = "None",
                       status = "primary",
                       size = "normal",
                       direction = "vertical",
                       justified = TRUE,
                       individual = FALSE,
                       checkIcon = list(
                         yes = icon("check-square"),
                         no = icon("square")
                       ),
                       width = 250,
                       choices = c("None","Add ATCG Content"),
                       disabled = FALSE
                     ),
                     pickerInput(
                       inputId = 'basic_genome_info_karyoplote_binwidth',
                       label = "Heatmap Bin width",
                       selected = 1000000,
                       choices = c(100000,500000,1000000,5000000)),
                     ## ATCG color node
                     shiny::tags$label(class = "control-label","ATCG color palette select"),
                     fluidRow(
                       column(width = 3,
                              colourpicker::colourInput(
                                label = "A",
                                inputId = 'basic_genome_info_karyoplote_a_color',
                                value = "#E27E72"
                              )
                       ),
                       column(width = 3,
                              colourpicker::colourInput(
                                label = "T",
                                inputId = 'basic_genome_info_karyoplote_t_color',
                                value = "#72A3E2"
                              )
                       ),
                       column(width = 3,
                              colourpicker::colourInput(
                                label = "C",
                                inputId = 'basic_genome_info_karyoplote_c_color',
                                value = "#72E281"
                              )
                       ),
                       column(width = 3,
                              colourpicker::colourInput(
                                label = "G",
                                inputId = 'basic_genome_info_karyoplote_g_color',
                                value = "#EBDA70"
                              )
                       )
                     ),
                     hr(),
                     shiny::downloadButton(style="color: #fff; background-color: #4D72A9; border-color: #4D72A9;",
                                           class = "bttn-simple",outputId = 'basic_genome_info_karyoplote_plot_dl',label = "Download Plot")
              )

            )
          )
        )
        ),
        ####--- Page 2 Genome Viewer ---####
        tabItem(
            tabName = "genome_viewer",
          # Target Region Info 
          fluidRow(
            box(title = "Target Fragment Position Info",
                status  = "maroon",
                solidHeader = TRUE,
                width = 8, height = 600, elevation = 2,
                DTOutput('simulation_check_igvinfo_DT',height = 520)
            ),
            box(title = "Target Fragment IGV Track Control",
                background  = "maroon",
                solidHeader = TRUE,
                width = 4, height = 600, elevation = 2,
                h5("You can upload simuation bam or small test library bam to check match in silico result.
                   (Advice bam file small than 500 Mb.) and needs upload index bai file."),
                fileInput('simulation_check_bam_file',"Input Simulate BAM and BAI File select:", 
                          multiple = TRUE, accept = c(".bam",".bai"),
                          width = NULL, buttonLabel = "Browse...",
                          placeholder = "No file selected"),
                hr(),
                shiny::actionButton('simulation_check_add_gene_track',label ="Add Gene Tracks Info", icon("water"),width = 250,
                                    style="color: #fff; background-color: #C68841; border-color: #C68841;", class = "bttn-simple" )
                
              )  
          ),
          fluidRow(
            box(title = "Target Fragment Position Info",
                status  = "maroon",
                solidHeader = TRUE,
                width = 12, height = 700, elevation = 2,
                igvShinyOutput('igvShiny_0'))
          ),
          ## BAM Summary Info 
          fluidRow(
            ## Summary plot
            tabBox(
              title = "Input Simulation BAM Summary Plot",
              id = "simulation_check_bam_summary_plot_tabbox",
              status  = "maroon",
              side = "right",
              type = "pills",
              width = 7, height = 655, elevation = 2,
              selected = "MAPQ Distribution",solidHeader = TRUE,
              tabPanel(
                title = "MAPQ Distribution",
                ggplotZoomDownloadUI('bam_mapq_distribution_plot', height = 550)
              )
            )
          )
      ),
      
      #===================================#
      ####---   Page  Version Log   ---####
      tabItem(
        tabName = "version_log",
        fluidRow(
          bs4ValueBox(
            value = shiny::tags$p(0, style = "font-size: 180%;"),
            elevation = 4,width = 3,
            subtitle = "Total Usage Count",
            color = "primary",gradient = TRUE,
            icon = icon("user-friends")
          ),
          bs4ValueBox(
            value =  shiny::tags$p(paste0(0," mins"), style = "font-size: 180%;"),
            elevation = 4,width = 3,
            subtitle = "Total Usage Time",
            color = "olive",gradient = TRUE,
            icon = icon("user-clock")
          )
        ),
        fluidRow(
          box(
            id = "version_log_box",
            title = "Version log",elevation = 2,
            status = "warning",width = 6,height = 600,solidHeader = TRUE,
            p("Version Number    : First : Important change or Server upload ;"),
            p("Second : Module add or Big change ; Third : Bug fixed or Slightly change."),
            reactableOutput('version_log_reactable')
          )
        ),
        fluidRow(
          box(
            id = "vsession_package_info_box",
            title = "Package session info",elevation = 2,
            status = "primary",width = 6,height = 600,solidHeader = TRUE,
            p("Session Package version."),
            p("Group package by source and use."),
            reactableOutput('session_package_info')
          )
        )
       
      )  
      
      
    )
  ),
  ### Footer
  footer = bs4DashFooter(left = "created by Biotools BioInfo."),
)
