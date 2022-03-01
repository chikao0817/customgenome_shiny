library(shiny)
library(igvShiny)
library(bs4Dash)
library(DT)
library(rtracklayer)

# tracks loading
if(!dir.exists("tracks"))
  dir.create("tracks")
addResourcePath("tracks", "tracks")

# read gff3/gtf by rtracklayer 
geneAnnotation = rtracklayer::import("data/Serendipita_indica_dsm_11827_gca_000313545.ASM31354v1.50_subset.gff3")

# read fake snp table
snptable = read.csv("data/fake_snptable.csv")

server <- function(input, output, session){
  
  ## variant Info Table
  output$'variant_viewer_info_DT' <- renderDT({
    DT::datatable(
        snptable,
        selection = "single",
        rownames = FALSE,
        escape=FALSE,
        options = list(
          autowidth = FALSE,
          columnDefs = list(list(width = '50px')),
          pageLength = 10,
          lengthMenu = c(5,10, 15, 30),
          scrollX = TRUE,
          scrollY = 400
        )
      )
  })
  
  ## igvShiny 
  # igvShiny add Annotation by local gff3/gtf by Annotation ?  
  
  output$'variant_viewer_genome_igv' <- renderIgvShiny(
    igvShiny(options = list(
      genomeName="local",
      initialLocus = "",
      fasta="data/Serendipita_indica_dsm_11827_gca_000313545.ASM31354v1.dna.toplevel_subset.fa",
      index="data/Serendipita_indica_dsm_11827_gca_000313545.ASM31354v1.dna.toplevel_subset.fa.fai"
      # geneAnnotation = "data/Serendipita_indica_dsm_11827_gca_000313545.ASM31354v1.50_subset.gff3" 
    ))
  
  
    
  )
  
  ## DT selected to position
  observeEvent(input$'variant_viewer_info_DT_rows_selected',{
    
    posdf = snptable[input$'variant_viewer_info_DT_rows_selected',]
    igvShiny::showGenomicRegion(session, id="variant_viewer_genome_igv", 
                      paste0(posdf$Chr,":",posdf$start-200,"-",posdf$end+200))
    
  })
  
  
  ## how to load custom gene track (gff3/gtf) like loadVcfTrack ?
  # observeEvent(input$'igv_check_add_vcf_track', {
  #   
  #   loadVcfTrack(session, id="variant_viewer_genome_igv", trackName=vcftrackname, vcfData=VariantAnnotation_object)
  #
  #   ? loadAnnotationTrack(session, id="variant_viewer_genome_igv", trackName=gff3trackname, gtfData=geneAnnotation)
  #   
  # })
  
}

ui <- dashboardPage( 
  fullscreen = TRUE,
  scrollToTop = TRUE,
  ### Header  
  header = dashboardHeader(
    title = dashboardBrand(
      title =  tags$span("Test App", 
                         style = "font-weight: 400; color: white; font-size: 26px"),
      color = "navy",
      opacity = 0.8
    ),
    ## Navbar
    status = "primary"
  ),
  ### 
  
  ###### Siderbar  ######
  sidebar	= dashboardSidebar(
    skin = "light", 
    
    ## Siderbar Menu 
    bs4SidebarMenu(
      id = "sidebar_menu",
      bs4SidebarMenuItem(
        "_1. Variant Viewer",
        tabName = "variant_viewer",
        icon = icon("eye")
      ))
  ),
  
  ###### Body ###### 
  body = dashboardBody(
    ##------ Body content -------##
    tabItems(
      
      ####--- Page 1 Variant Viewer ---####
      tabItem(
        tabName = "variant_viewer",
        ### variant Info Table
        box(
          title = "Variant Info Table",
          status  = "navy",
          solidHeader = TRUE,
          width = 12, height = 700, elevation = 2,
          DTOutput('variant_viewer_info_DT')
        ),
        ### igvShiny 
        box(
          title = "IGV Viewer",
          status  = "navy",
          solidHeader = TRUE,
          width = 12, height = 1000, elevation = 2,
          igvShinyOutput('variant_viewer_genome_igv')
        ),
      )
    )
  ),
  ### Footer ###
  footer = bs4DashFooter(left = "Test App.")
)

runApp(shinyApp(ui = ui, server = server), port=9834)