

shinyServer(function(input, output, session) {
  
  #### large file upload accept 4 Gb ####
  options(shiny.maxRequestSize=4000*1024^2)
  
  ## Page loading
  Sys.sleep(1)
  

  ## shinylogs tracking
  track_usage(storage_mode = store_sqlite(path = "./logs/"))
  
  ## Time close session
  
  observeEvent(input$timeOut, { 
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    shinyalert(HTML('<a style="color:#6294eb"> Idle for too long ! <br> 
                                               Please sign out !!</a>'),
               HTML(paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time() + lubridate::hours(8) )),
               type = "warning",html = TRUE, showConfirmButton = FALSE)
    Sys.sleep(4)
    session$close()
  })
  
 
  ## datatable control panel dark theme
  
  output$'datatable_darkcontrol' = renderUI({
    if(input$'dark_mode'){
        tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
            color: #D8D5E1 !important;
        }"))
    }else{
        tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
            color: #000000 !important;
        }"))
    }
  })
  
  ## Module action with dark theme
  dark_mode_action = reactive(input$'dark_mode')
  
  
  #################################################
  ####--- Page 1 Basic Info & Genome Select ---####
  
  ## Common
  genome_common <- reactiveValues(BSgenome_obj = c(),
                                 genome_path = c(),
                                 chr_length_info = c())
  counter <- reactiveValues(
    genome_selected_ready = 0,
    chr_selected_ready = 0
  ) 
  
  makeReactiveBinding("genome_common")
  
  
  output$'basic_genome_info_DT' <- renderDT({

    genome_ref_link = genome_info[,c(1,7:8)] %>% tidyr::separate_rows(c(Reference.Genome.Version,Genome.Link.Info) ,sep = ";") %>%
                      mutate(Genome = factor(Genome,levels = genome_info$Genome),Links = paste0("<a target='_blank' href='",Genome.Link.Info,"'><i>",Reference.Genome.Version,"</i></a>")) %>%
                      group_by(Genome) %>% summarise(Reference.Genome.Link = paste(Links,collapse = ";"))


    genome_info_dt <- data.frame(
      Genome  = paste0("<a target='_blank' href='",genome_info$Wiki,"'><i>",genome_info$Genome,"</i></a>"),
      Plot = paste0('<img src="',genome_info$Short.Name,'.jpg" height="50"></img>'),
      `Genome Size` = genome_info$Genome.Size,
      `Chr Number` = genome_info$ChrNumber,
      `Assemble Level` = genome_info$Assemble.Level,
      `Genome Download Database` = paste0("<a target='_blank' href='",genome_info$Genome.Download.Database.Link,"'><i>",genome_info$Genome.Download.Database,"</i></a>"),
      `Reference Genome Version`= genome_ref_link$Reference.Genome.Link
      ,stringsAsFactors=FALSE)


    DT::datatable(
      genome_info_dt,
      escape = FALSE,
      selection = "single",
      extensions = 'Scroller',
      options = list(
        pageLength = 6,
        lengthMenu = c(6, 12, 24),
        scrollY = "430px"
      ),
      fillContainer = T
    )

  })

  ## Genome Select
  output$'basic_genome_info_genome_select_ui' <- renderUI({

    if(is.null(input$'basic_genome_info_DT_rows_selected')){
       genome_select = genome_info$Genome[1]
    }else{
       genome_select = genome_info$Genome[input$'basic_genome_info_DT_rows_selected']
    }
      pickerInput(
        inputId = 'basic_genome_info_genome_select',
        label = "Genome Select",
        selected = genome_select,
        choices = genome_info$Genome)
    
  })

  ## Ref Version Select
  output$'basic_genome_info_genome_ref_select_ui' <- renderUI({

    ref_select_choice = unlist(str_split(genome_info$Reference.Genome.Version[which(genome_info$Genome==input$'basic_genome_info_genome_select')],";"))
    
    if(length(ref_select_choice)>0){counter$genome_selected_ready = 1} 
    
    ## Reference Version Select
    pickerInput(
      inputId = 'basic_genome_info_genome_ref_select',
      label = "Reference Version Select",
      selected = NULL,
      choices = ref_select_choice)
     
  })

  ## Chr Select

  output$'basic_genome_info_karyoplote_chr_ui' <- renderUI({
    
    if(counter$genome_selected_ready==0){
      return()
    }else{

    chr_length_info = genome_length_info %>% dplyr::filter(Genome == as.character(input$'basic_genome_info_genome_select')&
                                                           `Reference Genome Version` == as.character(input$'basic_genome_info_genome_ref_select'))
    genome_common$chr_length_info <<- chr_length_info

    if(sum("autosome" %in% chr_length_info$type)>0){
      chr_select = chr_length_info %>% dplyr::filter(type == "autosome")
      chr_select = chr_select$Chr
    }else{
      chr_select = chr_length_info %>% arrange(-length)
      chr_select = chr_select$Chr[1:20]
    }
    
    counter$chr_selected_ready = 1

    pickerInput('basic_genome_info_karyoplote_chr_select',
                choices = chr_length_info$Chr,
                selected = chr_select,
                options = list(`live-search`=TRUE,
                               `actions-box` = TRUE),label = "Selected Chr", multiple = TRUE,width = "100%")
    
    
    }

  })


  

  ## Genome karyoplote

  genome_info_karyoplote_plot <- reactive({
    
    if(counter$chr_selected_ready==0){
      return()
    }else{

    genome_path = paste0("Genome_database/",gsub(" ","_",input$'basic_genome_info_genome_select'),
                         "/",input$'basic_genome_info_genome_ref_select')

    genome_common$genome_path <<- genome_path

    chr_length_info = genome_length_info %>% dplyr::filter(Genome == as.character(input$'basic_genome_info_genome_select')&
                                                           `Reference Genome Version` == as.character(input$'basic_genome_info_genome_ref_select'))

    genome_ref = genome_info[,c(1,7,10,11)] %>% tidyr::separate_rows(c(Reference.Genome.Version,BSgenome),sep = ";")
    
    BSgenome_obj = get(x = genome_ref$Short.Name[which(genome_ref$Reference.Genome.Version==input$'basic_genome_info_genome_ref_select')],
                       pos = paste0("package:", genome_ref$BSgenome[which(genome_ref$Reference.Genome.Version==input$'basic_genome_info_genome_ref_select')]))
    
    genome_common$BSgenome_obj <<- BSgenome_obj

    ## karyotype cytoband
    if("karyotype.txt"%in%list.files(path = genome_path)){
      cytobands = read.delim(paste0(genome_path,"/karyotype.txt"))
    }else{
      cytobands = NULL
    }

    if(input$'basic_genome_info_karyoplote_addtrack'=="Add ATCG Content"){
      
      waiter_show(id = 'basic_genome_info_karyoplote_plot',html = tagList(spin_timer(),
                                                                          h4("Loading Sequence and Caculating...",style = "color:black;")),
                  color = transparent(.5),hide_on_render = TRUE)



      bases <- c("C", "G", "A", "T")
      base.colors <- setNames(c(input$'basic_genome_info_karyoplote_c_color',
                                input$'basic_genome_info_karyoplote_g_color',
                                input$'basic_genome_info_karyoplote_a_color',
                                input$'basic_genome_info_karyoplote_t_color'), bases)

      chr_range = GRanges(data.frame(Chr = chr_length_info$Chr,start = 1L, end = chr_length_info$length))
      tiles = unlist(tile(x = chr_range, width = as.numeric(input$'basic_genome_info_karyoplote_binwidth')))

      ## subset by chromosome
      tiles = tiles[seqnames(tiles) %in% input$'basic_genome_info_karyoplote_chr_select']

      seqs <- getSeq(BSgenome_obj, tiles)

      counts <- alphabetFrequency(seqs, baseOnly=TRUE)
      freqs <- counts/rowSums(counts)

      mcols(tiles) <- DataFrame(freqs[,bases])


      cum.sums <- lapply(seq_len(length(tiles)),
                         function(i) {
                           return(cumsum(as.numeric(data.frame(mcols(tiles))[i,c(1:4)])))
                         })
      cum.sums <- do.call(rbind, cum.sums)
      cum.sums <- data.frame(cum.sums)
      names(cum.sums) <- bases

    }else{
      tiles = NULL
      cum.sums = NULL
      base.colors = NULL
    }

    genome_info_karyoplote_plot = list(cytobands = cytobands,
                                       tiles = tiles,
                                       cum.sums = cum.sums,
                                       base.colors = base.colors)

    return(genome_info_karyoplote_plot)
    
    
    }


  })


  output$'basic_genome_info_karyoplote_plot' <- renderPlot({

    if(is.null(genome_info_karyoplote_plot())){
      return()
    }else{
      
      waiter_show(id = 'basic_genome_info_karyoplote_plot',html = tagList(spin_timer(),
                                                                          h4("loading...",style = "color:black;")),
                  color = transparent(.5),hide_on_render = TRUE)


      chr_length_info = genome_common$chr_length_info
      cytobands = genome_info_karyoplote_plot()$cytobands
      tiles = genome_info_karyoplote_plot()$tiles
      cum.sums = genome_info_karyoplote_plot()$cum.sums
      base.colors = genome_info_karyoplote_plot()$base.colors

      chr_range = GRanges(data.frame(Chr = chr_length_info$Chr,start = 1L, end = chr_length_info$length))

    if(input$'basic_genome_info_karyoplote_addtrack'=="None"){

      kp <- plotKaryotype(genome = chr_range,chromosomes = input$'basic_genome_info_karyoplote_chr_select',cytobands = cytobands)

    }else if(input$'basic_genome_info_karyoplote_addtrack'=="Add ATCG Content"){

      kp <- plotKaryotype(genome = chr_range,chromosomes = input$'basic_genome_info_karyoplote_chr_select',cytobands = cytobands)
      kpPlotRibbon(kp, data=tiles, y0 = 0, y1=cum.sums$C, col=base.colors["C"])
      kpPlotRibbon(kp, data=tiles, y0 = cum.sums$C, y1=cum.sums$G, col=base.colors["G"])
      kpPlotRibbon(kp, data=tiles, y0 = cum.sums$G, y1=cum.sums$A, col=base.colors["A"])
      kpPlotRibbon(kp, data=tiles, y0 = cum.sums$A, y1=cum.sums$T, col=base.colors["T"])

      kpLines(kp, data=tiles, y=cum.sums$G, lwd=2)
      kpText(kp, data=tiles[length(tiles)], y=cum.sums$G[nrow(cum.sums)], labels = "%GC", pos=4, clipping=FALSE)
      kpPoints(kp, cex=1, col="red", data=tiles[length(tiles)], y=cum.sums$G[nrow(cum.sums)], clipping=FALSE)
      legend(x = "bottomright", fill = c(input$'basic_genome_info_karyoplote_a_color',
                                         input$'basic_genome_info_karyoplote_t_color',
                                         input$'basic_genome_info_karyoplote_c_color',
                                         input$'basic_genome_info_karyoplote_g_color'),

             legend = c("A", "T","C", "G"),title = "Base Content")
    }

    }
  })
  
  
  
  
  output$'basic_genome_info_karyoplote_plot_dl' <- downloadHandler(
    filename = function() {
      paste("Basic_genome_info_karyoplote_plot.png")
    }, 
    content = function(file) {
      chr_length_info = genome_common$chr_length_info
      cytobands = genome_info_karyoplote_plot()$cytobands
      tiles = genome_info_karyoplote_plot()$tiles
      cum.sums = genome_info_karyoplote_plot()$cum.sums
      base.colors = genome_info_karyoplote_plot()$base.colors
      
      chr_range = GRanges(data.frame(Chr = chr_length_info$Chr,start = 1L, end = chr_length_info$length))
      
      if(input$'basic_genome_info_karyoplote_addtrack'=="None"){
        
        png(file ,width = 1200, height = 800, res = 120,type='cairo')
        kp <- plotKaryotype(genome = chr_range,chromosomes = input$'basic_genome_info_karyoplote_chr_select',cytobands = cytobands)
        dev.off()
        
      }else if(input$'basic_genome_info_karyoplote_addtrack'=="Add ATCG Content"){
        
        png(file ,width = 1200, height = 800, res = 120,type='cairo')
        kp <- plotKaryotype(genome = chr_range,chromosomes = input$'basic_genome_info_karyoplote_chr_select',cytobands = cytobands)
        kpPlotRibbon(kp, data=tiles, y0 = 0, y1=cum.sums$C, col=base.colors["C"])
        kpPlotRibbon(kp, data=tiles, y0 = cum.sums$C, y1=cum.sums$G, col=base.colors["G"])
        kpPlotRibbon(kp, data=tiles, y0 = cum.sums$G, y1=cum.sums$A, col=base.colors["A"])
        kpPlotRibbon(kp, data=tiles, y0 = cum.sums$A, y1=cum.sums$T, col=base.colors["T"])
        
        kpLines(kp, data=tiles, y=cum.sums$G, lwd=2)
        kpText(kp, data=tiles[length(tiles)], y=cum.sums$G[nrow(cum.sums)], labels = "%GC", pos=4, clipping=FALSE)
        kpPoints(kp, cex=1, col="red", data=tiles[length(tiles)], y=cum.sums$G[nrow(cum.sums)], clipping=FALSE)
        legend(x = "bottomright", fill = c(input$'basic_genome_info_karyoplote_a_color',
                                           input$'basic_genome_info_karyoplote_t_color',
                                           input$'basic_genome_info_karyoplote_c_color',
                                           input$'basic_genome_info_karyoplote_g_color'),
               
               legend = c("A", "T","C", "G"),title = "Base Content")
        dev.off()
      }
    }
  )
  
  
  observeEvent(input$'basic_genome_info_select_check',{
    counter$genome_selected_ready = 2
    updatebs4TabItems(inputId = 'sidebar_menu',selected = "genome_viewer")
  })
  
  ## read genome fragement
  target_pool <- reactive({
    input$'basic_genome_info_select_check'   
    isolate({  
      filepath = paste0("./data/",
                        as.character(input$'basic_genome_info_genome_select'),"_",
                        as.character(input$'basic_genome_info_genome_ref_select'),
                        "_result_table.xlsx")
      
      target_pool = openxlsx::read.xlsx(filepath)
      return(target_pool)
    })
  })
  
  
  output$'simulation_check_igvinfo_DT' <- renderDT({
    if(is.null(target_pool())){
      return()
    }else{
      
      target_pool_dt = DT::datatable(
        target_pool(),
        selection = "single",
        rownames = FALSE,
        escape=FALSE,
        options = list(
          autowidth = FALSE,
          columnDefs = list(list(targets="_all",width = '80px')),
          pageLength = 10,
          lengthMenu = c(5,10, 15, 30),
          scrollX = TRUE,
          scrollY = 340
        )
      )
      
      if(input$'dark_mode'){
        target_pool_dt = DT::datatable(
          target_pool(),
          selection = "single",
          rownames = FALSE,
          escape=FALSE,
          options = list(
            autowidth = FALSE,
            columnDefs = list(list(targets="_all",width = '50px')),
            pageLength = 10,
            lengthMenu = c(5,10, 15, 30),
            scrollX = TRUE,
            scrollY = 340,
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"))
        ) %>% 
          formatStyle(columns = colnames(target_pool),backgroundColor = "#282828",color = "#fff",target = 'row')
      }
      return(target_pool_dt)
    }
  })
  
  
  ## For igvShiny add source tmp directory 
  addResourcePath("tracks", "tracks")
  
  
  
  #### igvShiny input Id need igvShiny_0...
  ### IGV Box
  output$igvShiny_0 <- renderIgvShiny({
    
    if(counter$genome_selected_ready!=2){
      return()
    }else{
    genome_ref = genome_info[,c(1,7,10,11,12)] %>% tidyr::separate_rows(c(Reference.Genome.Version,BSgenome,Fasta),sep = ";")
    fasta_name = genome_ref$Fasta[which(genome_ref$Reference.Genome.Version==input$'basic_genome_info_genome_ref_select')]
    
    genome_fasta_path = paste0(genome_common$genome_path,"/",fasta_name)
    
    igvShiny(list(
      genomeName="local",
      initialLocus="",
      fasta = genome_fasta_path,
      index = paste0(genome_fasta_path,".fai"),
      displayMode="SQUISHED"
    ))
    }
  })
  
  ## IGV click info modal
  observeEvent(input$'trackClick', {
    sprintf("--- igv-trackClick event")
    x <- input$"trackClick"
    attribute.name.positions <- grep("name", names(x))
    attribute.value.positions <- grep("value", names(x))
    attribute.names <- as.character(x)[attribute.name.positions]
    attribute.values <- as.character(x)[attribute.value.positions]
    length(attribute.values) = length(attribute.names) 
    tbl <- data.frame(name=attribute.names,
                      value=attribute.values,
                      stringsAsFactors=FALSE)
    showModal(
      modalDialog(size = "m",easyClose=TRUE,{
        
      ## Info DT  
      output$'igvclick_info_DT' <- renderDT({
        
        DT::datatable(
        tbl,
        escape = FALSE,
        selection = "none",
        extensions = 'Scroller',
        options = list(
          pageLength = 50,
          scrollY = "500px",
          scrollX = "300px"
        ),
        fillContainer = T
      )}) 
      
      ## Download 
      # download
      output$'igvclick_info_DT_dl' <- downloadHandler(
        filename = "Gene_Info.xlsx",
        content = function(filename) {
          openxlsx::write.xlsx(tbl, filename)
        }
      )
      
      fluidPage(DTOutput('igvclick_info_DT')) 
      
    },
    footer = tagList(
      downloadBttn('igvclick_info_DT_dl',
                   label = "Download Gene Info table",
                   style = "bordered",
                   color = "primary",size = "sm"),
      HTML('<button class="action-button bttn bttn-bordered bttn-sm bttn-primary bttn-no-outline" id="sample_corplot_md_cancel" type="button" data-dismiss="modal">Cancel</button>')
    )
    
    )
    )
  })
  
  
  ## simulation IGV position ##
  
  observeEvent(input$'simulation_check_igvinfo_DT_rows_selected',{
    
    target_pool = target_pool() 
    
    target_pool_click = target_pool[input$'simulation_check_igvinfo_DT_rows_selected',]
    
    print(paste0(target_pool_click$L.seqnames,":",target_pool_click$L.pos-200,"-",target_pool_click$R.pos+200))
    
    
    
    ## BAM Track
    
    ## targe_range = GRanges(selected_variant_filter_df$filter_df)
    targe_range = GRanges(data.frame(Chr = target_pool_click$L.seqnames,
                                     Start =  target_pool_click$L.pos - 100000 ,
                                     End = target_pool_click$R.pos + 100000))
    
    ## setting ScanBamParam 
    
    bam_params = ScanBamParam(which = targe_range,what = c("seq","qual"))

    if(!is.null(input$'simulation_check_bam_file')){
      
      bamfile = input$'simulation_check_bam_file'$datapath[grep("bam$",input$'simulation_check_bam_file'$datapath)]
      baifile = input$'simulation_check_bam_file'$datapath[grep("bai$",input$'simulation_check_bam_file'$datapath)]
  
      bamfilename = input$'simulation_check_bam_file'$name[grep("bam$",input$'simulation_check_bam_file'$name)]
  
      bam = readGAlignments(file = bamfile,
                            index = baifile,
                            param = bam_params)
  
      bamtrackname = gsub(".sort.bam","",bamfilename)
        
   
      loadBamTrackFromLocalData(session, id="igvShiny_0", trackName = bamtrackname, data = bam)
    
    }
    
    showGenomicRegion(session, id="igvShiny_0", paste0(target_pool_click$L.seqnames,":",
                                                                      target_pool_click$L.pos-200,"-",
                                                                      target_pool_click$R.pos+200))


  })
  
  
  ## GFF3 Annotation Track ##
  
  observeEvent(input$'simulation_check_add_gene_track',{
               
               genome_ref = genome_info[,c(1,7,10,11,13)] %>% tidyr::separate_rows(c(Reference.Genome.Version,BSgenome,Annotation),sep = ";")
               gff3_name = genome_ref$Annotation[which(genome_ref$Reference.Genome.Version==input$'basic_genome_info_genome_ref_select')]
               
               genome_gff3_path = paste0(genome_common$genome_path,"/",gff3_name)
               
               annotation_table = read.table(genome_gff3_path,sep="\t", as.is=TRUE, header=TRUE)
               sprintf("--- about to call loadGFF3rackFromLocalData, dim: %d, %d", nrow(annotation_table), ncol(annotation_table))
               
               
               color.table <- list(processed_transcript="blue",
                                   protein_coding="darkgreen",
                                   retained_intron="brown",
                                   nonsense_mediated_decay="orange",
                                   miRNA="darkred",
                                   default="black")
               
               colorByAttribute <- "biotype"
               
               loadGFF3TrackFromLocalData(session,
                 id="igvShiny_0",
                 trackName = "gene_track",
                 tbl.gff3 = annotation_table,
                 color="blue",
                 colorTable=color.table,
                 colorByAttribute=colorByAttribute,
                 displayMode="EXPANDED",
                 trackHeight=200,
                 visibilityWindow=80000,
                 deleteTracksOfSameName=TRUE)
               
               
               })
  
  ## MAPQ Distribution
  
  bam_mapq_distribution_plot <- reactive({
    
    if(is.null(input$'simulation_check_bam_file')){
      return()
    }else{  
      
      bamfile = input$'simulation_check_bam_file'$datapath[grep("bam$",input$'simulation_check_bam_file'$datapath)]
      baifile = input$'simulation_check_bam_file'$datapath[grep("bai$",input$'simulation_check_bam_file'$datapath)]
      
      bamfilename = input$'simulation_check_bam_file'$name[grep("bam$",input$'simulation_check_bam_file'$name)]
      bamfilename = gsub(".sort.bam","",bamfilename)
      
      bam_params = ScanBamParam(what = c("mapq","isize"))
      
      bam = readGAlignments(file = bamfile,
                            index = baifile,
                            param = bam_params)
      
      mapq_df <- data.frame(Sample = bamfilename,mapq = bam@elementMetadata$mapq)
      bam_mapq_distribution_plot <- ggplot(mapq_df,aes(x = mapq)) + geom_histogram(binwidth = 1,fill = "navy") + 
        labs(y = "Reads Count",x = "MAPQ",title = paste0(bamfilename," MAPQ Distribution"))
      
      print(bam_mapq_distribution_plot)
    }
  })
  
  callModule(
    ggplotZoomDownload,
    'bam_mapq_distribution_plot',
    plotFun = bam_mapq_distribution_plot,
    action = dark_mode_action,
    filename = "Input_bam_MAPQ_distribution.png",
    showwidth = "900px",
    showheight = "700px"
  )
  
  
  
  #######################################
  ####---    Page  Version Log    ---####
  #### 
  

  ## version log
  output$'version_log_reactable' <- renderReactable({
    
  reactable(version_log,
              height = 400,defaultPageSize = 100,
              wrap = FALSE,resizable = TRUE,bordered = TRUE,
              theme = reactable_darktheme,
              ## column width
              columns = list(
                `Change Info` = colDef(minWidth = 800),
                `Update Time` = colDef(minWidth = 120, sticky = "left"),
                 Version = colDef(minWidth = 100, sticky = "left",
                                  # Add a right border style to visually distinguish the sticky column
                                  style = list(borderRight = "3px solid #eee"),
                                  headerStyle = list(borderRight = "3px solid #eee"))
              ))

  })
  
  ## session info
  
  
  output$'session_package_info' <- renderReactable({
    
    package = c(# Bioconductor 3
                "karyoploteR","GenomicAlignments","VariantAnnotation","Gviz",
                # Devtools 3
                "bs4Dash","reactable","igvShiny",
                # CRAN 13
                "magrittr","plyr","dplyr","dtplyr","tidyr","stringr","tibble","cyphr","qs","data.table","lubridate","httr","htmlwidgets",
                # CRAN (Shiny) 14
                "shiny","colourpicker","waiter","shinyWidgets","shinycssloaders","shinyjqui",
                "shinyalert","shinylogs","shinyjs","shinybusy","sortable","shinyFeedback","fresh","cicerone",
                # CRAN (Plot and table) 12
                "RColorBrewer","gplots","ggplot2","plotly","ggrepel","cowplot","echarts4r","showtext","DT","rhandsontable",
                "openxlsx","wordcloud2",
                # Custom BSgenome 3
                "BSgenome.Osativa.ENSEMBLPLANTS.IRGSP1.0",
                "BSgenome.Slycopersicum.SolGenomicsNetwork.SL3.0","BSgenome.Slycopersicum.SolGenomicsNetwork.SL4.0"
                )
    version = apply(matrix(package,ncol=1),1,function(x){as.character(utils::packageVersion(pkg = x))})
    
    package_type = c(rep("Bioconductor",4),rep("Devtools",3),rep("CRAN",13),rep("CRAN (Shiny)",14),
                     rep("CRAN (Plot and table)",12),rep("Custom BSgenome",3))
    
    sessioninfo_df = data.frame(package_type = package_type,package = package,version = version,stringsAsFactors = F)
    
    
    reactable(sessioninfo_df, height = 400,defaultPageSize = 10,
              wrap = FALSE,resizable = TRUE,bordered = TRUE,groupBy = "package_type",
              theme = reactable_darktheme)
  })
  
  
 
  
  
})