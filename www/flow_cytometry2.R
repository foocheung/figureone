library(png)
library(ggimage)
####ADD MEAN
### ADD DELTA TO PLOT
###
#library(ggpubr)
options(digits=4)
options(warn=-1)
library(ggrepel)
library(plyr)
#library(DT)
library(ggforce)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
#library(RMySQL)
#library(d3heatmap)
library(ggfortify)
library(readr)
library("genefilter")
library(stats)
library(shinyjs)
#library(webshot)
library(htmltools)
library(magrittr)
#source("chooser.R")
library(gridExtra)
library(grid)
library(gtools)
library(shinyBS)
#library(DT)
#source("global.R")


ui <- function(request) {
  
 dashboardPage(
  # skin = "yellow",
  
  
  dashboardHeader(title = "A Figure One Web Tool", titleWidth = 650),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      
      id = "tabs",
      menuItem("", tabName = "Figure"),
          status = "primary",
          solidHeader = TRUE
          
     ),
    
    # div(style="display:inline-block, margin-top: 25px;", selectInput(inputId ="auntps",
    #             "Step1: Select Number of TimePoints",
    #             c("",1:10),
    #             selected="",width = "100%")
    #     ),
    
    
   
    
    conditionalPanel(
    
        "input.add > 0",
        column(8, align="center", 
            uiOutput("result1"),   
      conditionalPanel(
        
        condition =   "input.add > 0",  
       
        actionButton("goButton3", "Draw Figure!",icon("picture", lib = "glyphicon"), 
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        
     #   actionButton("", "Run Analysis", icon("paper-plane"), 
      #               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        
        #  actionButton("rmv", "Remove Row"),
      ),
      
      
      checkboxInput('rs', 'Restart', FALSE),
      
      conditionalPanel(
        condition = "input.rs == true",
        tags$a(href="javascript:history.go(0)", 
               popify(tags$i(class="fa fa-refresh fa-5x"),
                      title = "Reload", 
                      content = "Click here to restart the Shiny session and reset all values",
                      placement = "right"))
        
      )
      ),
      
      
      column(8, align="center",    
       div(style="display:inline-block",bookmarkButton(style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
           ),
          
           
    
    checkboxInput('returnpdf', 'Output Plot To PDF?', FALSE),
    
   
           conditionalPanel(
             condition = "input.returnpdf == true",
             
             
             strong("PDF size (inches):"),
             sliderInput(inputId="w", label = "width:", min=3, max=20, value=8, width=100, ticks=F),
             sliderInput(inputId="h", label = "height:", min=3, max=20, value=6, width=100, ticks=F),
             br(),
             downloadLink('pdflink'),
             br(),
             br()
           ) 
    #)
           ,
    
    
    checkboxInput("lb", "Add Titles and Labels", TRUE),
    conditionalPanel(
     "input.lb > 0",
     align="left",  
    textInput('title', HTML("<i class='glyphicon glyphicon-pencil fa-1x' ></i>"),"Add Your Title Here" ),
    textInput('captions', HTML("<i class='glyphicon glyphicon-pencil fa-1x'></i>"), "Add Your Caption Labels here"), 
    textInput('subtitle',  HTML("<i class='glyphicon glyphicon-pencil fa-1x'></i>"), "Add Your Subtitle Labels here"),
    textInput('xlab',  HTML("<i class='glyphicon glyphicon-pencil fa-1x' ></i>"), "Add Your X-axis Labels here"),
    textInput('ylab',  HTML("<i class=' glyphicon glyphicon-pencil fa-1x'></i>"), "Add Your Y-axis Labels here")
    
    )
,
checkboxInput("annt", "Edit TimePoint Labels", TRUE),
checkboxInput("rotx", "Rotate TimePoint Labels by 180", FALSE),
conditionalPanel( condition= "input.annt > 0",
                  align="left",  
                 uiOutput('ann_tps')
              #  outputOptions(, "ann_tps", suspendWhenHidden = FALSE)
),

  
 
br(),br(),
      HTML('<p><center>Further Help ? <br>Contact the developer at <font color="cyan"><br> foo.cheung @ nih . gov </font></center>'),
br(),br(),br()

  )
) 
),
   
  #),
  dashboardBody(
    
    
    
    tags$head(
     # tags$head(tags$style(HTML('
    #  .skin-blue .content-wrapper {
    #    background-color: white;
    #  }
    #'))),
     
    
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                      ")),
      
      tags$script(
        HTML("
             Shiny.addCustomMessageHandler ('hide',function (selector) {
             $(selector).parent().slideUp();
             });"
        )
        )
      ),
    
    fluidRow(
      
      tabItems(
        # First tab content
        
     
        
        tabItem(
          tabName = "Figure",
        
          fluidRow(
            column(
              12,
              offset = 3,
            
            #  conditionalPanel(
        #        "input.add > 0 ",  
             box(
     
        uiOutput('web3')
        
          ) 
        #)
        
        ) ),
          
        fluidRow(
          
            title = "",
            id = "tabset2",
            width = 12,
            height = 800,
            
            align="center",
            conditionalPanel(
              condition =   "input.add == 0",   
              
            box(
             # textOutput("result")
              width = 12,
              
            #  selectInput(inputId ="auntps",
            ##HTML(" <i class='glyphicon glyphicon-time fa-1x'></i> Select Number of TimePoints"),
              #                                                                c("",1:20),selected="",width = "50%")
              
            sliderInput("auntps", 
                        HTML(" <i class='glyphicon glyphicon-time fa-2x' style='color: #FFD700;  border-color: #337ab7'></i> Select Number of TimePoints")
                        ,
                        min = 0, max = 20,
                        value = 0)
            
            
            
            
            
            )         
             
                                    
             
                                             
            
        )                                                       
        ,
            
            
            column(
              
            width=12,
            offset=0.5,
             conditionalPanel(
             condition =   "input.auntps > 0",  
             align="left",
             actionButton("add", "Add Row", icon("plus", lib = "glyphicon"), 
                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
             )
              
              
            )
        )
          
            )
          )
        )
        )
      )
  #    )
 #     )
}
        
   #

shinyApp(
  ui,enableBookmarking = "url",
  server = function(input, output,session) {
    
    #addClass(selector = "body", class = "sidebar-collapse")
    #  observe({ 
    #    vidurl <- paste0("https://www.youtube.com/embed/jc1aJwoOu0A")   
    #  })
    
    output$result1 <- renderUI({
      req(input$auntps)
      HTML(paste("<center><i> <i class='glyphicon glyphicon-time fa-1x'></i> <b> ",
                 "<font color='red'>", input$auntps,"</font>",
                 'TimePoints Selected', 
                 sep=' '))
    })
    
    tags$footer(title="Your footer here", align = "right", style = "
position:absolute;
                bottom:0;
                width:100%;
                height:50px; /* Height of the footer */
                color: white;
                padding: 10px;
                background-color: black;
                z-index: 1000;"
    )
    
    
    images <-  c(
      "Select",  "syringe_b", "syringe_r", "syringe2","drug_r","drug_b",  "drug2", "blood_sample","blood_sample2","blood_sample3",
                 "RNA_Seq", "flow_cytometry", "proteomics","HAI","microneutralization", "bcell_ellispot",
      "subjects_b","subjects_r", "subjects_rb", "subjects_br",
      "mouse", "researcher", "blood_cells", "vectors", "smallmolecules", "bioinformatics", "antibodies", "assays", "phage",
      "cell_lines","chip","epigenomics","serology","microbiome" ,"blank")
                 
    
    drug_list<-c("drug_r","drug_b",  "drug2")
    vacc_list<-c("syringe_b", "syringe_r", "syringe2")
    blood_list<-c("blood_sample", "blood_sample2", "blood_sample3")
    sbj_list<-c("subjects_b","subjects_r", "subjects_rb", "subjects_br")
    serology_list<-c("HAI","microneutralization", "bcell_ellispot")
    transcriptomics_list<-c("chip","RNA_Seq")
    proteomics_list<-c("proteomics", "phage",  "heatmap" )
    cells_list<-c("flow_cytometry","flow_cytometry2","blood_cells" )
    misc_list<-c( "mouse", "researcher", "vectors", "smallmolecules", "bioinformatics", "antibodies", "assays", 
                  "cell_lines","epigenomics","serology","microbiome" ,"blank")
   color_list<-c("red", "blue", "green", "yellow", "purple")
  tool_list<-c("insert_text","draw_arrow")
    
    onBookmarked(function(url) {
      showModal(
        urlModal(url, title = "This is the bookmark URL", subtitle = "something")
      )
    })
  
    
    
    
    observeEvent(input$txtbx,{
      if (input$file1 == "") return(NULL)
      session$sendCustomMessage (type="hide", "#ex")
      print(input$file1)
    })


    
      
    tgp<-reactive({
      
      #myData <- filedata()$myData
      myData <- dataM()$data
      tps<- as.data.frame(table(myData$TimePoint))
      tpg1<- tps %>% filter(Freq > 0) %>% arrange(desc(Freq) )
      colnames(tpg1)<-c("TimePoint", "Frequency")
      
      timepoints<-tpg1[1]
      
      return(list("timepoints"=timepoints))
    })
    
    
    output$web3 <- renderUI( {
     # req(input$goButton3)
    #  if (is.null(input$goButton3))
    #    return(NULL)
    #  if (input$goButton3==0)
    #    return(NULL)
      
      
      isolate({
      tagList(
       
          plotOutput('plot2')
        )
          
        })
    }
    )
    
    
    
    # output$web2 <- renderUI( {
    # 
    #   
    #   myData3 <- dataM()$data
    #   req(input$add)
    #   
    #   ch<- input$add
    #   i=ch
    #   
    #   sct<- images
    #   
    #   tagList(
    #     
    #    box(
    #       width=12,  
    #      # checkboxInput("crect1", "Add Rectangle Region 1", FALSE),
    #    
    #       conditionalPanel(
    #         "input.crect1 > 0 ",   
    #         selectizeInput(
    #           'rect1', 'Select TimePoints', choices =  c(sort(as.numeric(unlist(t(tgp()$timepoint))))),selected=c(sort(as.numeric(unlist(t(tgp()$timepoint))))[1],sort(as.numeric(unlist(t(tgp()$timepoint))))[4]),
    #           multiple = TRUE, options = list(maxItems = 2)),
    #         checkboxInput("crect2", "Add Rectangle Region 2", FALSE)
    #       ),
    #       conditionalPanel(
    #         "input.crect2 > 0 ",   
    #         selectizeInput(
    #           'rect2', 'Select TimePoints', choices =  c(sort(as.numeric(unlist(t(tgp()$timepoint))))),selected=c(sort(as.numeric(unlist(t(tgp()$timepoint))))[4],sort(as.numeric(unlist(t(tgp()$timepoint))))[6]),
    #           multiple = TRUE, options = list(maxItems = 2))
    #         ,
    #         checkboxInput("crect3", "Add Rectangle Region 3", FALSE)
    #         
    #       )  
    #        ,
    #        conditionalPanel(
    #          "input.crect3 > 0 ",
    #          selectizeInput(
    #            'rect3', 'Select TimePoints', choices =  c(sort(as.numeric(unlist(t(tgp()$timepoint))))),selected=c(sort(as.numeric(unlist(t(tgp()$timepoint))))[6],sort(as.numeric(unlist(t(tgp()$timepoint))))[9]),
    #            multiple = TRUE, options = list(maxItems = 2))
    #        )
    #       
    #     )
    #   )
    #   
    #   
    #   
    # })
    # 
    
    
    
    
    
    addItem <- function(id) {
     
      id <- id  
      insertUI(
        
          selector = "#add",
          where ="beforeBegin",
          tagList(
        #    fluidRow(
            
          #  fluidRow(
              #column(
               # 12,
                #offset = 1,
            box(
              offset=1,
              solidHeader=TRUE,
              Title="Row 1",
              width=2,
              height="400px",
              id= paste0('bx', id, sep="") ,
             ## actionButton("add", "Add Row", icon("plus", lib = "glyphicon"))
              
             align = 'left',
             textInput( 
               width="150px",
               #paste0('c', id) , label=paste0("Row", id, 'Name',   sep='' ), paste0('Row', id, 'Name')
               paste0('c', id) , label=HTML("<i class='glyphicon glyphicon-pencil fa-1x' style='color: #FF5347;  border-color: #337ab7'></i>",  sep='') ,
               paste0('Label For Row ', 
                      id)
               
                   ),
             
             
             #####NEED TO ADD 0.25 intervals in ALL NUMBERS MISSING PN MIDDLE NUMBERS
              selectizeInput(
                paste0('bbb', id  ) , 
                paste0('Row', id, "Position"), 
              #  label=HTML("<i class='glyphicon glyphicon-pencil fa-1x'></i>"), paste0('Row', id, "Position"),
                choices =   c(2.5/10,5/10,7.5/10,10/10,12.5/10,15/10,17.5/10,20/10,22.5/10,25/10,27.5/10,30/10,
                              32.5/10,35/10,37.5/10,40/10,42.5/10,45/10,47.5/10, 50/10,
                              52.5/10, 55/10,57.5/10,60/10,62.5/10,65/10,67.5/10,70/10,
                              72.5/10,75/10,77.5/10,80/10,82.5/10,85/10,87.5/10,90/10,
                              92.5/10,95/10,97.5/10,100/10,
                              102.5/10,105/10,107.5/10,
                              110/10,112.5/10,115/10,117.5/10,120/10,122.5/10,125/10,127.5/10,130/10,
                              132.5/10,135/10,137.5/10,140/10,142.5/10,145/10,147.5/10,150/10,
                              152.5/10,155/10,157.5/10,160/10,162.5/10,165/10,167.5/10,170/10,172.5/10,
                              175/10,177.5/10,180/10,182.5/10,185/10,187.5/10,190/10,192.5/10,195/10,197.5/10,200/10
                              
                              ),selected=id,multiple = FALSE) ,
             #,width="100px"),
              
              
              selectizeInput(
   paste0('a', id ) , 
    ## label=HTML("<i class='glyphicon glyphicon-time fa-1x'></i>", paste0('Row', id, "TimePoints"), sep=''),
   label=HTML("<i class='glyphicon glyphicon-time fa-1x' style='color: #FFD700;  border-color: #337ab7'></i>"),
   
   choices =c(tgp()$timepoint),selected=c(tgp()$timepoint),multiple = TRUE),
   #,width="100px"),
              
   
 
              selectizeInput(
                #width="100px",#
                paste0('b', id  ) , 
                #paste0('Row', id, "Image"), 
                label=HTML("<i class='glyphicon glyphicon-picture fa-1x' style='color: #fff; background-color: #337ab7; border-color: #2e6da4'></i>"),
                             

   
 choices =  
   list(
    "Vaccine" =  c(vacc_list),
     "Subjects" = c(sbj_list),
    "Samples" = c(blood_list),
    "Drugs"=c(drug_list),
    "serology" = c(serology_list),
 "transcriptomics" = c(transcriptomics_list),
    "proteomics" = c(proteomics_list), 
 "cell" = c(cells_list),
   "colors"=c(color_list),
  "misc" = c(misc_list),
 "tools"=c(tool_list)
   ),
 

     
   

                options = list(render = I(sprintf(
                  "{
                  option: function(item, escape) {
                  return '<div><img width=\"30\" height=\"30\"  src=\"'   + escape(item.value)   + '.png\" />' + escape(item.value) + '</div>';
                  }
    }"      ))
              )
              #  )
              
   
            ))
              
               # )
            
                )
    #      )
        
      )
    }
    
#    removeUI <- function(selector,
##                         multiple = FALSE,
#                         immediate = FALSE,
#                         session = getDefaultReactiveDomain()) {
##      
#      force(selector)
#      force(multiple)
#      force(session)
#      
#      callback <- function() {
#        session$sendRemoveUI(selector = selector,
#                            multiple = multiple)
#      }
#      
#      if (!immediate) session$onFlushed(callback, once = TRUE)
#      else callback()
#    }
    
    observeEvent(input$add, {
    
addItem(input$add)        
    }
, ignoreInit = TRUE)
    
    
    
    
    
    #observeEvent(input$rmv, {
    #  removeUI(
        
     #  selector = "div:has(> #bx3)",
     # selector = "div:has(> #a1)"
    
    #  )
    #})
  
    
    onRestore(function(state) {
    
       # for(i in 1:input$add ) {    
      for (i in seq_len(input$add)) {
        addItem(i)
        
      }
      }
    )
    
    
    output$ann_tps<-renderUI({
     if (is.null(input$auntps)){
        
    return(NULL)    
      }
      req(input$auntps > 0)
      
    lapply(1:input$auntps, function(i) {
      
      tagList(
        textInput(
              inputId = paste0("timepts", i),
             label =HTML("<i class='glyphicon glyphicon-time fa-1x' style='color: #FFD700;  border-color: #337ab7'></i> Label For TimePoint", i , sep=''),
              i
        )
      )
      }
      )

        })
  
   
    
    
    
    
    observeEvent(input$addtp, {
    
      
      
      isolate(
        insertUI(
          selector = "#addtp",
          where ="beforeBegin",
          
          tagList(
            box(
              width=2,
              numericInput(   
                  paste0('tpt', input$addtp, "TimePoints"),label=paste0("TimePoint",input$addtp, sep=""), value ="" , width="100px")
            
              )
            )
          )
        )
      
    })
    
    
                
  dddd<-reactive({
      
   # rrrr<<-input$addtp
    library('foreach')
    req(input$gotp)
    
      i_squared <- integer(input$addtp)  

      
      for(i in 1:input$addtp ) {
        
        i_squared[i] <- eval(parse(text=paste0("input$tpt", i, "TimePoints", sep="")) )  
        
      }
      
      
  #  testt<<-  i_squared
    
    updateSelectInput(session, "inSelect2",
                      label = paste("Select label", c_label),
                      choices = s_options,
                      selected = paste0("option-", c_num, "-B")
    )
    
    }
    )
    
   
    ##########################START Summary TAB
    
    
    
    
    
    tgp<-reactive({
      myData <- dataM()$data
      #myData <- filedata()$myData
      
tps<-matrix(1:input$auntps)
   
#   tps<- as.data.frame(table(myData$TimePoint))
     # tpg1<<- tps %>% filter(Freq > 0) %>% arrange(desc(Freq) )
      colnames(tps)<-c("TimePoint")
      
      timepoints<-tps
      
      return(list("timepoints"=timepoints))
    })
    
    

    
    
    library(raster)
    
    maddy<-function(x) { out<- mad(x, center = median(x), constant = 1.4826,na.rm = FALSE, low = FALSE, high = FALSE)    }
    
    cccv<-function(x) { out<- cv(x)    }
    
    s<-function(x) { out<- sd(x)    }
    m<-function(x) { out <- mean(x) }
    
    
    output$summatout <- DT::renderDataTable(
      summary()$summat,
      extensions = 'Buttons', options = list(
        lengthMenu = c(50, 100, 200), pageLength = -1,
        
        dom = 'Blfrtip'
        ,
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        
      )
      
    )
    
    
    
    
    
    
    
    ##########################END SUMMARY TAB################################
    
    ##########################START FILTERED TAB
    output$filter_sampleII <- renderUI({
      
      # aaaa<- tgp()$timepoint
      #aaaa<- sort(as.numeric(unlist(t(aaaa))))
      
      #  aaaa<- sort(as.numeric(unlist(t(tgp()$timepoint))))
      tpg1<-time_p_g1()$tpg1
      tagList(
        box(
          
          
          selectInput(
            inputId = "fil_by_tp",
            label = "Filter By TimePoint",
            c(sort(as.numeric(unlist(t(tgp()$timepoint))))),
            # c(0,1,7,30,37,180,187,210,360),
            #selected = c(t(aaaa)),
            selected = c(sort(as.numeric(unlist(t(tgp()$timepoint))))),
            # selected=c(0,1,7,30,37,180,187,210,360),
            selectize = TRUE,
            multiple = TRUE
            
          ), 
          br(), br()
          ,
          fileloaded()
        )
      )
      
    })
    # 
    ##########################END FILTERED TAB################################
    
    ##########################START FILE UPLOAD TAB
    
    filedata <- reactive({
      
      req(input$file1)
      inFile <- input$file1
      is.null(inFile)
      inFile2 <- input$file2
      
      inFile3 <- input$file3
      
      myData<-read.csv(inFile$datapath, header=T, fill=TRUE, sep="\t")
     # myData<-myData[!apply(myData == "", 1, all),]
      
      
      
      #    if (input$idtp > 0){
      ###   rownames(myData)<-(make.names(paste(myData$Sample.ID,myData$TimePoint,sep="_"),unique=TRUE))
      #      }
      #    else{
      #rownames(myData)<-(make.names(paste(myData$Sample.ID,sep="_"),unique=TRUE))
     # rownames(myData)<-(make.names(paste(myData$TimePoint,myData$Sample.ID,sep="_"),unique=TRUE))
      
      myData <- cbind(myData, "SampleID"=1:nrow(myData) ) 
      rownames(myData)<-myData$SampleID
      tttt<-myData
      #    }
      
      #    if ((is.null(input$file2))){
      
      #    }
      #   else{
      #      tofilter<-read.csv(inFile2$datapath, header=F, fill=TRUE, sep="\t")
      #      coln<-t(tofilter)
      #      coln2<-c("Sample.ID", "Groups")
      #      coln3<-c(coln, coln2) 
      #      myData<-myData[,colnames(myData)%in%coln3] 
      #    }
      
      
      
      #    if ((is.null(input$file3))){
      
      #    }
      #    else{
      #      rtofilter<-read.csv(inFile3$datapath, header=F, fill=TRUE, sep="\t")
      #      rown<-t(rtofilter)
      #      
      #      myData<-myData[grep(
      #        paste(rown, collapse = '|'),
      #        rownames(myData),
      ##        ignore.case = TRUE
      #      ),]
      
      #      }
      
      
      return(list("myData"=myData))
    })
    
    
    dataM <- reactive({
          req(input$auntps> 0)
          tps<-matrix(1:input$auntps)
            colnames(tps)<-c("TimePoint")
          
          timepoints<-tps
          data<-timepoints 
          
          data <- cbind(data, "SampleID"=1:nrow(data) ) 
      
          rownames(data)<-data[,1]  
          
      return(list("data"=data))
    })
    
    
    
    
  
    
    ##########################END FILE UPLOAD TAB
    
    
    
    
    
    

    
    wrapper <- function(x, ...) 
    {
      paste(strwrap(x, ...), collapse = "\n")
    }
    
    
    
    
    output$plot2 <- renderPlot({
    #  if (is.null(input$auntps)){
        
    #    return(NULL)    
    #  }
      if (is.null(input$goButton3) |input$goButton3 == 0 )
        return(NULL)
      
    #  if (input$goButton3 == 0)
    #    return(NULL)
    #  print(input$add)
      
    #  if (is.null(input$add))
    #    return(NULL)
      
    #  if (is.null(input$a))
    #    return(NULL)
     # if (is.null(input$b))
  #     return(NULL)
  #    if (is.null(input$ccc))
  #      return(NULL)
      
     # req(input$auntps > 0)
     # req(input$add < 11)
      #req(input$add > 1)
      
          #  req(dataM()$data)
      #req(filedata()$myData)
    # req(input$a)
   #   req(input$b)
  #    req(input$ccc)
    #  req(input$c1)
      isolate ({
           tttt<-input$add
      
      
        #myData3 <-filedata()$myData 
     myData3 <-dataM()$data
     
    # if (is.null(input$goButton3))
    #    return()
      #if (input$goButton3==0)
      #  return()

        
       # my_title <- "Phase I Study of the Safety of a Replication-Defective Herpes Simplex Virus-2 Vaccine, HSV529, in Adults Aged 18 to 40 Years With or Without HSV Infection"
      #myData3 <- dplyr::filter(myData3, !grepl('t', TimePoint))
    #  myData4 <- dplyr::filter(myData3, !grepl('t', TimePoint))
    #  myData4$TimePoint <- as.character(myData4$TimePoint)
    #  myData4$TimePoint <- factor(myData4$TimePoint, levels=unique(myData4$TimePoint))
    
        blank = sample("./www/blank.png")
        
          test<-myData3 
       p <-
          ggplot(
            #data = test, 
            #     aes(
       
          #  x=as.factor(c(1:input$auntps)),
          #  y =as.numeric(input$add) * 10  +30 )
          
          
          data=data.frame(x=as.factor(c(1:input$auntps)), y=(10) ), aes(x=x,y=y) 
         ) +
     #  geom_blank() +
        
          theme_bw()+
          theme(
          ##  axis.line.y = element_blank(),
           axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            text = element_text(size = 13, face = "bold"),
            #  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          #  panel.background = element_blank(), axis.line = element_line(colour = "black"),
            plot.title = element_text(size = 12, face = "bold"),
          #  panel.border = element_blank(),
      #  panel.grid = element_line(color = "grey80"),
        #  panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y =element_blank(),
            #  axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
            
            axis.title.x = element_text(margin = margin(t = 20)) 
     
            
           )  
       
   ###   p <- p +  scale_x_continuous(breaks = 1:input$auntps
       
         if (input$rotx  >0 ){
      #    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
           p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust=0))
         }
         
       
        
       p <-  p +     ggtitle(input$title2) +
         
         labs(title =wrapper(input$title, width = 90),
          
              subtitle = wrapper(input$subtitle, width = 100),
              caption = wrapper(input$captions, width = 100), 
              x = input$xlab, y = input$ylab)
       

    
#if (input$rotx == TRUE ){
   
  #P <- p +  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) 
#}

      

      tttt<-input$b
      insert_text = sample("./www/text.png")
      draw_arrow = sample("./www/draw_arrow.png")
      blood_sample3 = sample("./www/blood_sample3.png")
      syringe_b = sample("./www/syringe_b.png")
      syringe_r = sample("./www/syringe_r.png")
      syringe2 = sample("./www/double_injection.png")
      drug_b = sample("./www/drug_b.png")
      drug_r = sample("./www/drug_r.png")
      drug2 = sample("./www/drug2.png")
      blood_sample = sample("./www/blood_sample.png")
      RNA_Seq = sample("./www/RNA_Seq.png")
      flow_cytometry = sample("./www/flow_cytometry.png")
      flow_cytometry2 = sample("./www/flow_cytometry2.png")
      microneutralization = sample("./www/microneutralization.png")
      proteomics = sample("./www/proteomics.png")
      bcell_ellispot = sample("./www/bcell_ellispot.png")
      pair=sample("./www/pair.png")
      subjects_b = sample("./www/subjects_b.png")
      subjects_r = sample("./www/subjects_r.png")
      subjects_rb = sample("./www/subjects_rb.png")
      subjects_br = sample("./www/subjects_br.png")
      Select = sample("./www/Select.png")
      blank = sample("./www/blank.png")
      chip= sample("./www/chip.png")
      epigenomics= sample("./www/epigenomics.png")
      heatmap= sample("./www/heatmap.png")
      serology= sample("./www/serology.png")
      microbiome= sample("./www/microbiome.png")
      
  mouse= sample("./www/mouse.png")
      researcher= sample("./www/researcher.png")
      blood_cells= sample("./www/blood_cells.png")
      vectors= sample("./www/vectors.png")
      smallmolecules= sample("./www/smallmolecules.png")
      bioinformatics= sample("./www/bioinformatics.png")
      antibodies= sample("./www/antibodies.png")
      assays= sample("./www/assays.png")
      phage= sample("./www/phage.png")
    
      blood_sample2 = sample("./www/blood_sample2.png")
      HAI = sample("./www/HAI.png")
      
      
      
      red= sample("./www/red.png")
      yellow= sample("./www/yellow.png")
      blue= sample("./www/blue.png")
      purple= sample("./www/purple.png")
      green= sample("./www/green.png")
      
      
        #"mouse", "researcher", "blood_cells", "vectors", "smallmolecules", "bioinformatics", "antibodies", "assays", "phage",
      #"cell_lines"
      
     # cccc<-paste0('img_',input$b1,sep="")
    #  ann <- paste0('img_',input$b1,sep="") 
    
      library('foreach')
     
    # pl <- vector("list", length = input$add)
    
     
      pl <- vector("list", length =  input$add)
                                     
    p <- p +  geom_image(data=data.frame(x=as.factor(c(1:input$auntps)), y=(10)), aes(x=x,y=y), image=blank)
    #+
     #  scale_x_continuous(sec.axis = dup_axis())
    #   
   ##  print (paste0("1", input$add))
    
     if (input$add == 0){
##     print (input$add)
       return(NULL)
       }
else  if (input$add > 0){
       
    
 
 
       foreach (i=1:input$add, .combine=c) %do% { 
         
        
         
         
         ee<-eval(parse(text = paste0('input$b', i,sep="")))
         
         tt<-paste0('input$a',i,sep="")
         
         ttt3<-paste0('input$bbb',i,sep="")
         cnt <- as.numeric(as.character(c(eval(parse(text=ttt3))))) * 14
         ff<-paste0('input$c',i,sep="")
         
         
            
         cccc <-input$auntps
         
         
         
        # print(ee)
        # print(tt)
        # print(ttt3)
        # print(cnt)
        # print(ff)
       #  print (ee)
        ## im<-eval(as.name(ee))
        
         #print(ee)
         if (is.null(ee))
         req(eval(as.name(ee)) > 0 )
         
         im<- eval(as.name(ee))
       #  im<-get(ee)
     ##   print(factor(as.character(c(eval(parse(text=tt))))))
       #  p <- p + 
           
         #  geom_image(
        #     data=data.frame(x=factor(as.character(c(eval(parse(text=tt))))), 
         #                    y=cnt ), aes(x=x,y=y) , image=yellow ) +
        #   annotate(geom="text", x=-(as.numeric(cccc)/11), y=cnt, label=eval(parse(text=ff)) ,color="black", parse=FALSE, fontface=2) +
         
        #   geom_segment(x=1,xend=input$auntps,y=cnt,yend=cnt,color="grey80", alpha=0.05,linetype="dashed") 
         
         
         
         
         if (ee == 'insert_text'){ 
           df<-  data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt )  
           
           #annotate("text", x = 4, y = 25, label = "Some text")
           p <- p +     annotate("label", x=as.numeric(as.character(df$x[1])), y=cnt + 1.8, label=eval(parse(text=ff)), fill ="red", colour = "white", fontface = "bold" )
                             #,color="black", parse=FALSE, fontface=2,  hjust = -.5,stat="identity")
         } 
         
         
         
                 else if (ee == 'draw_arrow'){
                      
                                        
                df<-  data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt )    
                       
   # p <- p +     geom_segment(
  #        x=as.numeric(as.character(df$x[1]))+0.10, color="black", size=2,xend=as.numeric(as.character(df$x[2]))-0.10,
  #        y=cnt,yend=cnt,arrow = arrow(length = unit(0.1, "cm")
  #                                  ), stat="identity"
   #                       )
   
                 p<- p + annotate(geom="segment",x=as.numeric(as.character(df$x[1]))+0.10, color="black", size=2,xend=as.numeric(as.character(df$x[2]))-0.10,
                     y=cnt,yend=cnt,arrow = arrow(length = unit(0.1, "cm")
                     ), stat="identity" )
    
    #+
     #   annotate("text", x=as.numeric(as.character(df$x[1])), y=cnt + 1, label=eval(parse(text=ff)) ,color="black", parse=FALSE, fontface=2,  hjust = -.5,stat="identity")
    
    #+
    #   geom_text( x=as.numeric(as.character(df$x[1])), y=cnt + 1.5, label=eval(parse(text=ff)) ,color="black", parse=FALSE, fontface=2,  hjust = -.5)
                        
                      }
         
       
           else{
      p <- p +     geom_image(
                   data=data.frame(x=factor(as.character(c(eval(parse(text=tt))))), 
                  #levels = unique(as.character(c(eval(parse(text=tt)))))),
                  y=cnt ), aes(x=x,y=y) , image=im ) +
        #geom_segment(x=1,xend=input$auntps,y=cnt,yend=cnt,color="grey80", alpha=0.05,linetype="dashed") +
         annotate(geom="segment",x=1,xend=input$auntps,y=cnt,yend=cnt,color="black", alpha=0.1,linetype="dashed" ) +
       annotate(geom="text", x=-(as.numeric(cccc)/11), y=cnt, label=eval(parse(text=ff)) ,color="black", parse=FALSE, fontface=2, colour = "white", fontface = "bold")
           }
           #geom_hline(yintercept=cnt,  element_line(color = "grey80"), linetype="dashed") 
         
        # p <- p +        annotate(geom="text", x=-(as.numeric(cccc)/11), y=cnt, label=eval(parse(text=ff)) ,color="black", parse=FALSE, fontface=2) 
           
      }
     }
      
       
       
       
     #}
    

      
     rrr<-factor(as.character(input$rect1), levels = unique(as.character(input$rect1)))
     rrr2<-factor(as.character(input$rect2), levels = unique(as.character(input$rect2)))
     rrr3<-factor(as.character(input$rect3), levels = unique(as.character(input$rect3)))
     
     
     start_rec <- as.numeric(input$add) *  10 +20
     end_rec <- start_rec + start_rec * .25
     
     #  if (is.null(input$crect1)  | (input$crect1 ==0) ){
     # 
     #  }
     # 
     # 
     # else if (input$crect3 == 'TRUE' ){
     #   
     #   p <- p +
     #     annotate("rect", xmin=rrr2[1], xmax=rrr2[2],  ymin=start_rec, ymax=end_rec , alpha=.4,fill="yellow") +
     #     annotate("rect", xmin=rrr[1], xmax=rrr[2],  ymin=start_rec, ymax=end_rec , alpha=.4,fill="blue") +
     #   annotate("rect", xmin=rrr3[1], xmax=rrr3[2],  ymin=start_rec, ymax=end_rec , alpha=.4,fill="green") 
     #   
     #   }
     # 
     # 
     # 
     #  else if (input$crect2 == 'TRUE' ){
     #   
     #   p <- p +
     #     annotate("rect", xmin=rrr2[1], xmax=rrr2[2],  ymin=start_rec, ymax=end_rec , alpha=.4,fill="yellow") +
     #     annotate("rect", xmin=rrr[1], xmax=rrr[2],  ymin=start_rec, ymax=end_rec , alpha=.4,fill="blue")
     # }
     # 
     #   else if (input$crect1 == 'TRUE' ){
     #     
     #      p <- p +
     #      annotate("rect", xmin=rrr[1], xmax=rrr[2],  ymin=start_rec, ymax=end_rec , alpha=.4,fill="blue")
     #  }
      

    #if (is.null(input$crect2)  | (input$crect2 ==0) ){

     # }
      #else 
   ##  if  (input$crect2[2] > 0 ){
   
         # p <- p +
        #  annotate("rect", xmin=rrr2[1], xmax=rrr2[2],   ymin=start_rec, ymax=end_rec, alpha=.4,  fill="yellow")
    ##  }

    # else if (is.null(input$crect3)  | (input$crect3 ==0) ){

    #  }else if (input$rect3[2] > 0 ){
    
    #    p <- p +
    #      annotate("rect", xmin=input$rect3[1], xmax=input$rect3[2],  ymin=start_rec, ymax=end_rec, alpha=.4 , fill="green")
    #  }
   

    p<- p+ expand_limits(x = -(as.numeric(cccc)/4) ) +  scale_y_reverse(limits=c(end_rec,0  ) )
      
     #   scale_x_continuous(sec.axis = dup_axis()) +  scale_y_continuous(sec.axis = dup_axis())
    
      #!== 'undefined'
     aval<-list()
      for(i in 1:input$auntps) {
        if (i == 'undefined'){
          return()
        }
        else{
       
          
          
          # if ( i == '' |  ){
         itp<- paste0("input$timepts", i, sep="")
        #inputId = paste0("timepts", i),
        #aval[i]=paste0("timepts", i)
         aval<-append(aval, eval(parse(text = itp)) ) 
        # aval<-append(aval, itp) 
        #print (itp)
        }
        
        }
      
    #  rrrrt<<-input$auntps
    #  rrrr<<-aval
    # p <-p +  scale_x_discrete(labels=c(input$timepts1, input$timepts2, input$timepts3, input$timepts4,input$timepts5,input$timepts6 ,input$timepts7,input$timepts8,input$timepts9,input$timepts10 )) 
      p <-p +  scale_x_discrete(labels=aval, position = "top") 
      #+  scale_x_continuous(sec.axis = dup_axis())
  #   p <- p +  geom_image(data=data.frame(x=c(1:10), y=(10)), aes(x=x,y=y), image=blank) +  scale_x_discrete(labels=c(1:10)) +xlim(0,10)
  #     annotate(geom="text", x=0, y=10, label="q" ,color="black", parse=FALSE, fontface=2) 
    # p<- p +   scale_x_discrete(labels=c(factor(1:20) ))
       plot (p ,  width = 7, height = 6, units = 'in',res = 500)
#     plot (p +  scale_x_discrete(labels=c(1:10)), width = 6, height = 6, units = 'in',res = 500)
})


     if(input$returnpdf){
       pdf("plot.pdf", width=as.numeric(input$w), height=as.numeric(input$h))
       print(p)
       
    
       dev.off()
     }
      
    })
    
    
    output$pdflink <- downloadHandler(
      filename <- "myplot.pdf",
      content <- function(file) {
        file.copy("plot.pdf", file)
      }
    )
    
    
    ###################################
    
    
    
    
    
    
  })


  