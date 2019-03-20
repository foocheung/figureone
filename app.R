##libraries that needs to be installed from CRAN or Bioconductor

library(Biobase)
library(rintrojs)
library(ggpubr)
options(digits=4)
options(warn=-1)
library(png)
library(ggimage)

library(ggrepel)
library(plyr)
library(ggforce)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(ggfortify)
library(readr)
library(stats)
library(shinyjs)
library(htmltools)
library(magrittr)
library(gridExtra)
library(grid)
library(gtools)
library(shinyBS)



ui <- function(request) {
  
  dashboardPage(
   
    
    #Title
    dashboardHeader(title = "A Figure One Web Tool", titleWidth = 650),
    
    dashboardSidebar(
      width = 350,
      
      sidebarMenu(
        
        
        id = "tabs",
        
        
        menuItem(
           #Templates
          "Draw From Templates",
          tabName = "Templates",
          icon = shiny::icon("file-pdf-o")
        ),
        
        menuItem(
          #scratch
          "Draw From Scratch", tabName = "Figure"),
        status = "primary",
        solidHeader = TRUE
        
      ),
      
      conditionalPanel(
        #Timepoints activates sidebar and Draw Button
        condition =   "input.auntps > 0",   
        column(8, align="center", 
               uiOutput("result1"),  
               
               conditionalPanel(
                 
                 condition =   "input.auntps > 0", 
                 actionButton("goButton3", "Draw Figure!",icon("picture", lib = "glyphicon"), 
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 checkboxInput('adv', 'More Options', FALSE)
                 
                 
                  ),
               
               uiOutput("result1s"),
               
               HTML('<li><a href="https://foocheung.shinyapps.io/figureone" target="_blank">Start New Drawing</a></li>')
               
             
        ),
         
        
        column(8, align="center",    
               
               #PDF output and download
               
               checkboxInput('returnpdf', 'Output Plot To PDF?', FALSE),
               
               
               conditionalPanel(
                 condition = "input.returnpdf == true",
                 
                 
                 strong("PDF size (inches):"),
                 div(style="display:inline-block",  sliderInput(inputId="w", label = "width:", min=3, max=20, value=8, width=100, ticks=F)),
                 div(style="display:inline-block", sliderInput(inputId="h", label = "height:", min=3, max=20, value=6, width=100, ticks=F)),
                 br(),
                 downloadLink('pdflink'),
                 br(),
                 br()
               ) 
               ,
               
               #labels: title, subtitle, captions,xlabels,ylabels
               checkboxInput("lb", "Add Titles and Labels", TRUE),
               conditionalPanel(
                 "input.lb > 0",
                 align="left",  
                 textInput('title', HTML("<i class='glyphicon glyphicon-pencil fa-1x' style='color: #fff;  background-color: #FA9900; border-color: #2e6da'></i>"),"Add Your Title Here" ),
                 textInput('captions', HTML("<i class='glyphicon glyphicon-pencil fa-1x' style='color: #fff;  background-color: #FA9900; border-color: #2e6da'></i>"), "Add Your Caption Labels here"), 
                 textInput('subtitle',  HTML("<i class='glyphicon glyphicon-pencil fa-1x' style='color: #fff;  background-color: #FA9900; border-color: #2e6da'></i>"), "Add Your Subtitle Labels here"),
                 textInput('xlab',  HTML("<i class='glyphicon glyphicon-pencil fa-1x' style='color: #fff;  background-color: #FA9900; border-color: #2e6da'></i>"), "Add Your X-axis Labels here"),
                 textInput('ylab',  HTML("<i class=' glyphicon glyphicon-pencil fa-1x' style='color: #fff;  background-color: #FA9900; border-color: #2e6da'></i>"), "Add Your Y-axis Labels here")
                 
               )
               ,
               checkboxInput("annt", "Edit TimePoint Labels", TRUE),
               checkboxInput("rotx", "Rotate TimePoint Labels by 180", TRUE),
               
               div(style="display:inline-block",  sliderInput(inputId="sp_lab", label = "Adjust Label Position:", min=-2, max=2, value=0, step = 0.25, width=200, ticks=T)),
               
               conditionalPanel( condition= "input.annt > 0",
                                 align="left",  
                                 uiOutput('ann_tps')
                                 
               ),
               
               
               ##Contact info
               br(),br(),
               HTML('<p><center>Further Help ? <br>Contact the developer at <font color="cyan"><br> foo.cheung @ nih . gov </font></center>'),
               br(),br(),br()
               
        )
      )
      
    )
      
      
    ),
    
        dashboardBody(
      
      id="headp1",
      tags$head(
        
    ##CSS    
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
          #Landing Page with Templates shown
          tabItem(
            
            tabName = "Templates"
            ,
            
            column(
              12,
              align="center",
              
              box(
                width = 12,
                HTML('<center>'),
                HTML('<H1> A Figure One Web Tool</H1>'),
                HTML('<p>Select A Template Below Or Draw From Scratch </p>') ,
                actionButton("start", "Click Here To Draw From Scratch") ,
                HTML('</center>')
              ) ,
              
              #Templates available
              box(tags$h3(
                tags$a(href = "https://foocheung.shinyapps.io/figureone/?_inputs_&a1=%5B%221%22%2C%223%22%5D&a2=%5B%221%22%2C%223%22%2C%229%22%5D&a3=%5B%221%22%2C%222%22%2C%223%22%2C%224%22%2C%225%22%2C%226%22%2C%227%22%2C%228%22%2C%229%22%5D&a4=%5B%221%22%2C%222%22%2C%223%22%2C%224%22%2C%229%22%5D&a5=%5B%221%22%2C%222%22%2C%223%22%2C%224%22%2C%229%22%5D&a6=%5B%221%22%2C%222%22%2C%223%22%2C%224%22%2C%225%22%2C%226%22%2C%227%22%2C%228%22%2C%229%22%5D&add=6&adv=true&annt=true&auntps=9&b1=%22drug%22&b2=%22draw_arrow%22&b3=%22blood_sample%22&b4=%22point_15%22&b5=%22point_15%22&b6=%22point_15%22&bbb1=%221%22&bbb2=%221%22&bbb3=%222%22&bbb4=%223%22&bbb5=%224%22&bbb6=%225%22&c1=%22Drug%20Treatment%22&c2=%22%22&c3=%22Blood%20Sample%22&c4=%22Microarray%22&c5=%22Flow%20Cytometry%22&c6=%22CBC%22&captions=%22Add%20Your%20Caption%20Labels%20here%22&crop_plot=9&crop_plot_y=5.1&goButton3=28&h=6&help0=0&help2=0&lb=true&returnpdf=true&rev_col1=false&rev_col2=false&rev_col3=false&rev_col4=false&rev_col5=false&rev_col6=false&rotx=true&shp_col1=%22black%22&shp_col2=%22black%22&shp_col3=%22black%22&shp_col4=%22black%22&shp_col5=%22blue%22&shp_col6=%22green%22&shp_size1=1.5&shp_size2=2.5&shp_size3=1.5&shp_size4=1.5&shp_size5=1.5&shp_size6=1.5&shp_x1=0&shp_x2=0&shp_x3=0&shp_x4=0&shp_x5=0&shp_x6=0&sidebarCollapsed=false&sidebarItemExpanded=null&sp_lab=0&start=1&subtitle=%22Add%20Your%20Subtitle%20Labels%20here%22&tabs=%22Figure%22&timepts1=%22Day%200%22&timepts2=%22Day%201%22&timepts3=%22Day%202%22&timepts4=%22Day%203%22&timepts5=%22Day%204%22&timepts6=%22Day%205%22&timepts7=%22Day%206%22&timepts8=%22Day%207%22&timepts9=%22Day%208%22&title=%22Add%20Your%20Title%20Here%22&w=8&xlab=%22Add%20Your%20X-axis%20Labels%20here%22&ylab=%22Add%20Your%20Y-axis%20Labels%20here%22&start=1",target = "_blank",
                       "Template 1"))
                ,
                tags$iframe(style="height:270px; width:50%; scrolling:no; overflow:hidden",   src="./template1.pdf#zoom=50&view=FitH&toolbar=0")
                
              ),
              box(tags$h3(
                tags$a(href = "https://foocheung.shinyapps.io/figureone/?_inputs_&a1=%221%22&a10=%221%22&a11=%2210%22&a2=%5B%222%22%2C%225%22%2C%228%22%5D&a3=%5B%222%22%2C%225%22%2C%228%22%2C%2210%22%5D&a4=%5B%222%22%2C%223%22%2C%224%22%2C%225%22%2C%226%22%2C%227%22%2C%228%22%2C%229%22%2C%2210%22%5D&a5=%5B%222%22%2C%223%22%2C%224%22%2C%225%22%2C%226%22%2C%228%22%2C%229%22%2C%2210%22%5D&a6=%5B%222%22%2C%223%22%2C%224%22%2C%225%22%2C%226%22%2C%228%22%2C%229%22%2C%2210%22%5D&a7=%5B%222%22%2C%224%22%2C%225%22%2C%226%22%2C%228%22%2C%229%22%2C%2210%22%5D&a8=%5B%222%22%2C%225%22%2C%227%22%2C%2210%22%5D&a9=%221%22&add=11&adv=true&annt=true&auntps=11&b1=%22screening%22&b10=%22blank%22&b11=%22end%22&b2=%22syringe%22&b3=%22draw_arrow%22&b4=%22blood_sample%22&b5=%22point_15%22&b6=%22point_15%22&b7=%22point_15%22&b8=%22point_15%22&b9=%22blank%22&bbb1=%220.5%22&bbb10=%220.75%22&bbb11=%220.5%22&bbb2=%5B%220.25%22%2C%220.75%22%5D&bbb3=%5B%220.25%22%2C%220.75%22%5D&bbb4=%221.25%22&bbb5=%221.75%22&bbb6=%222.25%22&bbb7=%222.75%22&bbb8=%223.25%22&bbb9=%220.25%22&c1=%22%22&c10=%22Placebo%22&c11=%22%22&c2=%22%22&c3=%22%22&c4=%22Blood%20Sample%22&c5=%22Flow%20Cytometry%22&c6=%22RNA-Seq%22&c7=%22Complete%20Blood%20Count%22&c8=%22Neutralizing%20Titers%22&c9=%22HSV-529%22&captions=%22NIAID%20Protocol%20Number%3A%20%2013-I-0172%22&crop_plot=11&crop_plot_y=3.3&goButton3=22&h=6&help0=0&help2=0&lb=true&returnpdf=false&rev_col1=false&rev_col10=false&rev_col11=false&rev_col2=false&rev_col3=false&rev_col4=false&rev_col5=false&rev_col6=false&rev_col7=false&rev_col8=false&rev_col9=false&rotx=true&shp_col1=%22black%22&shp_col10=%22black%22&shp_col11=%22black%22&shp_col2=%22black%22&shp_col3=%22black%22&shp_col4=%22black%22&shp_col5=%22green%22&shp_col6=%22black%22&shp_col7=%22red%22&shp_col8=%22blue%22&shp_col9=%22black%22&shp_size1=1&shp_size10=1.5&shp_size11=1.5&shp_size2=1.5&shp_size3=2&shp_size4=1.5&shp_size5=1.5&shp_size6=1.5&shp_size7=1.5&shp_size8=1.5&shp_size9=1.5&shp_x1=0&shp_x10=0&shp_x11=0&shp_x2=0&shp_x3=0&shp_x4=0&shp_x5=0&shp_x6=0&shp_x7=0&shp_x8=0&shp_x9=0&sidebarCollapsed=false&sidebarItemExpanded=null&sp_lab=-1&start=1&subtitle=%22%22&tabs=%22Figure%22&timepts1=%22Screening%22&timepts10=%22Day360%22&timepts11=%22END%22&timepts2=%22Day0%22&timepts3=%22Day1%22&timepts4=%22Day7%22&timepts5=%22Day30%22&timepts6=%22Day37%22&timepts7=%22Day60%22&timepts8=%22Day180%22&timepts9=%22Day187%22&title=%22Phase%20I%20Study%20of%20the%20Safety%20of%20a%20Replication-Defective%20Herpes%20Simplex%20Virus-2%20Vaccine%2C%20HSV529%2C%20in%20Adults%20Aged%2018%20to%2040%20Years%20With%20or%20Without%20HSV%20Infection%22&w=8&xlab=%22TimePoints%22&ylab=%22Experimental%20Design%22",target = "_blank",
                       "Template 2"))
                ,
                tags$iframe(style="height:270px; width:50%; overflow:hidden",   src="./template2.pdf#zoom=50&view=FitH")
              ),
              box(tags$h3(
                tags$a(href = "https://foocheung.shinyapps.io/figureone/?_inputs_&a1=%221%22&a10=%5B%222%22%2C%225%22%2C%227%22%5D&a11=%5B%222%22%2C%225%22%2C%227%22%5D&a12=%5B%227%22%2C%2210%22%5D&a13=%5B%227%22%2C%2210%22%5D&a2=%5B%222%22%2C%225%22%5D&a3=%5B%222%22%2C%225%22%2C%227%22%2C%2210%22%5D&a4=%2210%22&a5=%227%22&a6=%5B%222%22%2C%223%22%2C%224%22%2C%225%22%2C%226%22%2C%227%22%2C%228%22%2C%229%22%2C%2210%22%5D&a7=%5B%222%22%2C%223%22%2C%224%22%2C%225%22%2C%226%22%2C%227%22%2C%228%22%2C%229%22%2C%2210%22%5D&a8=%5B%223%22%2C%226%22%2C%228%22%5D&a9=%5B%222%22%2C%223%22%2C%224%22%2C%225%22%2C%226%22%2C%227%22%2C%228%22%2C%229%22%2C%2210%22%5D&add=13&adv=true&annt=true&auntps=11&b1=%22screening%22&b10=%22highlight%22&b11=%22highlight%22&b12=%22highlight%22&b13=%22highlight%22&b2=%22syringe%22&b3=%22draw_arrow%22&b4=%22end%22&b5=%22syringe%22&b6=%22blood_sample%22&b7=%22point_15%22&b8=%22point_16%22&b9=%22point_14%22&bbb1=%220.5%22&bbb10=%220.25%22&bbb11=%220.75%22&bbb12=%220.25%22&bbb13=%220.75%22&bbb2=%5B%220.25%22%2C%220.75%22%5D&bbb3=%5B%220.25%22%2C%220.75%22%5D&bbb4=%220.5%22&bbb5=%5B%220.25%22%2C%220.75%22%5D&bbb6=%221.5%22&bbb7=%222%22&bbb8=%222.5%22&bbb9=%223%22&c1=%22Vaccination%22&c10=%22Add%20Label%20%2310%22&c11=%22Add%20Label%20%2311%22&c12=%22%22&c13=%22%22&c2=%22%22&c3=%22Add%20Label%20%233%22&c4=%22Add%20Label%20%234%22&c5=%22%22&c6=%22Blood%20Sample%22&c7=%22RNA-Seq%22&c8=%22Flow%20Cytometry%22&c9=%22Complete%20Blood%20Cell%22&captions=%22Add%20Your%20Caption%20Labels%20here%22&crop_plot=11&crop_plot_y=3.2&goButton3=34&h=6&help0=0&help2=0&lb=true&returnpdf=true&rev_col1=false&rev_col10=false&rev_col11=false&rev_col12=false&rev_col13=false&rev_col2=false&rev_col3=false&rev_col4=false&rev_col5=true&rev_col6=false&rev_col7=false&rev_col8=false&rev_col9=false&rotx=true&shp_col1=%22black%22&shp_col10=%22red%22&shp_col11=%22blue%22&shp_col12=%22blue%22&shp_col13=%22red%22&shp_col2=%22black%22&shp_col3=%22black%22&shp_col4=%22black%22&shp_col5=%22black%22&shp_col6=%22blue%22&shp_col7=%22black%22&shp_col8=%22green%22&shp_col9=%22blue%22&shp_size1=1.5&shp_size10=1&shp_size11=1&shp_size12=1&shp_size13=1&shp_size2=1.5&shp_size3=1.5&shp_size4=1.5&shp_size5=1.5&shp_size6=1.5&shp_size7=1.5&shp_size8=1.5&shp_size9=1.5&shp_x1=0&shp_x10=0&shp_x11=0&shp_x12=0&shp_x13=0&shp_x2=0&shp_x3=0&shp_x4=0&shp_x5=0&shp_x6=0&shp_x7=0&shp_x8=0&shp_x9=0&sidebarCollapsed=false&sidebarItemExpanded=null&sp_lab=0&start=1&subtitle=%22Add%20Your%20Subtitle%20Labels%20here%22&tabs=%22Figure%22&timepts1=%22Screening%22&timepts10=%22Day%208%22&timepts11=%22END%22&timepts2=%22Day%200%22&timepts3=%22Day%201%22&timepts4=%22Day%202%22&timepts5=%22Day%203%22&timepts6=%22Day%204%22&timepts7=%22Day%205%22&timepts8=%22Day%206%22&timepts9=%22Day%207%22&title=%22Add%20Your%20Title%20Here%22&w=8&xlab=%22Add%20Your%20X-axis%20Labels%20here%22&ylab=%22Add%20Your%20Y-axis%20Labels%20here%22",target = "_blank",
                       "Template 3"))
                ,
                tags$iframe(style="height:270px; width:50%; overflow:hidden",   src="./template3.pdf#zoom=50&view=FitH")
              )
              ,
              box(tags$h3(
                tags$a(href = "https://foocheung.shinyapps.io/figureone/?_inputs_&a1=%5B%221%22%2C%222%22%5D&a10=%5B%226%22%2C%2210%22%5D&a11=%226%22&a12=%5B%222%22%2C%226%22%5D&a13=%5B%222%22%2C%226%22%5D&a14=%5B%226%22%2C%2210%22%5D&a15=%5B%226%22%2C%2210%22%5D&a2=%222%22&a3=%5B%222%22%2C%225%22%5D&a4=%2210%22&a5=%5B%222%22%2C%223%22%2C%224%22%2C%225%22%2C%226%22%2C%227%22%2C%228%22%2C%229%22%2C%2210%22%5D&a6=%5B%222%22%2C%223%22%2C%224%22%2C%225%22%2C%226%22%2C%227%22%2C%228%22%2C%229%22%2C%2210%22%5D&a7=%5B%222%22%2C%223%22%2C%224%22%2C%225%22%2C%226%22%2C%227%22%2C%228%22%2C%229%22%2C%2210%22%5D&a8=%5B%222%22%2C%223%22%2C%224%22%2C%225%22%2C%226%22%2C%227%22%2C%228%22%2C%229%22%2C%2210%22%5D&a9=%225%22&add=15&adv=true&annt=true&auntps=13&b1=%22draw_arrow%22&b10=%22draw_arrow%22&b11=%22syringe%22&b12=%22highlight%22&b13=%22highlight%22&b14=%22highlight%22&b15=%22point_1%22&b2=%22point_17%22&b3=%22line%22&b4=%22end%22&b5=%22blood_sample2%22&b6=%22RNA_Seq%22&b7=%22blood_cells%22&b8=%22flow_cytometry%22&b9=%22screening%22&bbb1=%220.5%22&bbb10=%5B%220.25%22%2C%220.75%22%5D&bbb11=%5B%220.25%22%2C%220.75%22%5D&bbb12=%2220%22&bbb13=%2219.75%22&bbb14=%2220%22&bbb15=%2220%22&bbb2=%220.5%22&bbb3=%220.5%22&bbb4=%220.5%22&bbb5=%221%22&bbb6=%221.5%22&bbb7=%222%22&bbb8=%222.5%22&bbb9=%220.5%22&c1=%22Label%20%231%22&c10=%22Label%20%2310%22&c11=%22Label%20%2311%22&c12=%22Label%20%2312%22&c13=%22Label%20%2313%22&c14=%22Label%20%2314%22&c15=%22Label%20%2315%22&c2=%22Label%20%232%22&c3=%22Label%20%233%22&c4=%22Label%20%234%22&c5=%22Label%20%235%22&c6=%22Label%20%236%22&c7=%22Label%20%237%22&c8=%22Label%20%238%22&c9=%22Label%20%239%22&captions=%22Add%20Your%20Caption%20Labels%20here%22&crop_plot=11&crop_plot_y=2.7&goButton3=84&h=6&help0=0&help2=0&lb=true&returnpdf=true&rev_col1=false&rev_col10=false&rev_col11=false&rev_col12=false&rev_col13=true&rev_col14=false&rev_col15=false&rev_col2=false&rev_col3=false&rev_col4=false&rev_col5=false&rev_col6=false&rev_col7=false&rev_col8=false&rev_col9=false&rotx=true&rs=false&shp_col1=%22black%22&shp_col10=%22black%22&shp_col11=%22black%22&shp_col12=%22green%22&shp_col13=%22red%22&shp_col14=%22red%22&shp_col15=%22blue%22&shp_col2=%22green%22&shp_col3=%22black%22&shp_col4=%22black%22&shp_col5=%22black%22&shp_col6=%22black%22&shp_col7=%22black%22&shp_col8=%22black%22&shp_col9=%22black%22&shp_size1=2&shp_size10=1&shp_size11=1.5&shp_size12=1&shp_size13=1&shp_size14=1&shp_size15=1&shp_size2=1.5&shp_size3=5&shp_size4=2&shp_size5=1.5&shp_size6=1.5&shp_size7=1.5&shp_size8=1.5&shp_size9=1&shp_x1=0&shp_x10=1&shp_x11=0&shp_x12=0&shp_x13=0&shp_x14=0&shp_x15=0&shp_x2=0&shp_x3=0&shp_x4=0&shp_x5=0&shp_x6=0&shp_x7=0&shp_x8=0&shp_x9=0&sidebarCollapsed=false&sidebarItemExpanded=null&sp_lab=0&subtitle=%22Add%20Your%20Subtitle%20Labels%20here%22&tabs=%22Figure%22&timepts1=%22Screening%22&timepts10=%22Day360%22&timepts11=%22END%22&timepts12=%2212%22&timepts13=%2213%22&timepts2=%22Day0%22&timepts3=%22Day1%22&timepts4=%22Day7%22&timepts5=%22Day30%22&timepts6=%22Day37%22&timepts7=%22Day180%22&timepts8=%22Day187%22&timepts9=%22Day210%22&title=%22Add%20Your%20Title%20Here%22&w=8&xlab=%22Add%20Your%20X-axis%20Labels%20here%22&ylab=%22Add%20Your%20Y-axis%20Labels%20here%22&start=1",target = "_blank",
                       "Template 4"))
                ,
                tags$iframe(style="height:270px; width:50%; overflow:hidden",   src="./template4.pdf#zoom=50&view=FitH")
              ),
              
              
              column(12,
                     align="center",
                     actionButton("help0", "Press for instructions")
              )
            )
            
          ),
          
          
          tabItem(
            tabName = "Figure",
            
            fluidRow(
              
              
              column(
                12,
                offset = 3,
                
                 
                box(
                  
                  
                  uiOutput('web3')
                  
                ) 
                
                
              )
              
            ),
            
            fluidRow(
              introjsUI(),
              title = "",
              id = "tabset2",
              
              width = 12,
              
              align="center",
               ##Interactive help
              box(
                
                useShinyjs(),
                
                
                  width = 12,
                
                id="t2",
                id="t3",
                sliderInput(
                  "auntps", 
                  HTML(" <b> <i><h4><i class='glyphicon glyphicon-time fa-2x' <i class='glyphicon glyphicon-time fa-1x' style='color: #fff;  background-color: #FA9900; border-color: #2e6da'></i>  Select Number of TimePoints</h4>"),
                  min = 0, max = 25,
                  value = 10),
                actionButton("help2", "Press for instructions",
                             icon("info-sign", lib = "glyphicon"), 
                             style="color: #fff; background-color: #FA9900; border-color: #2e6da4") 
             
                
              ),                                                       
              
              
              # Add content to diagram
              column(
                
                width=12,
                offset=0.5,
                
                conditionalPanel(
                  condition =   "input.auntps > 0",  
                  align="left",
                  actionButton("add", "Add Item", icon("plus", lib = "glyphicon"), 
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                  
                )
                
              )
            )
            
          )
        )
      )
      )

    )
}



shinyApp(
#bookmark drawing
  ui,enableBookmarking = "url",
  server = function(input, output,session) {
    addClass(selector = "body", class = "sidebar-collapse")
    
    
    
    observeEvent(input$start, {
      updateTabItems(session, "tabs", "Figure")
    }
    )
    
    
    setBookmarkExclude("goButton4")
    
    
    
    observe({
      if (is.null(input$start) ||input$start == 0 ) {
        
      }
      else{
        removeClass(selector = "body", class = "sidebar-collapse")
        
      }
     
      session$doBookmark()  
      
    })
   
    #update url with the features of the drawing
    onBookmarked(function(url) {
      updateQueryString(url)
      
      
    })
    
    
    #Interactive Help
    observeEvent(input$help0, {
      introjs(session, options = list(steps = steps()))
    })
    
    steps <- reactive(data.frame(element=c("#a"), 
                                 intro =c("Select one of the \"Templates\" <BR> or <BR> Select \"Draw From Scratch\""),
                                 
                                 position = "bottom"))
    
    
    
    
    
    observeEvent(input$help2, {
      introjs(session, options = list(steps = steps2()))
    })
    
    
    steps2 <- reactive(data.frame(element=c("#auntps", "#add","#goButton3","#adv", '#crop_plot_y', '#crop_plot',"#rs", "#bm", "#returnpdf", "#lb",
                                            "#title","#captions","#subtitle","#xlab", "#ylab" ,
                                            "#annt","#rotx"
    ), 
    intro =c(
      "Use the slider to select the number of TimePoints required (default = 10)<BR>",
      "Press \"Add Items\" to start adding shapes and information<BR>
      <img src='additem.png' style='width: 50%; height:50%'>",
      "Press the 'Draw Figure' to update the figure<BR>
      <img src='draw.png' style='width: 100%; height:100%'>",
      "More Options<BR>
      <img src='moreoptions.png' style='width: 50%; height:50%'>",
      "crop Y<BR>
      <img src='cropy.png' style='width: 100%; height:100%'>",
      "crop X<BR>
      <img src='cropx.png' style='width: 100%; height:100%'>",
      "Restarts the Shiny session and resets values<BR>
      <img src='restart.png' style='width: 60%; height:60%'>",
      "BookMark<BR>
      <img src='bookmark.png' style='width: 100%; height:100%'>",
      "Download as PDF<BR>
      <img src='downloadpdf.png' style='width: 100%; height:100%'>",
      "Show Titles and Labels
      <img src='title.png' style='width: 30%; height:30%'>",
      "Title",
      "Caption",
      "Subtitle",
      "x-label",
      "y-label",
      "Add TimePoints",
      "Rotate TimePoint Labels"
      
      
      
    ),
    
    
    position = "bottom"))
    
    
    #Display how many timepoints bering used
    output$result1 <- renderUI({
      req(input$auntps)
     
      
      
      HTML(paste("<center><i> <i class='glyphicon glyphicon-time fa-1x' style='color: #fff;  background-color: #FA9900; border-color: #2e6da'></i> <b> ",
                 "<font color='red'>", input$auntps,"</font>",
                 'TimePoints Selected', 
                 sep=' '))
         
      
    })
    #Button to crop the drawing
    output$result1s <- renderUI({
      req(input$auntps)
      
      isolate(
        tagList(
          
          div(id="crop_plot",style="display:inline-block", sliderInput(inputId="crop_plot", label = "Crop X axis", min=1, max=input$auntps, step = 1,value=input$auntps, width=100, ticks=F)),
          div(id="crop_plot_y",style="display:inline-block", sliderInput( inputId="crop_plot_y", label = "Crop Y axis", min=1, max=18, step = 0.1, value=4 , width=100, ticks=F))    
        )
      )
    })
    
    
    
    
 #List of groups for drop down menu   
    
    drug_list<-c("drug")
    vacc_list<-c("syringe")
    blood_list<-c("blood_sample", "blood_sample2", "blood_sample3")
    sbj_list<-c("subjects_b","subjects_r", "subjects_rb", "subjects_br")
    serology_list<-c("HAI","microneutralization", "bcell_ellispot")
    transcriptomics_list<-c("chip","RNA_Seq")
    proteomics_list<-c("proteomics", "phage",  "heatmap" )
    cells_list<-c("flow_cytometry","flow_cytometry2","blood_cells" )
     misc_list<-c("blank","tick", "cross","insert_text")
    shapes_list<-c( "point_1","point_2","point_3","point_4","point_5","point_6","point_7","point_8","point_9","point_10","point_11","point_12","point_13","point_14","point_15","point_16", "point_17","point_24")
    tool_list<-c("line","draw_arrow", "draw_arrow2", "screening","crossover","end","highlight")
    
    
    
    
    
    observeEvent(input$txtbx,{
      if (input$file1 == "") return(NULL)
      session$sendCustomMessage (type="hide", "#ex")
      print(input$file1)
    })
    
    
    
    
    tgp<-reactive({
    
      myData <- dataM()$data
      tps<- as.data.frame(table(myData$TimePoint))
      tpg1<- tps %>% filter(Freq > 0) %>% arrange(desc(Freq) )
      colnames(tpg1)<-c("TimePoint", "Frequency")
      
      timepoints<-tpg1[1]
      
      return(list("timepoints"=timepoints, "mxtp"=mxtp))
    })
    
    
    ##Plot diagram
    output$web3 <- renderUI( {
      
      tagList(
        
        plotOutput('plot2'
           
        )
        
      )
      
    
      
    }
    )
    
    
    
    
    
    
    #Add items to drawing and offer coordinates
    
    addItem <- function(id) {
      
      id <- id  
      
      
      

      insertUI(
        
        selector = "#add",
        where ="beforeBegin",
        tagList(
          
          box(
            collapsible=FALSE,
            status="warning",
            offset=1,
            solidHeader=TRUE,
            title=paste0("#", id, sep=''),
            width=2,
            
            align = 'left',
            
            
            
            textInput( 
              
              
              paste0('c', id) , label=HTML("<i class='glyphicon glyphicon-pencil fa-1x' style='color: #fff;  background-color: #FA9900; border-color: #2e6da'></i>",  sep='') ,
              paste0('Add Label #', 
                     id, sep='')
              
            ),
            selectizeInput(paste0('a', id ) , label=HTML("<i class='glyphicon glyphicon-time fa-1x' style='color: #fff;  background-color: #FA9900; border-color: #2e6da'></i>"), choices =c(1:25),selected=c(tgp()$timepoint[1],tgp()$timepoint[2]),multiple = TRUE,width="200px"),
            div(style="display:inline-block;vertical-align:top",selectizeInput(
              paste0('b', id  ) , 
              label=HTML("<i class='glyphicon glyphicon-picture fa-1x' style='color: #fff;  background-color: #FA9900; border-color: #2e6da''></i>"),
              choices =  
                list(
                  "tools"=c(tool_list),
                  "syringe" =  c(vacc_list),
                  "Shapes"=c(shapes_list),
                  
                  "Subjects" = c(sbj_list),
                  "Samples" = c(blood_list),
                  "Drugs"=c(drug_list),
                  "serology" = c(serology_list),
                  "transcriptomics" = c(transcriptomics_list),
                  "proteomics" = c(proteomics_list), 
                  "cell" = c(cells_list),
                  "misc" = c(misc_list)
                  
                  
                ),width="125px",
              options = list(render = I(sprintf(
                "{
                option: function(item, escape) {
                return '<div><img width=\"40\" height=\"40\"  src=\"'   + escape(item.value)   + '.png\" />' + escape(item.value) + '</div>';
                }
    }"      ))
              )
              
            )),
            selectizeInput(
              
              paste0('bbb', id  ) , 
              "Row Position",
              choices =   c(2.5/10,3.3/10,5/10,6.6/10,7.5/10,10/10,12.5/10,15/10,17.5/10,20/10,22.5/10,25/10,27.5/10,30/10,
                            32.5/10,35/10,37.5/10,40/10,42.5/10,45/10,47.5/10, 50/10,
                            52.5/10, 55/10,57.5/10,60/10,62.5/10,65/10,67.5/10,70/10,
                            72.5/10,75/10,77.5/10,80/10,82.5/10,85/10,87.5/10,90/10,
                            92.5/10,95/10,97.5/10,100/10,
                            102.5/10,105/10,107.5/10,
                            110/10,112.5/10,115/10,117.5/10,120/10,122.5/10,125/10,127.5/10,130/10,
                            132.5/10,135/10,137.5/10,140/10,142.5/10,145/10,147.5/10,150/10,
                            152.5/10,155/10,157.5/10,160/10,162.5/10,165/10,167.5/10,170/10,172.5/10,
                            175/10,177.5/10,180/10,182.5/10,185/10,187.5/10,190/10,192.5/10,195/10,197.5/10,200/10
                            
              ),selected=id,multiple = TRUE) ,
            
            
            conditionalPanel(
              condition = "input.adv == true",
              sliderInput(inputId= paste0('shp_x', id),label = 'Nudge X:', min=0, max = 20, value=0),
              sliderInput(inputId= paste0('shp_size', id),label = 'size:', min=1, max = 5, value=1.5,step=0.5),
              selectizeInput(inputId= paste0('shp_col', id),label = 'color:', choices=c("red","blue", "black", "green","orange", "yellow", "white"), "black"),
              checkboxInput(inputId= paste0('rev_col',id), 'Reverse Colors', FALSE)
            )
            
            
            
            
            
            
            
            
          )
        )
        
      )
    }
    
    
    #Restore drawing from bookmark
    observeEvent(input$add, {
      
      addItem(input$add)        
    }
    , ignoreInit = TRUE)
    
    onRestore(function(state) {
      req(input$add)
      for (i in seq_len(input$add)) {
        addItem(i)
        
      }
    }
    )
    
    #label for Timepoints
    output$ann_tps<-renderUI({
      if (is.null(input$auntps)){
        
        return(NULL)    
      }
      req(input$auntps > 0)
      
      lapply(1:input$auntps, function(i) {
        
        tagList(
          
          textInput(
            inputId = paste0("timepts", i),
            
            label =HTML("<i class='glyphicon glyphicon-time fa-1x' style='color: #fff;  background-color: #FA9900; border-color: #2e6da'></i> Label For TimePoint", i , sep=''),
            
            paste0("Day ", i-1)
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
      
      
      library('foreach')
      req(input$gotp)
      
      i_squared <- integer(input$addtp)  
      
      
      for(i in 1:input$addtp ) {
        
        i_squared[i] <- eval(parse(text=paste0("input$tpt", i, "TimePoints", sep="")) )  
        
      }
      
      
      
      
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
      
      tps<-matrix(1:input$auntps)
      
      colnames(tps)<-c("TimePoint")
      
      timepoints<-tps
      mx<-input$auntps
      return(list("timepoints"=timepoints, "mx"=mx))
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
      
      
      tpg1<-time_p_g1()$tpg1
      tagList(
        box(
          
          
          selectInput(
            inputId = "fil_by_tp",
            label = "Filter By TimePoint",
            c(sort(as.numeric(unlist(t(tgp()$timepoint))))),
            selected = c(sort(as.numeric(unlist(t(tgp()$timepoint))))),
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
      
      myData <- cbind(myData, "SampleID"=1:nrow(myData) ) 
      rownames(myData)<-myData$SampleID
      
      
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
      
      
      #Plot diagram
      
      if (is.null(input$goButton3) |input$goButton3 == 0 |input$goButton3 == 'FALSE')
        return(NULL)
      
      
      isolate ({
        

        #set up canvas
        blank = sample("./www/blank.png")
        
      
        p <-
          ggplot(
            
            
            data=data.frame(x=as.factor(c(1:input$auntps)), y=(10) ), aes(x=x,y=y) 
          ) +
          
          
          theme_bw()+
          theme(
            
            
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            text = element_text(size = 13, face = "bold"),
            
            plot.title = element_text(size = 12, face = "bold"),
            
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y =element_blank(),
            
           
            
            axis.title.x = element_text(margin = margin(t = 20)) ,
            axis.text.x.top = element_text(vjust = 0.5)
            
          )  
        
      
        
        if (input$rotx  >0 ){
       
          p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0))
        }
        
        #Add captions,title,subcaptions
        
        p <-  p +     ggtitle(input$title2) +
          
          labs(title =wrapper(input$title, width = 90),
               
               subtitle = wrapper(input$subtitle, width = 100),
               caption = wrapper(input$captions, width = 100), 
               x = input$xlab, y = input$ylab) 
        
       
        
        
        #Shapes in www folder
        cross = sample("./www/cross.png")
        
        tick = sample("./www/tick.png")
        
        
        insert_text = sample("./www/text.png")
        point = sample("./www/dot.png")
        end = sample("./www/draw_arrow2.png")
        screening = sample("./www/screening.png")
        screening2 = sample("./www/screening.png")
        screening3 = sample("./www/screening.png")
        screening4 = sample("./www/screening.png")
        draw_arrow6 = sample("./www/draw_arrow2.png")
        dbl_arrow= sample("./www/draw_arrow2.png")
        syringe= sample("./www/syringe.png")
        drug= sample("./www/drug.png")
        draw_arrow2 = sample("./www/draw_arrow2.png")
        draw_arrow = sample("./www/draw_arrow.png")
        diag_arrowl= sample("./www/draw_arrow2.png")
        draw_arrow3 = sample("./www/draw_arrow2.png")
        highlight= sample("./www/red.png")
        draw_arrow4 = sample("./www/draw_arrow.png")
        crossover= sample("./www/crossover.png")
        blood_sample3 = sample("./www/blood_sample3.png")
        line= sample("./www/line.png")
        
        syringe_b = sample("./www/syringe_b.png")
        syringe_r = sample("./www/syringe_r.png")
        
        drug_b = sample("./www/drug_b.png")
        drug_r = sample("./www/drug_r.png")
        
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
        
        
        
        point_1= sample("./www/point_1.png")
        point_2= sample("./www/point_2.png")
        point_3= sample("./www/point_3.png")
        point_4= sample("./www/point_4.png")
        point_5= sample("./www/point_5.png")
        point_6= sample("./www/point_6.png")
        point_7= sample("./www/point_7.png")
        point_8= sample("./www/point_8.png")
        point_9= sample("./www/point_9.png")
        point_10= sample("./www/point_10.png")
        point_11= sample("./www/point_11.png")
        point_12= sample("./www/point_12.png")
        point_13= sample("./www/point_13.png")
        point_14= sample("./www/point_14.png")
        point_15= sample("./www/point_15.png")
        point_16= sample("./www/point_16.png")
        point_17= sample("./www/point_17.png")
        point_24= sample("./www/point_24.png")
        
      
        
        
        #parse input to create drawing
        library('foreach')
        
        
        pl <- vector("list", length =  input$add)
        
        p <- p +  geom_image(data=data.frame(x=as.factor(c(1:input$auntps)), y=(10)), aes(x=x,y=y), image=blank)
        
        if (input$add == 0){
          
         
        }
        else  if (input$add > 0){
          
          
          withProgress(message = 'Parsing Data', value = 0, {
            n<-input$add
           
            foreach (i=1:input$add, .combine=c) %do% { 
             
              
              
              
              rc<- paste0('input$rev_col', i,sep="")
              ee<-eval(parse(text = paste0('input$b', i,sep="")))
              
              tt<-paste0('input$a',i,sep="")
             
              ttt3<-paste0('input$bbb',i,sep="")
              
              cnt <- as.numeric(as.character(c(eval(parse(text=ttt3)))))  * 14
              ff<-paste0('input$c',i,sep="")
              ss<-paste0('input$shp_x', i, sep="") 
              si<- paste0('input$shp_size', i, sep="")
              sc<-  paste0('input$shp_col', i, sep="")
              cccc <-input$auntps
              
              
              
             
              
              if (is.null(ee))
                req(eval(as.name(ee)) > 0 )
              
              im<- eval(as.name(ee))
              
              
              
              
              if (ee == 'insert_text'){ 
                df<-  data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt )  
                foreach (i=1:length(df$x), .combine=c) %do% { 
                 
                  p <- p +     annotate("label", x=as.numeric(as.character(df$x[i])), y=cnt , label=eval(parse(text=ff)), color = eval(parse(text=sc)), fontface = "bold" )
                  
                }
                
              } 
              else if (ee == 'draw_arrow'){
                
                
                
                x<-factor(as.character(c(eval(parse(text=tt)))))
                y<-as.numeric(as.character(c(eval(parse(text=ttt3))))) * 14
                
                
                foreach (i=1:length(x), .combine=c) %do% { 
                  foreach (j=1:length(cnt), .combine=c) %do% { 
                    
                    p<- p + annotate(geom="segment",
                                     x=as.numeric(as.character(x[i]))+eval(parse(text=si))*1.5/30*2, 
                                     color=eval(parse(text=sc)), size=1,
                                     xend=as.numeric(as.character(x[i+1]))-eval(parse(text=si))*1.5/30*2,
                                     
                                     y=cnt,
                                     yend=cnt,arrow = arrow(length = unit(0.25, "cm")
                                     ) )
                    
                  }
                }
                
              }
              
              else if (ee == 'line'){
                
                
                
                x<-factor(as.character(c(eval(parse(text=tt)))))
                y<-as.numeric(as.character(c(eval(parse(text=ttt3))))) * 14
                
                
                foreach (i=1:length(x), .combine=c) %do% { 
                  foreach (j=1:length(cnt), .combine=c) %do% { 
                    
                    p<- p + annotate(geom="segment",
                                     x=as.numeric(as.character(x[i])) + eval(parse(text=si))/30, 
                                     color=eval(parse(text=sc)), size=1,
                                     xend=as.numeric(as.character(x[i+1])),    #-eval(parse(text=si))/eval(parse(text=si)),
                                     y=cnt,
                                     yend=cnt )
                    
                  }
                }
                
              }
              
              
              else if (ee == 'crossover'){
                
                
                
                x<-factor(as.character(c(eval(parse(text=tt)))))
                y<-as.numeric(as.character(c(eval(parse(text=ttt3))))) * 14
                
                
                foreach (i=1:length(x), .combine=c) %do% { 
                  foreach (j=1:length(cnt), .combine=c) %do% { 
                    p<- p + 
                      
                      annotate(geom="segment",x=as.numeric(as.character(x[i])) + eval(parse(text=si))/30*6 ,  color=eval(parse(text=sc)),
                               size=1,xend=as.numeric(as.character(x[i+1])) - eval(parse(text=si))/30*7  ,
                               y=cnt+3 * eval(parse(text=si)) ,yend=cnt-2.5 * eval(parse(text=si)) ,arrow = arrow(length = unit(0.25, "cm")
                                                                                                                  
                               ) 
                      ) +
                      
                      
                      annotate(geom="segment",x=as.numeric(as.character(x[i])) + eval(parse(text=si))/30*6  , color=eval(parse(text=sc)),
                               size=1,xend=as.numeric(as.character(x[i+1])) - eval(parse(text=si))/30*7 ,
                               y=cnt-3 * eval(parse(text=si)),yend=cnt+2.5 * eval(parse(text=si)),arrow = arrow(length = unit(0.25, "cm")
                                                                                                                
                               ) 
                      )
                    
                  }       
                }
              }
              
              
              
              else if (ee == 'draw_arrow4'){
                df<-  data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt )    
                foreach (i=1:length(df$x), .combine=c) %do% { 
                  p<- p + annotate(geom="segment",x=as.numeric(as.character(df$x[i])), color=eval(parse(text=sc)), size=1,xend=as.numeric(as.character(df$x[i]))+1,
                                   y=cnt,yend=cnt +eval(parse(text=si))*2,arrow = arrow(length = unit(0.25, "cm")
                                   ) )  
                }
              }
              
              
              else if (ee == 'end'){
                df<-  data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt )    
                foreach (i=1:length(df$x), .combine=c) %do% { 
                  p<- p + annotate(geom="segment",x=as.numeric(as.character(df$x[i]))+eval(parse(text=si))/10, color=eval(parse(text=sc)), size=1,xend=as.numeric(as.character(df$x[i]))+1,
                                   y=cnt+4,yend=cnt -2 ) +
                    annotate(geom="segment",x=as.numeric(as.character(df$x[i]))+eval(parse(text=si))/10, color=eval(parse(text=sc)), size=1,xend=as.numeric(as.character(df$x[i]))+1,
                             y=cnt-4,yend=cnt +2 )
                  
                }
              }
              
              else if (ee == 'end2'){
                # 2 lines coming together like arrow
                df<-  data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt )    
                foreach (i=1:length(df$x), .combine=c) %do% { 
                  p<- p + annotate(geom="segment",x=as.numeric(as.character(df$x[i]))+eval(parse(text=si))/10, color=eval(parse(text=sc)), size=1,xend=as.numeric(as.character(df$x[i]))+1,
                                   y=cnt+4,yend=cnt) +
                    annotate(geom="segment",x=as.numeric(as.character(df$x[i]))+eval(parse(text=si))/10, color=eval(parse(text=sc)), size=1,xend=as.numeric(as.character(df$x[i]))+1,
                             y=cnt-4,yend=cnt)
                  
                }
              }
              
              else if (ee == 'diag_arrowl'){
                df<-  data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt )    
                foreach (i=1:length(df$x), .combine=c) %do% { 
                  p<- p + 
                    annotate(geom="segment",x=as.numeric(as.character(df$x[i]))+eval(parse(text=ss))/10, color=eval(parse(text=sc)), size=1 , xend=as.numeric(as.character(df$x[i]))+0.7,
                             y=cnt,yend=cnt + eval(parse(text=si)) ,arrow = arrow(length = unit(0.25, "cm"))) 
                                   
                }
              }
              
              
              
              
              
              else if (ee == 'screening3'){
                df<-  data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt )    
                foreach (i=1:length(df$x), .combine=c) %do% { 
                  
                  
                  
               
                  
                  p<- p + 
                    
                    
    
                  
                  annotate(geom="segment",
                           x=as.numeric(as.character(df$x[i]))-0.2 , 
                           color=eval(parse(text=sc)), size=1 , 
                           xend=as.numeric(as.character(df$x[i])) -0.8 , 
                           y= cnt + 0.7 +eval(parse(text=ss))*2 ,
                           yend=cnt + eval(parse(text=si))*7 -1.6 ,
                           arrow = arrow(length = unit(0.25, "cm"))) +
                    
                    
                    annotate(geom="text", x=-2, y=cnt, label=eval(parse(text=ff)) ,color="black", parse=FALSE, fontface = "bold")
                }
              }
              
              
              else if (ee == 'screening4'){
                df<-  data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt )    
                foreach (i=1:length(df$x), .combine=c) %do% { 
                  p<- p + 
                    
                    annotate(geom="segment",
                             x=as.numeric(as.character(df$x[i])) +0.2, 
                             color=eval(parse(text=sc)), size=1 , 
                             xend=as.numeric(as.character(df$x[i]))+0.8,
                             y= cnt  + 0.7 +eval(parse(text=ss))*2,
                             yend=cnt  +eval(parse(text=si))*7 -1.7,##start forward
                             arrow = arrow(length = unit(0.25, "cm"))) +
                    
                    
                    
                    annotate(geom="segment",
                             x=as.numeric(as.character(df$x[i]))-0.2 , 
                             color=eval(parse(text=sc)), size=1 , 
                             xend=as.numeric(as.character(df$x[i])) -0.8 , 
                             y= cnt + 0.7 +eval(parse(text=ss))*2 ,
                             yend=cnt + eval(parse(text=si))*7 -1.6 ,
                             arrow = arrow(length = unit(0.25, "cm")))  +
                    
                    annotate(geom="text", x=0.1, y=cnt, label=eval(parse(text=ff)) ,color="black", parse=FALSE, fontface=2)
                }
              }
              else if (ee == 'screening'){
                df<-  data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt )    
                foreach (i=1:length(df$x), .combine=c) %do% { 
                  p<- p + 
                    annotate(geom="segment",
                             x=as.numeric(as.character(df$x[i])) + eval(parse(text=ss))/10,
                             color=eval(parse(text=sc)), size=1 ,
                             xend=as.numeric(as.character(df$x[i]))+0.7,
                             y=cnt,yend=cnt + eval(parse(text=si))*2 ,arrow = arrow(length = unit(0.25, "cm"))) +
                    annotate(geom="segment",
                             x=as.numeric(as.character(df$x[i])) + eval(parse(text=ss))/10,
                             color=eval(parse(text=sc)), size=1,
                             xend=as.numeric(as.character(df$x[i]))+0.7,
                             y=cnt,yend=cnt -eval(parse(text=si))*2,arrow = arrow(length = unit(0.25, "cm"))) +
                    annotate(geom="text", x=-2-input$sp_lab, y=cnt, label=eval(parse(text=ff)) ,color="black", parse=FALSE, fontface=2)
                  
                  
                  
                  
                }
              }
              else if (ee == 'screening2'){
                df<-  data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt )    
                foreach (i=1:length(df$x), .combine=c) %do% { 
                  p<- p + 
                    
                    
                    
                    annotate(geom="segment",
                             x=as.numeric(as.character(df$x[i])) +0.2, 
                             color=eval(parse(text=sc)), size=1 , 
                             xend=as.numeric(as.character(df$x[i]))+0.8,
                             y= cnt  + 0.7 +eval(parse(text=ss))*2,
                             yend=cnt  +eval(parse(text=si))*7 -1.7,##start forward
                             arrow = arrow(length = unit(0.25, "cm"))) +
                    
                    
                    
                    
                   
                  annotate(geom="text", x=0.1, y=cnt, label=eval(parse(text=ff)) ,color="black", parse=FALSE, fontface=2)
                  
                  
                }
              }
              
              
              
              else if (ee == 'syringe' ||  ee == 'drug' ){
                
                
                
                
                
                x<-factor(as.character(c(eval(parse(text=tt)))))
                y<-as.numeric(as.character(c(eval(parse(text=ttt3))))) * 14
                
                
                foreach (i=1:length(x), .combine=c) %do% { 
           
                  foreach (j=1:length(cnt), .combine=c) %do% { 
                    
                    
                    
                    if (eval(parse(text=rc)) == 0)
                    {
                      
                      if ( (j %% 2) == 0) {
                        
                        sim <- paste0( ee,'_b', sep='')
                      }
                      
                      else{
                        
                        sim <- paste0( ee,'_r', sep='')
                        
                      }
                    }
                    
                    else{
                      
                      if ( (j %% 2) == 0) {
                        
                        sim <- paste0( ee,'_r', sep='')
                      }
                      
                      else{
                        
                        sim <- paste0( ee,'_b', sep='')
                        
                      }  
                      
                      
                    }
                    
                   
                    p<- p + 
                      
                      geom_image(
                        data=data.frame(x=as.numeric(as.character(x[i])), 
                                        y=cnt[j]), aes(x=x,y=y) , image=eval(parse(text=sim)) ,size=eval(parse(text=si))/40) 
                    
                    
               
                    
                    
                  }
                  
                  
                }
                  p<- p+ annotate(geom="segment",x=1,xend=input$auntps,y=cnt,yend=cnt,color="black", alpha=0.05,linetype="dashed" ) +
                  annotate(geom="text", x=-2-input$sp_lab, y=cnt, label=eval(parse(text=ff)),color="black", parse=FALSE, fontface=2)
                
                
              }
              
              
              
              
              
              
              
              
              else if (ee == 'dbl_arrow'){
                
                x<-factor(as.character(c(eval(parse(text=tt)))))
                y<-as.numeric(as.character(c(eval(parse(text=ttt3))))) * 14
                
                
                foreach (i=1:length(x), .combine=c) %do% { 
                  foreach (j=1:length(cnt), .combine=c) %do% { 
                    
                    p<- p + 
                      annotate(geom="segment",
                             
                               x=as.numeric(as.character(x[i]))+eval(parse(text=si))/30*2, 
                               color=eval(parse(text=sc)), size=1,
                               xend=as.numeric(as.character(x[i+1]))-eval(parse(text=si))/30*2,
                               y=cnt,
                               yend=cnt+20,arrow = arrow(length = unit(0.25, "cm")
                               ) )  +
                      
                      
                      annotate(geom="segment",
                               
                               xend=as.numeric(as.character(x[i]))+eval(parse(text=si))/30*2, 
                               color=eval(parse(text=sc)), size=1,
                               x=as.numeric(as.character(x[i+1]))-eval(parse(text=si))/30*2,
                               y=cnt,
                               yend=cnt,arrow = arrow(length = unit(0.25, "cm")
                               ) )
                    
                    
                    
                    
                    
                    
                  }
                }
                
              }
              
              
              else if (ee == 'draw_arrow6'){
                df<-  data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt )    
                foreach (i=1:length(df$x), .combine=c) %do% { 
                  p<- p + annotate(geom="segment",x=as.numeric(as.character(df$x[i]))+0.8, color=eval(parse(text=sc)), size=1,xend=as.numeric(as.character(df$x[i])),
                                   y=cnt,yend=cnt +eval(parse(text=si))*2)  +
                    annotate(geom="segment",x=as.numeric(as.character(df$x[i]))+0.8, color=eval(parse(text=sc)), size=1,xend=as.numeric(as.character(df$x[i])),
                             y=cnt,yend=cnt - eval(parse(text=si))*2) 
                  
                  
                }
              }
              
              else if (grepl("^point_", ee)){
                col.lst <- c("blue" , "red", "green" , "bisque", "grey20", "grey90", "green2", "olivedrab2")
                
                shp<-gsub("^.*?_","",ee)
                
                x<-factor(as.character(c(eval(parse(text=tt)))))
                y<-as.numeric(as.character(c(eval(parse(text=ttt3))))) * 14
                
                
                foreach (i=1:length(x), .combine=c) %do% { 
                  foreach (j=1:length(cnt), .combine=c) %do% { 
                    
                
                    p<- p + 
                      annotate(geom="point",
                               x=as.numeric(as.character(x[i])), 
                               
                               color=eval(parse(text=sc)),
                               fill=eval(parse(text=sc)),
                               size=eval(parse(text=si))*2,
                               
                               y=cnt[j]  ,
                               
                               shape=as.numeric(shp)+1)  
                    
                  }
                  
                  
                }
                p<- p+ annotate(geom="segment",x=1,xend=input$auntps,y=cnt,yend=cnt,color="black", alpha=0.05,linetype="dashed" ) +
                  annotate(geom="text", x=-2-input$sp_lab, y=cnt, label=eval(parse(text=ff)),color="black", parse=FALSE, fontface=2)
                
                
              }
              
              
              else if (ee == 'highlight'){
                x<-factor(as.character(c(eval(parse(text=tt)))))
                y<-as.numeric(as.character(c(eval(parse(text=ttt3))))) * 14
                
                
                foreach (i=1:length(x), .combine=c) %do% { 
                  foreach (j=1:length(cnt), .combine=c) %do% {  
                    
                    
                    
                    p<- p + annotate(geom="rect",
                                     xmin=as.numeric(as.character(x[i])) - eval(parse(text=ss))/2,
                                     fill=eval(parse(text=sc)) ,
                                     # color=eval(parse(text=sc)), 
                                     size=1,
                                     alpha=0.1,
                                     xmax=as.numeric(as.character(x[i+1])) - eval(parse(text=ss))/2,
                                     ymin=cnt - eval(parse(text=si)) *3 ,ymax=cnt +eval(parse(text=si)) *3
                    ) 
                    
                    
                  }             
                }
                
                
              }
              
              
              
              else if (ee == 'draw_arrow3'){
                df<-  data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt )    
                
                foreach (i=1:length(df$x), .combine=c) %do% { 
                  p<- p + annotate(geom="segment",x=as.numeric(as.character(df$x[i])), color=eval(parse(text=sc)), size=1,xend=as.numeric(as.character(df$x[i]))+1,
                                   y=cnt,yend=cnt - eval(parse(text=si))*2,arrow = arrow(length = unit(0.25, "cm")
                                   ) )
                  
                }
              }
              
              else if (ee == 'draw_arrow2'){
                df<-  data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt )    
                
                
                foreach (i=1:length(df$x), .combine=c) %do% { 
                  p<- p + annotate(geom="segment",x=as.numeric(as.character(df$x[i])), color=eval(parse(text=sc)), size=1,xend=as.numeric(as.character(df$x[i])),
                                   y=cnt,yend=cnt +eval(parse(text=si))*2,arrow = arrow(length = unit(0.25, "cm")
                                                                                        
                                   ))
                }
              }
              
              
              
              
              
              
              
              
              
              
              
              else{
                
                # dddd<<-data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt )
                p <- p +geom_image(
                  data=data.frame(x=factor(as.character(c(eval(parse(text=tt))))), y=cnt ), aes(x=x,y=y) , image=im ,size=eval(parse(text=si))/40) +
                  # annotate(geom="segment",x=1,xend=input$auntps,y=cnt,yend=cnt,color="black", alpha=0.05,linetype="dashed") +
                  annotate(geom="segment",x=1,xend=25,y=cnt,yend=cnt,color="black", alpha=0.05,linetype="dashed") +
                  
                  annotate(geom="text", x=-2-input$sp_lab, y=cnt, label=eval(parse(text=ff)) ,color="black", parse=FALSE, fontface=2)
                
              }
              
              
              incProgress(1/n, detail = paste("Doing part", i))
              
              Sys.sleep(0.1)     
            }
            
            
            
            
            
          })
          
          
        }
        
        
        
        
        
        
        
        
        
        
        start_rec <- as.numeric(input$crop_plot_y) *  10 +20
        
        end_rec <- start_rec + start_rec * .25
        
        
        
        p<- p+   scale_y_reverse(limits=c(end_rec,0  ) )
        
       
        
        aval<-list()
        for(i in 1:input$auntps) {
          if (i == 'undefined'){
            return()
          }
          else{
            
            
            itp<- paste0("input$timepts", i, sep="")
            
            aval<-append(aval, eval(parse(text = itp)) ) 
            
          }
          
        }
        
        
        
              p <-p +  scale_x_discrete(labels=aval, position = "top")  +  coord_cartesian(xlim=c(-3.5-input$sp_lab,input$crop_plot,expand = FALSE), ylim=c(3, input$crop_plot_y*15))   
        
        
        withProgress(message = 'Printing plot', value = 0, {
          n <- 1      
          plot (p ,  width = 7, height = 6, units = 'in',res = 500)
          
        })
      })
      
      
      if(input$returnpdf){
        pdf("plot.pdf", width=as.numeric(input$w), height=as.numeric(input$h))
        print(p)
           
        dev.off()
      }
      
    })
    
    
    
    
    
    
    
    output$info <- renderText({
      
      
      xy_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("x=", (round(e$x, 1)*10)-1, " y=", round(e$y, 1), "\n")
      }
      xy_range_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
               " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
      }
      
      paste0(
        "click: ", xy_str(input$plot_click),
        "dblclick: ", xy_str(input$plot_dblclick),
        "hover: ", xy_str(input$plot_hover),
        "brush: ", xy_range_str(input$plot_brush)
      )
    })
    

    
    
    #download pdf file
    
    output$pdflink <- downloadHandler(
      filename <- "myplot.pdf",
      content <- function(file) {
        file.copy("plot.pdf", file)
      }
    )
    

    
    
    
    
    
  })
