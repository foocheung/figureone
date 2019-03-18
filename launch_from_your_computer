Instructions to deploy Figure One Web tool

Launch Figure One web tool directly from R and GitHub

Step 1: Install R and RStudio

Before running the app you will need to have R and RStudio installed (tested with R 3.5.1 and RStudio 1.1.463).
Please check CRAN (https://cran.r-project.org/) for the installation of R.
Please check https://www.rstudio.com/ for the installation of RStudio.

Step 2: Install the R Shiny package and other packages required by the web tool

Start an R session using RStudio and run these lines:

# Make sure the ollowing packages are installed (from CRAN or bioconductor)

library(Biobase)
library(rintrojs)
library(ggpubr)
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



Step 3: Start the app

Start an R session using RStudio and run these lines:

shiny::runGitHub("figureone", "foocheung")  
This command will download the code from GitHub to a temporary directory of your computer and then launch the figureone app in the web browser. Once the web browser was closed, the downloaded code would be deleted from your computer. Next time when you run this command in RStudio, it will download the source code from GitHub to a temporary directory again.



#############################################################################################


Deploy Figure one web tool on local or web Linux server

Step 1: Install R

Please check CRAN (https://cran.r-project.org/) for the installation of R.

Step 2: Install the R Shiny package and other packages (RAN or Bioconductor) required by Figure one web tool

# Make sure the ollowing packages are installed (from CRAN or bioconductor)

library(Biobase)
library(rintrojs)
library(ggpubr)
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


Step 3: Install Shiny-Server

Please check the following pages for the installation of shiny-server.
https://www.rstudio.com/products/shiny/download-server/
https://github.com/rstudio/shiny-server/wiki/Building-Shiny-Server-from-Source

Step 4: Upload files of Figureone web tool

Put the directory containing the code and data of figureone to /srv/shiny-server.

Step 5: Configure shiny server (/etc/shiny-server/shiny-server.conf)

# Define the user to spawn R Shiny processes
run_as shiny;

# Define a top-level server which will listen on a port
server {  
  # Use port 3838  
  listen 3838;  
  # Define the location available at the base URL  
  location /figureone {  
    # Directory containing the code and data of figureone  
    app_dir /srv/shiny-server/figureone;  
    # Directory to store the log files  
    log_dir /var/log/shiny-server;  
  }  
}  
Step 6: Change the owner of the figureone directory

$ chown -R shiny /srv/shiny-server/figureone  
Step 7: Start Shiny-Server

$ start shiny-server  
Now, the figureone app is available at http://IPAddressOfTheServer:3838/figureone/.
