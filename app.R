#=====TarotreadR App=====#

#Library Load-in.
library(shiny)
library(shinythemes)
library(shiny)
library(shinyjs)
library(shinyanimate)
library(tableHTML)



#Loading the cards/data into the environment.
source(here::here("scripts","tarot_process.R"))


#Defining the UI for the TarotreadR====
ui <- fluidPage(title ="TarotreadR",
 
#Using Darkly bootstrap theme as a base.   
    theme = bslib::bs_theme(bootswatch = "darkly"),
    
#Adding additional CSS onto darkly themed foundation. Parallax background won't display properly if CSS is sourced externally. Believe this is because I'm using the Darkly theme as a base. not sure.

         tags$head(HTML("<div style='position: absolute; overflow:hidden; width: 100%;'>
             <style>html {
                 height: 100%;
                 /* max-height: 100%; */
                     width: 100%;
                 background-image:		
                     url('Bckgd4.png'),
                 url('Bckgd3.png'),
                 url('Bckgd2.png'),
                 url('Bckgd1.png');
                 background-repeat: repeat-x 0 bottom;
                 background-position: 
                     0 100%,
                 0 100%,
                 0 100%,
                 0 100%,
                 0 0;
                 animation: 100s loop infinite linear;
             }
         @keyframes loop {
             100% {
                 background-position: 
                      60%,
                 -800px 95%,
                 500px 50%,
                 1000px 100%,
                 400px 0;
             }
         }
        #keywordsone1{
            text-align:center;
            border: 10px solid #000000;
            border-radius: 15px 15px 15px 15px;
            background-color:#1f1f1f;
            opacity: 0.95;
            margin: %50 auto;
        }
        
          #keywordstwo1{
            text-align:center;
            border: 10px solid #000000;
            border-radius: 15px 0px 0px 15px;
            background-color:#1f1f1f;
            opacity: 0.95;
          }
        
         #keywordstwo2{
            text-align:center;
            border: 10px solid #000000;
            border-radius: 0px 15px 15px 0px;
            background-color:#1f1f1f;
            opacity: 0.95;
         }
        
        #keywordsthree1{
            text-align:center;
            border: 10px solid #000000;
            border-radius: 0px 15px 15px 0px;
            background-color:#1f1f1f;
            opacity: 0.95;
        }
        
        #keywordsthree2{
            text-align:center;
            border: 10px solid #000000;
            border-radius: 0px 15px 15px 0px;
            background-color:#1f1f1f;
            opacity: 0.95;
        }
        
        #keywordsthree3{
            text-align:center;
            border: 10px solid #000000;
            border-radius: 0px 15px 15px 0px;
            background-color:#1f1f1f;
            opacity: 0.95;
        }
        
         </style>")),
    
    
#Addding shinyjs to toggle info divs to remain hidden until action button is pressed
    useShinyjs(),

#App title/header with custom font.    
    HTML("<center>"),
img(src = "tarotreadr.svg", width = "50%"),
HTML("</center>"),


tags$script(src = "mobile_detect.js"),
 

#Creation of the app's tabs.
    tabsetPanel(

#About panel==== 

        tabPanel(style = "background-color: #222; opacity: .90;", "About", 
                 HTML('&emsp;'),
                 "This Shiny app was created for the", 
                 HTML("<a href= 'https://blog.rstudio.com/2021/03/11/time-to-shiny/'>2021 Shiny Contest hosted by R Studio</a>"), 
                 "by Meghan Harris.",
                 HTML("<br><br>"),
                 HTML('&emsp;'),
                 "Tarot cards have existed for centuries. Although originally designed as a standard playing card game, tarot has involved into a practice of divination, self-help guidance, and general entertainment. That being said, this app should be used for entertainment purposes only. Enjoy! - More formatted info will go here when I feel like it."),

#One Card Draw panel====
        tabPanel("One Card Draw",     
                 
                 actionButton("button1","Draw One Card",icon("hand-sparkles"), 
                                                   style="color: #fff; background-color: #000000; border-color: #2e6da4"),
                 fixedRow(id = "row", withAnim(),
                          column(width = 12, align = "center", div(style ="display: inline-block; center-align;",id="imageone1",tags$img(imageOutput("imageone1", inline = TRUE))),
                 HTML("<br><br>"),
                 hidden(div(style = "display: inline-block; center-align; width: 15%;", id="kwone1",tableOutput("keywordsone1")))))),
 
#Two Card Draw panel====        
        tabPanel("Two Card Draw", 
                 actionButton("button2","Draw Two Cards",icon("hand-sparkles"), 
                 style="color: #fff; background-color: #000000; border-color: #2e6da4"),
                 fixedRow(id = "row", withAnim(),
                          column(width = 6, align = "right", div(style ="display: inline-block; right-align;",id="imagetwo1",tags$img(imageOutput("imagetwo1", inline = TRUE))), hidden(div(style = "display: inline-block; margin: 3%; width: 35% ", id="kwtwo1",tableOutput("keywordstwo1")))),
                          column(width = 6, align = "left",
                                 div(style ="display: inline-block; left-align;",id="imagetwo2",tags$img(imageOutput("imagetwo2", inline = TRUE))),
                 hidden(div(style = "display: inline-block; margin: 3%; width: 35% ", id="kwtwo2",tableOutput("keywordstwo2")))))),

#Three Card Draw panel====  
         tabPanel("Three Card Draw", 
                  actionButton("button3","Draw Three Cards",
                               icon("hand-sparkles"), 
                               style="color: #fff; background-color: #000000; border-color: #2e6da4"),
                  fixedRow(id = "row", withAnim(),
                           column(width = 4, 
                                  align = "right", 
                                  div(style ="display: inline-block; center-align;",
                                      id="imagethree1",
                                      tags$img(imageOutput("imagethree1", 
                                                           inline = TRUE))),
                                  hidden(div(style = "display: inline-block; margin: 3%; width: 55% ", 
                                             id="kwthree1",tableOutput("keywordsthree1")))),
                           column(width = 4, align = "center",
                                  div(style ="display: inline-block; center-align;",
                                      id="imagethree2",
                                      tags$img(imageOutput("imagethree2", 
                                                           inline = TRUE))),
                                  hidden(div(style = "display: inline-block; margin: 3%; width: 55% ",
                                             id="kwthree2",tableOutput("keywordsthree2")))),
                           column(width = 4, align = "left",
                                  div(style ="display: inline-block; center-align;",
                                      id="imagethree3",
                                      tags$img(imageOutput("imagethree3", 
                                                           inline = TRUE))),
                                  hidden(div(style = "display: inline-block; margin: 3%; width: 55% ",
                                      id="kwthree3",tableOutput("keywordsthree3"))))))))

#Defining the server logic to construct card/info randomizer==== 
server <- function(input, output, session) {
    
#One Card Draw Server Logic====
    #Defining reactivity for "button 2" = "two card draw". 
    observeEvent(input$button1, {
        
        #Setting a randomized seed number for card pulls.
        seednum <- runif(1,0,10000)
        set.seed(seednum)
        
        #Randomly selecting one card from the deck. Pulling it's relative file path... 
        onecardfile <- unlist(lapply(sample(testtarot$Card,1), function(x) testtarot$Path[testtarot$Card == x]))
        onecardfile1 <- onecardfile[1]
        
        #...and title for alt text.
        onecardalt1 <- testtarot$Card[testtarot$Path == onecardfile1]
        
        #Staging the first image for rendering.
        output$imageone1 <- renderImage({
            # Return a list
            list(src = onecardfile1, 
                 contentType = "image/png",
                 width = "20%",
                 align = "center",
                 alt = onecardalt1)}, deleteFile = FALSE)
        
        #Staging the keywords for the first card for rendering.
        output$keywordsone1 <- renderTable(testtarot %>% filter(Path == onecardfile1) %>% select(Keywords), width = "100%", align ="c", sanitize.text.function=identity, bordered = FALSE)
        
    })

    
    
#Two Card Draw Server Logic====
#Defining reactivity for "button 2" = "two card draw". 
    observeEvent(input$button2, {

#Setting a randomized seed number for card pulls.
        seednum <- runif(1,0,10000)
        set.seed(seednum)

#Randomly selecting two cards from the deck and their positions... 
        twocardpositions <- sample(reversalset,2, replace = TRUE)
        twocards <- sample(mastercardset,2, replace = FALSE)
        
#Combining the cards and positions.
        for (i in seq_along(twocards)){
            twocards[i] <- str_trim(paste(twocards[i],twocardpositions[i]))
        }

#Pulling their relative file paths
        twocardfile <- unique(unlist(lapply(twocards, function(x) testtarot$Path[testtarot$Card == x])))
        twocardfile1 <- twocardfile[1]
        twocardfile2 <- twocardfile[2]

#...and titles for alt text.
        twocardalt1 <- testtarot$Card[testtarot$Path == twocardfile1]
        twocardalt2 <- testtarot$Card[testtarot$Path == twocardfile2]
        
#Staging the first image for rendering.
        output$imagetwo1 <- renderImage({
            # Return a list
            list(src = twocardfile1, 
                 contentType = "image/png",
                 width = "40%",
                 align = "center",
                 alt = twocardalt1)}, deleteFile = FALSE)

#Staging the second image for rendering.   
        output$imagetwo2 <- renderImage({
            # Return a list
            list(src = twocardfile2, 
                 contentType = "image/png",
                 width = "40%",
                 align = "center",
                 alt = twocardalt2)}, deleteFile = FALSE)
        
#Staging the keywords for the first card for rendering.
        output$keywordstwo1 <- renderTable(testtarot %>% filter(Path == twocardfile1) %>% select(Keywords), width = "100%", align ="c", sanitize.text.function=identity, bordered = FALSE)
        
#Staging the keywords for the second card for rendering.       
        output$keywordstwo2 <- renderTable(testtarot %>% filter(Path == twocardfile2) %>% select(Keywords), width = "100%", align ="c", sanitize.text.function=identity, bordered = FALSE)
    })
    
#Three Card Draw Server Logic====
    #Defining reactivity for "button 3" = "three card draw". 
    observeEvent(input$button3, {
        
#Setting a randomized seed number for card pulls.
        seednum <- runif(1,0,10000)
        set.seed(seednum)
        
#Randomly selecting two cards from the deck and their positions... 
        threecardpositions <- sample(reversalset,3, replace = TRUE)
        threecards <- sample(mastercardset,3, replace = FALSE)
        
#Combining the cards and positions.
        for (i in seq_along(threecards)){
            threecards[i] <- str_trim(paste(threecards[i],threecardpositions[i]))
        }
        
#Pulling their relative file paths
        threecardfile <- unique(unlist(lapply(threecards, function(x) testtarot$Path[testtarot$Card == x])))
        threecardfile1 <- threecardfile[1]
        threecardfile2 <- threecardfile[2]
        threecardfile3 <- threecardfile[3]
        
#...and titles for alt text.
        threecardalt1 <- testtarot$Card[testtarot$Path == threecardfile1]
        threecardalt2 <- testtarot$Card[testtarot$Path == threecardfile2]
        threecardalt3 <- testtarot$Card[testtarot$Path == threecardfile3]
        
#Staging the first image for rendering.
        output$imagethree1 <- renderImage({
            # Return a list
            list(src = threecardfile1, 
                 contentType = "image/png",
                 width = "60%",
                 align = "center",
                 alt = threecardalt1)}, deleteFile = FALSE)
        
#Staging the second image for rendering.   
        output$imagethree2 <- renderImage({
            # Return a list
            list(src = threecardfile2, 
                 contentType = "image/png",
                 width = "60%",
                 align = "center",
                 alt = threecardalt2)}, deleteFile = FALSE)
        
#Staging the third image for rendering.
        output$imagethree3 <- renderImage({
            # Return a list
            list(src = threecardfile3, 
                 contentType = "image/png",
                 width = "60%",
                 align = "center",
                 alt = threecardalt3)}, deleteFile = FALSE)
        
        
        
#Staging the keywords for the first card for rendering.
        output$keywordsthree1 <- renderTable(testtarot %>% filter(Path == threecardfile1) %>% select(Keywords), width = "100%", align ="c", sanitize.text.function=identity, bordered = FALSE)
        
#Staging the keywords for the second card for rendering.       
        output$keywordsthree2 <- renderTable(testtarot %>% filter(Path == threecardfile2) %>% select(Keywords), width = "100%", align ="c", sanitize.text.function=identity, bordered = FALSE)
    
#Staging the keywords for the third card for rendering.       
    output$keywordsthree3 <- renderTable(testtarot %>% filter(Path == threecardfile3) %>% select(Keywords), width = "100%", align ="c", sanitize.text.function=identity, bordered = FALSE)
})
    

#Adding the toggle for the keyword divs====
#This allows the information to only show AFTER the action button is clicked.
    observe({
        toggle(id = "kwone1", condition = (input$button1 > 0))
        toggle(id = "kwtwo1", condition = (input$button2 > 0))
        toggle(id = "kwtwo2", condition = (input$button2 > 0))
        toggle(id = "kwthree1", condition = (input$button3 > 0))
        toggle(id = "kwthree2", condition = (input$button3 > 0))
        toggle(id = "kwthree3", condition = (input$button3 > 0))
    })
    
# Card/info animations====
#One Card Pull===
    observeEvent(input$button1, {startAnim(session, "imageone1", "fadeInDown")
        startAnim(session, "kwone1", "flipInX")
        delay(500, insertUI(selector = "#button1",
                            where = "afterEnd",
                            ui = tags$audio(src = "fairyglitter.wav", type = "audio/wav", autoplay = F, controls = NA, style="display:none;")
        ))})
    
#Two Card Pull===
    observeEvent(input$button2, {startAnim(session, "imagetwo1", "fadeInLeft")
        startAnim(session, "imagetwo2", "fadeInRight")
        startAnim(session, "kwtwo1", "flipInX")
        startAnim(session, "kwtwo2", "flipInX")
        delay(500, insertUI(selector = "#button1",
                            where = "afterEnd",
                            ui = tags$audio(src = "fairyglitter.wav", type = "audio/wav", autoplay = F, controls = NA, style="display:none;")
        ))})
    
#Three Card Pull===
    observeEvent(input$button3, {startAnim(session, "imagethree1", "fadeInLeft")
        startAnim(session, "imagethree2", "fadeInDown")
        startAnim(session, "imagethree3", "fadeInRight")
        startAnim(session, "kwthree1", "flipInX")
        startAnim(session, "kwthree2", "flipInX")
        startAnim(session, "kwthree3", "flipInX")
        delay(500, insertUI(selector = "#button1",
                            where = "afterEnd",
                            ui = tags$audio(src = "fairyglitter.wav", type = "audio/wav", autoplay = F, controls = NA, style="display:none;")
        ))})
}

shinyApp(ui = ui, server = server)
