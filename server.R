library(shiny)

shinyServer( function(input, output) {
    wg<<-reactive({if(as.numeric(input$wg)>0) as.numeric(input$wg) else 105})
    ag<<-reactive({if(as.numeric(input$ag)>0) as.numeric(input$ag) else 42})
    ed<<-reactive({switch(input$ed,
            "<High School"= 1,
            "High School Grad"=2,
            "Some College"=3, 
            "College Grad"=4, 
            "Advanced Degree"=5)
        })
    jc<<-reactive({switch(input$jc,
            "Industrial" =1,
            "Information" =2)
        })
    data<-matrix(c(rep(1,1000),rgamma(n = 1000, shape = 13.5, rate = .32),
                   sample(c(rep(1,89), rep(2,324), rep(3,217), rep(4,228), rep(5,142))),
                   sample(c(rep(1,515), rep(2, 485)))), nrow=1000, ncol=4)
    coef<-c(33.08, .5554, 12,4.5)
    ndata<-data%*%coef
    
    output$wg <- renderText(wg())
    output$ag <- renderText(ag())
    output$ed <- renderText({switch(ed(),
                    "1"="<High School",
                    "2"="High School Grad",
                    "3"="Some College", 
                    "4"="College Grad", 
                    "5"="Advanced Degree")
                }) 
    output$jc <- renderText({switch(jc(),
                    "1"="Industrial",
                    "2"="Information")
                })
    
    pwg <- reactive(49.58 + .5554*ag() + 12*(ed()-1) + 4.5*(jc()-1))
    
    output$pwg <- renderText({
        if (input$goButton == 0) "You have not pressed the predict button" 
        else if (input$goButton == 1) paste(pwg(), " thousand USD, with percentile ", mean(ndata>pwg())*100, "%", sep="")
        else pwg()    
    })

    output$msg <- renderText({
        if (input$goButton ==0) ""
        else if (pwg()>=wg()) "Keep working, and you will have a bright future!"
        else "Excellent! You are exceptional!"
    })
    
    output$hist <-renderPlot({
        if (input$goButton==0 | input$plotButton == 0) "You have not pressed the predict and plot button" 
        else {hist(ndata, xlab = "Annual Salary", main="Histogram of Salaries", col = "lightgreen", breaks = 20)
              lines(x=c(pwg(),pwg()), y=c(0,500), col = "red", lwd=5)
              lines(x=c(wg(),wg()), y=c(0,500), col = "blue", lwd=5)
              legend("topright", legend=c("predicted salary", "input salary"), col = c("red", "blue"), lty = 1, bty="n", lwd = 5)}
       # else "Quit processing"
        
    })
} )