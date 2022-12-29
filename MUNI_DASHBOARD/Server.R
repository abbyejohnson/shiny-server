SERVER <- function(input, output, session) {

    #us map
    output$plot1<-renderPlot({
      width<-session$cliendData$output_plot1_width
      height<-session$clientData$output_plot1_height
      states<-map_data("state")
      #northeast map
      ne<-which(states$region %in% c("maine","new hampshire","vermont","massachusetts","rhode island","connecticut","new york","pennsylvania","new jersey"))
      ne<-states[ne,]
      #midwest map
      mw<-which(states$region %in% c("north dakota","south dakota","nebraska","kansas","missouri","iowa","minnesota","wisconsin","michigan","ohio","indiana","illinois"))
      mw<-states[mw,]
      #south map
      s<-which(states$region %in% c("texas","oklahoma","arkansas","louisiana","mississippi","alabama","georgia","florida","tennessee","kentucky","south carolina","north carolina","virginia","west virginia","delaware","maryland","district of columbia"))
      s<-states[s,]
      #west map
      w<-which(states$region %in% c("new mexico","colorado","wyoming","montana","idaho","utah","arizona","nevada","california","oregon","washington"))
      w<-states[w,]
      #us regional map
      ggplot(data=ne)+
        geom_polygon(aes(x=long,y=lat,group=group),fill="blue",color="black")+
        geom_polygon(data=mw,aes(x=long,y=lat,group=group),fill="red",color="black")+
        geom_polygon(data=s,aes(x=long,y=lat,group=group),fill="purple",color="black")+
        geom_polygon(data=w,aes(x=long,y=lat,group=group),fill="green",color="black")+
        labs(subtitle=CMSTitle)+
        annotate("text",x=-95,y=42.5,label="Midwest",size=12,angle=27)+
        annotate("text",x=-88,y=34,label="South",size=12,angle=27)+
        annotate("text",x=-74,y=43.5,label="Northeast",size=12,angle=27)+
        annotate("text",x=-113,y=40,label="West",size=12,angle=27)+
        annotate("text",x=-115,y=27,label=regInfo,size=5,color="red")+
        theme_bw()+
        theme(panel.background=element_rect(fill="cyan",color="cyan"),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank())
    })
    #update city based on entered zip code
    observe({
      req(input$ZIP)
      x<-getCities(input$ZIP)
      updateSelectInput(session,"city","",choices=x)
    })
    #update population for the selected city
    observe({
      if(input$city=="Other"){
        req(input$tCity)
        areapop<-getpop(input$ZIP,input$tCity)
        updateNumericInput(session,"pop","",
                           value=unique(areapop),min=1,max=9000000,step=1)
      }
      else{
        areapop<-getpop(input$ZIP,input$city)
        updateNumericInput(session,"pop","",
                           value=unique(areapop),min=1,max=9000000,step=1)
      }
    })
    #update region based on location provided
    observe({
      req(input$ZIP)
      x<-getState(input$ZIP)
      if(!is_empty(x)){
        updateSelectInput(session,"region",
                          "",
                          c("",ner,mwr,sr,wr),
                          selected=(if(x %in% c("ME","NH","vT","NY","MA","RI","CT","NJ","PA")){ner}
                                    else if(x %in% c("OH","IN","IL","MI","WI","MN","IA","MO","NE","KS","ND","SD")){mwr}
                                    else if(x %in% c("DE","MD","DC","WV","VA","TN","SC","NC","FL","AL","AR","LA","KY","OK","TX","GA")){sr}
                                    else if(x %in% c("NM","AZ","CO","WY","MT","ID","UT","NV","WA","OR","CA","HI","AK")){wr}
                                    else{""}))}
      else{updateSelectInput(session,"region",
                             "",
                             c("",ner,mwr,sr,wr))}
    })
    
    # create the page 2 datatable
    dtWithRadioButton<-reactiveValues(dt = m)
    
    observeEvent(input$btnContinue,{
      showModal(modalDialog(
        title=paste(T1_14,"Please answer each question"),
        size="l",easyClose=FALSE, fade=TRUE,
        DTOutput("datatable"),
        footer=tagList(
          actionBttn(inputId="btnProcess",label=submit,style="float",size="md",color="success")
        )
      ))
    })
    #creat the table for Sec 1 Q 14
    output$datatable = renderDT(
      datatable(dtWithRadioButton$dt, selection = "none", escape=FALSE,
                options= list(
                  dom = 't',
                  paging = FALSE,
                  ordering = FALSE
                ),
                callback = JS(
                  "table.rows().every(function(i, tab, row) {
                     var $this = $(this.node());
                     $this.attr('id', this.data()[0]);
                     $this.addClass('shiny-input-radiogroup');
});
                     Shiny.unbindAll(table.table().node());
                     Shiny.bindAll(table.table().node());"),
                rownames = TRUE), 
      server = FALSE
    )
    #display table on button click
    observeEvent(input$btnProcess,{
      dt<-dtWithRadioButton$dt
      x<-rep(0,7)
      i=1
      for(resp in text){
        req(input[[resp]])
        x[i]<-input[[resp]]
        i=i+1
      }
      tableScores<<-x
      newtab <- switch(input$survey, "Statements About Community" = "Community Framework","Community Framework" = "Statements About Community")
      updateTabItems(session, "survey", newtab) 
      removeModal(session)
    })
    
    #allow user to type in city
    output$textCity<-renderUI({
      req(input$city)
      if(input$city=="Other"){
        textInput("tCity","2a. Please type in your city",width=responseWidth)
      }
    })
    #text for % unmet need S2 Q10
    output$unmetNeedText<-renderText({
      req(input$Q2_10CurrNeeds)
      if(input$Q2_10CurrNeeds==n){
        paste("<b>",T2_10Need,"</b>")
      }
    })
    #ask what % of need is not met for S2 Q10 if they say need is not met
    output$unmetNeed<-renderUI({
      req(input$Q2_10CurrNeeds)
      if(input$Q2_10CurrNeeds==n){
        numericInput("Q2_10Need","",value=character(0),step=1,width=responseWidth)
      }
    })
    #ask what other for sec 3 q 1
    output$otherOrg<-renderUI({
      if(input$Q3_1Other){
        textInput("other31",ask31Other,width=responseWidth)
      }
    })
    #text for when muni ord was last written/updated
    output$muniOrdUpdtText<-renderText({
      req(input$Q3_5MuniOrd)
      if(input$Q3_5MuniOrd==y){
        paste("<b>",T3_5YrUpdt,"</b>")
      }
    })
    #ask when their muni ord was last written/updated
    output$muniOrdUpdt<-renderUI({
      req(input$Q3_5MuniOrd)
      if(input$Q3_5MuniOrd==y){
        numericInput("Q3_5YrUpdt","",value=character(0),step=1,width=responseWidth)
      }
    })
    #text for muni ord
    output$muniOrdText<-renderText({
      req(input$Q3_5MuniOrd)
      if(input$Q3_5MuniOrd==y){
        paste("<b>",T3_6,"</b>","<em>",checkAll,"</em>")
      }
    })
    #ask what muni ordinence they have about planting
    output$muniOrd1<-renderUI({
      req(input$Q3_5MuniOrd)
      if(input$Q3_5MuniOrd==y){
        checkboxGroupInput("Q3_61",
                           T3_61,
                           c(T3_6RegTrPub,T3_6RegTrPri,T3_6ReqTrND,T3_6ReqTrNP,T3_6ReqTrRP,T3_6ReqRpPOT),
                           selected=character(0),width=textWidth)
      }
    })
    #ask what muni ordinence they have about
    output$muniOrd2<-renderUI({
      req(input$Q3_5MuniOrd)
      if(input$Q3_5MuniOrd==y){
        checkboxGroupInput("Q3_62",
                           T3_62,
                           c(T3_6ReqPreDev,T3_6ResTrCtPP,T3_6RegAbat,T3_6IdPreHer),
                           selected=character(0),width=textWidth)
      }
    })
    #ask what muni ordinence they have about
    output$muniOrd3<-renderUI({
      req(input$Q3_5MuniOrd)
      if(input$Q3_5MuniOrd==y){
        checkboxGroupInput("Q3_63",
                           T3_63,
                           c(T3_6RegPubTrMn,T3_6ReqTyMn,T3_6RegRemDd,T3_6EstIDCS,T3_6DefTrReq,T3_6PhrTrTop),
                           selected=character(0),width=textWidth)
      }
    })
    #text for what year written plan was last updated
    output$writtenPlanUpdtText<-renderText({
      req(input$Q3_9WrtStgPln)
      if(input$Q3_9WrtStgPln==y){
        paste("<b>",T3_9UpdtText,"</b>")
      }
    })
    #ask what year the written plan was last updated
    output$writtenPlanUpdt<-renderUI({
      req(input$Q3_9WrtStgPln)
      if(input$Q3_9WrtStgPln==y){
        numericInput("Q3_9Updt","",value=character(0),step=1,width=responseWidth)
      }
    })
    #text for what the written plan entails
    output$writtenPlanText<-renderText({
      req(input$Q3_9WrtStgPln)
      if(input$Q3_9WrtStgPln==y){
        paste("<b>",T3_10,"</b>","<em>",checkAll,"</em>")
      }
    })
    #ask what written plan entails
    output$writtenPlan<-renderUI({
      req(input$Q3_9WrtStgPln)
      if(input$Q3_9WrtStgPln==y){
        checkboxGroupInput("Q3_10",
                           "",
                           c(T3_10CityMas,T3_10InsDsRead,T3_10MunWtr,T3_10StrEmer,T3_10StrWtrM,T3_10TrRskM,T3_10UrbForMgmt,T3_10UrbForStr,T3_10Other),
                           selected=character(0),width=textWidth)
      }
    })
    # allow user to specify other plan
    output$otherMngmt<-renderUI({
      if(input$Q3_10Other){
        textInput("other310",ask310Other,width=responseWidth)
      }
    })
    #text for year tree inventory was last updated
    output$trInvUpdtText<-renderText({
      req(input$Q6_1TrInv)
      if(input$Q6_1TrInv==y){
        paste("<b>",T6_3YrUpd,"</b>")
      }
    })
    #get what year the tree inventory was last updated
    output$trInvUpdt<-renderUI({
      req(input$Q6_1TrInv)
      if(input$Q6_1TrInv==y){
        numericInput("Q6_3YrUpd","",value=character(0),step=1,width=responseWidth)
      }
    })
    
    #text for tree inventory inclusion
    output$trInvText<-renderText({
      req(input$Q6_1TrInv)
      if(input$Q6_1TrInv==y){
        paste("<b>",T6_6,"</b>","<em>",checkAll,"</em>")
      }
    })
    #ask what the tree inventory includes
    output$trInvIncl<-renderUI({
      req(input$Q6_1TrInv)
      if(input$Q6_1TrInv==y){
        checkboxGroupInput("Q6_6",
                           "",
                           c(T6_6StrTr,T6_6PrkTr,T6_6MunGr,T6_6MunWd,T6_6PriTr,T6_6Other),
                           selected=character(0),width=textWidth)
      }
    })
    #text for tree inventory collection method
    output$invCollText<-renderText({
      req(input$Q6_1TrInv)
      if(input$Q6_1TrInv==y){
        paste("<b>",T6_7,"</b>","<em>",checkAll,"</em>")
      }
    })
    #ask what the tree inventory collection method is
    output$invColl<-renderUI({
      req(input$Q6_1TrInv)
      if(input$Q6_1TrInv==y){
        checkboxGroupInput("Q6_7",
                           "",
                           c(T6_7WndsSr,T6_7SmpSr,T6_7Census,T6_7RmSens,T6_7CanCo,T6_7iTrStr,T6_7iTrEco,T6_8GPSGIS,T6_7TreDm,T6_7TrePlLoc,T6_7SelTrS),
                           selected=character(0),width=textWidth)
      }
    })
    #text for canopy goal
    output$canGoalText<-renderText({
      req(input$Q6_15CanGl)
      if(input$Q6_15CanGl==y){
        paste("<b>",T6_16CanGl,"</b>")
      }
    })
    #ask what the canopy goal % is
    output$canGoal<-renderUI({
      req(input$Q6_15CanGl)
      if(input$Q6_15CanGl==y){
        numericInput("Q6_16CanGl","",value=character(0),step=1,width=responseWidth)
      }
    })
    #text for what the current canopy is
    output$canCurrText<-renderText({
      req(input$Q6_15CanGl)
      if(input$Q6_15CanGl==y){
        paste("<b>",T6_16CurCn,"</b>")
      }
    })
    #ask what the current canopy % is
    output$canCurr<-renderUI({
      req(input$Q6_15CanGl)
      if(input$Q6_15CanGl==y){
        numericInput("Q6_16CurCn","",value=character(0),step=1,width=responseWidth)
      }
    })
    #text for how many years to accomplish the canopy goal
    output$yearsToGlText<-renderText({
      req(input$Q6_15CanGl)
      if(input$Q6_15CanGl==y){
        paste("<b>",T6_16YrToGl,"</b>")
      }
    })
    #ask how many years they have to accomplish the goal
    output$yearsToGl<-renderUI({
      req(input$Q6_15CanGl)
      if(input$Q6_15CanGl==y){
        numericInput("Q6_16YrToGl","",value=character(0),step=1,width=responseWidth)
      }
    })
    #print the statement for regular pruning
    output$currPrunStatement<-renderText({
      req(input$Q7_4AppPrun)
      if(input$Q7_4AppPrun==T7_4RegPrun){
        paste("<b>",T7_4CurCyc,"</b>")
      }
    })
    #get the current pruning cycle
    output$currPrun<-renderUI({
      req(input$Q7_4AppPrun)
      if(input$Q7_4AppPrun==T7_4RegPrun){
        numericInput("Q7_4CurCyc","",value=character(0),step=1,width=responseWidth)
      }
    })
    #print the statement for desired pruning cycle
    output$desPrunStatement<-renderText({
      req(input$Q7_4AppPrun)
      if(input$Q7_4AppPrun==T7_4RegPrun){
        paste("<b>",T7_4DesCyc,"</b>")
      }
    })
    #get the desired pruning cycle
    output$desPrun<-renderUI({
      req(input$Q7_4AppPrun)
      if(input$Q7_4AppPrun==T7_4RegPrun){
        numericInput("Q7_4DesCyc","",value=character(0),step=1,width=responseWidth)
      }
    })
    
    #advance from into to survey
    observeEvent(input$beginSurvey,{
      newtab<-switch(input$survey, "Introduction" = "Demographics","Demographics" = "Introduction")
      updateTabItems(session,"survey",newtab)
    })
    #demographics submit button responses and advance
    observeEvent(input$subdemo,{
      if(input$ZIP==""){
        shinyalert(failure,"Please enter a zip code",type="warning")
      }
      else if(input$city=="Other"){
        if(input$tCity==""){
          shinyalert("failure","Please enter a city",type="warning")
        }
        else if(length(getpop(input$ZIP,input$tCity))==0){
          newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
          updateTabItems(session, "survey", newtab)
        }
        else if(input$pop >= 1.1*unique(getpop(input$ZIP,input$tCity))){
          shinyalert(highPop,paste("Our records show a population of ",unique(getpop(input$ZIP,input$tCity))," for ",input$tCity,".  Please check your entry.  If it is correct, proceed.  If it is not correct, re-enter population and re-submit.", sep=""),
                     showCancelButton=T,cancelButtonText="Fix Population",
                     confirmButtonText="Continue",type="warning",closeOnClickOutside=T,closeOnEsc=T,
                     callbackR=function(x){if(x!=FALSE){
                       newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
                       updateTabItems(session, "survey", newtab)}}
          )
        }
        else if(input$pop <= 0.9*unique(getpop(input$ZIP,input$tCity))){
          shinyalert(lowPop,paste("Our records show a population of ",unique(getpop(input$ZIP,input$tCity))," for ",input$tCity,".  Please check your entry.  If it is correct, proceed.  If it is not correct, re-enter population and re-submit.", sep=""),
                     showCancelButton=T,cancelButtonText="Fix Population",
                     confirmButtonText="Continue",type="warning",closeOnClickOutside=T,closeOnEsc=T,
                     callbackR=function(x){if(x!=FALSE){
                       newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
                       updateTabItems(session, "survey", newtab)}})
        }
        else{
          newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
          updateTabItems(session, "survey", newtab)
        }
      }
      else{
        if(length(getpop(input$ZIP,input$city))==0){
          newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
          updateTabItems(session, "survey", newtab)
        }
        else if(input$pop >= 1.1*unique(getpop(input$ZIP,input$city))){
          shinyalert(highPop,paste("Our records show a population of ",unique(getpop(input$ZIP,input$city))," for ",input$city,".  Please check your entry.  If it is correct, proceed.  If it is not correct, re-enter population and re-submit.", sep=""),
                     showCancelButton=T,cancelButtonText="Fix Population",
                     confirmButtonText="Continue",type="warning",closeOnClickOutside=T,closeOnEsc=T,
                     callbackR=function(x){if(x!=FALSE){
                       newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
                       updateTabItems(session, "survey", newtab)}})
        }
        else if(input$pop <= 0.9*unique(getpop(input$ZIP,input$city))){
          shinyalert(lowPop,paste("Our records show a population of ",unique(getpop(input$ZIP,input$city))," for ",input$city,".  Please check your entry.  If it is correct, proceed.  If it is not correct, re-enter population and re-submit.", sep=""),
                     showCancelButton=T,cancelButtonText="Fix Population",
                     confirmButtonText="Continue",type="warning",closeOnClickOutside=T,closeOnEsc=T,
                     callbackR=function(x){if(x!=FALSE){
                       newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
                       updateTabItems(session, "survey", newtab)}})
        }
        else{
          newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
          updateTabItems(session, "survey", newtab)    
        }
      }
    })
    #community framework submit button and advance page
    observeEvent(input$subcf,{
      if(is.na(input$Q1_15TotEmp)|is.na(input$Q1_15FullTiEq)|is.na(input$Q1_3MiMMS)|is.na(input$Q1_3MiMSWTr)|is.na(input$Q1_3AcMMP)|is.na(input$Q1_3AcMGS)|is.na(input$Q1_3AcMPWTr)|is_empty(input$Q1_6OvrcTr)|is_empty(input$Q1_13)|is_empty(input$QintNatVeg)){
        shinyalert(failure,paste("You have not finished question(s): ",if(is.na(input$Q1_3MiMMS)){"1."},if(is.na(input$Q1_3MiMSWTr)){"2."},if(is.na(input$Q1_3AcMMP)){"3."},if(is.na(input$Q1_3AcMGS)){"4."},if(is.na(input$Q1_3AcMPWTr)){"5."},if(is_empty(input$Q1_6OvrcTr)){"6."},if(is_empty(input$Q1_13)){"7."},if(is.na(input$Q1_15TotEmp)){"8a."},if(is.na(input$Q1_15FullTiEq)){"8b."},if(is_empty(input$QintNatVeg)){"9."}),type="error")
      }
      else if(input$Q1_15TotEmp<input$Q1_15FullTiEq){
        shinyalert(failure,"You have more Full Time Equivelent than Total Employees",type="error")
      }
      else if(input$Q1_3MiMSWTr>input$Q1_3MiMMS){
        shinyalert(failure,"You have more miles of streets with trees than miles of streets",type="error")
      }
      else{
        newtab <- switch(input$survey, "Community Framework" = "Budget and Management","Budget and Management" = "Community Framework")
        updateTabItems(session, "survey", newtab)
      }
    })
    
    #budget and management submit button and advance page
    observeEvent(input$subbm,{
      if(is.na(input$Q2_1TotBud)|is.na(input$Q2_3TotTreBud)|is_empty(input$Q2_10CurrNeeds)|is.na(input$Q2_13AdminSupr)|is.na(input$Q2_13TrPlan)|is.na(input$Q2_13TrPrun)|is.na(input$Q2_13TrRem)|is.na(input$Q2_13Othr)|is_empty(input$Q3_1)|is_empty(input$Q3_2AuthTrBd)|is_empty(input$Q3_5MuniOrd)|is_empty(input$Q3_9WrtStgPln)|is_empty(input$Q3_13)){
        shinyalert(failure,paste("You have not finished question(s): ",if(is.na(input$Q2_1TotBud)){"1."},if(is.na(input$Q2_3TotTreBud)){"2."},if(is_empty(input$Q2_10CurrNeeds)){"3."},if(is.na(input$Q2_13AdminSupr)){"4a."},if(is.na(input$Q2_13TrPlan)){"4b."},if(is.na(input$Q2_13TrPrun)){"4c."},if(is.na(input$Q2_13TrRem)){"4d."},if(is.na(input$Q2_13Othr)){"4e."},if(is_empty(input$Q3_1)){"5."},
                                 if(is_empty(input$Q3_2AuthTrBd)){"6."},if(is_empty(input$Q3_5MuniOrd)){"7."},if(is_empty(input$Q3_9WrtStgPln)){"8."},if(is_empty(input$Q3_13)){"9."},sep=""),type="warning")
      }
      else if(sumBudPercent(input$Q2_13AdminSupr,input$Q2_13TrPlan,input$Q2_13TrPrun,input$Q2_13TrRem,input$Q2_13Othr)!=100){
        shinyalert(failure,paste("Your percent of budgets totals ",sumBudPercent(input$Q2_13AdminSupr,input$Q2_13TrPlan,input$Q2_13TrPrun,input$Q2_13TrRem,input$Q2_13Othr),"%. It should total 100%.",sep=""),type="error")
      }
      else if((0.02*input$Q2_1TotBud)<input$Q2_3TotTreBud){
        shinyalert("Check for accuracy",paste("Are you sure your Total Budget of $",input$Q2_1TotBud," and Total Tree Budget of $",input$Q2_3TotTreBud," is accurate?",sep=""),
                   showCancelButton=T,cancelButtonText="No",
                   confirmButtonText="Yes, this is correct",type="warning",closeOnClickOutside=T,closeOnEsc=T,
                   callbackR=function(x){if(x!=FALSE){
                     newtab <- switch(input$survey, "Budget and Management" = "Inventory","Inventory" = "Budget and Management")
                     updateTabItems(session, "survey", newtab)}}) 
      }
      else if(input$Q2_10CurrNeeds==n){
        if(is.na(input$Q2_10Need)){
          shinyalert(failure,"You have not answered question 3a.",type="warning")
        }
        else{
          newtab <- switch(input$survey, "Budget and Management" = "Inventory","Inventory" = "Budget and Management")
          updateTabItems(session, "survey", newtab)
        }}
      else if(input$Q3_5MuniOrd==y){
        if(is.na(input$Q3_5YrUpdt)|(is_empty(input$Q3_61)&is_empty(input$Q3_62)&is_empty(input$Q3_63))){
          shinyalert(failure,paste("You have not answered question(s): ",if(is.na(input$Q3_5YrUpdt)){" 7a. "},if((is_empty(input$Q3_61)&is_empty(input$Q3_62)&is_empty(input$Q3_63))){" 7b. "},sep=","),type="warning")
        }
        else if(input$Q3_5YrUpdt>format(Sys.Date(),"%Y")){
          shinyalert("Whoops!","Please enter a valid year and not a future date for 7a.",type="error")
        }
        else{
          newtab <- switch(input$survey, "Budget and Management" = "Inventory","Inventory" = "Budget and Management")
          updateTabItems(session, "survey", newtab)
        }}
      else if(input$Q3_9WrtStgPln==y){
        if(is.na(input$Q3_9Updt)|is_empty(input$Q3_10)){
          shinyalert(failure,paste("You have not asnwered question: ", if(is.na(input$Q3_9Updt)){" 8a."},if(is_empty(input$Q3_10)){" 8b. "},sep=","),type="warning")
        }
        else if(input$Q3_9Updt>format(Sys.Date(),'%Y')){
          shinyalert("'Whoops!","Please enter a valid year and not a future date for 8a",type="error")
        }
        else{
          newtab <- switch(input$survey, "Budget and Management" = "Inventory","Inventory" = "Budget and Management")
          updateTabItems(session, "survey", newtab)
        }}
      else if(input$Q2_10CurrNeeds==n){
        if(input$Q2_10Need>100|input$Q2_10Need<0){
          shinyalert("Whoops!","Percent need identified (3a.) must be between 0% and 100%",type="error")}
        else{
          newtab <- switch(input$survey, "Budget and Management" = "Inventory","Inventory" = "Budget and Management")
          updateTabItems(session, "survey", newtab)
        }}
      else{
        newtab <- switch(input$survey, "Budget and Management" = "Inventory","Inventory" = "Budget and Management")
        updateTabItems(session, "survey", newtab)
      }
    })
    
    #inventory submit and advance button
    observeEvent(input$subci,{
      if(is_empty(input$Q6_1TrInv)|is_empty(input$Q6_15CanGl)|is.na(input$Q6_18PubTr)|is.na(input$Q6_18PubTrLoc)|is.na(input$Q6_19StrTr)|is.na(input$Q6_19PrkTr)|is.na(input$Q6_19MuniTr)|is.na(input$Q6_20StrTr)|is.na(input$Q6_20PrkTr)|is.na(input$Q6_20MuniTr)){
        shinyalert(failure,paste("You have not answered question(s): ",if(is_empty(input$Q6_1TrInv)){"1. "},if(is_empty(input$Q6_15CanGl)){"2. "},if(is.na(input$Q6_18PubTr)){"3. "},if(is.na(input$Q6_18PubTrLoc)){"4. "},if(is.na(input$Q6_19StrTr)){"5a. "},
                                 if(is.na(input$Q6_19PrkTr)){"5b. "},if(is.na(input$Q6_19MuniTr)){" 5c. "},if(is.na(input$Q6_20StrTr)){"6a. "},if(is.na(input$Q6_20PrkTr)){"6b. "},if(is.na(input$Q6_20MuniTr)){"6c. "}),type="warning")
      }
      else if(input$Q6_18PubTr<sum(input$Q6_19StrTr,input$Q6_19PrkTr,input$Q6_19MuniTr)){
        shinyalert("Whoops!","You have more Street Trees (5a.), Park Trees (5b.), and Municipal Trees (5c.) than publicly owned trees in your community (3).",type="error")
      }
      else if(input$Q6_18PubTrLoc<(input$Q6_20StrTr+input$Q6_20PrkTr+input$Q6_20MuniTr)){
        shinyalert("Whoops!","You have more Street Trees (6a.), Park Trees (6b.), and Municipal Trees (6c.) than publicly owned trees in your community (4).",type="error")
      }
      else if(input$Q6_1TrInv==y){
        if(is.na(input$Q6_3YrUpd)|is_empty(input$Q6_6)|is_empty(input$Q6_7)){
          shinyalert(failure,paste("You have not answered question(s): ",if(is.na(input$Q6_3YrUpd)){"1a. "},if(is_empty(input$Q6_6)){"1b. "},if(is_empty(input$Q6_7)){"1c. "}),type="warning")
        }
        else if(input$Q6_3YrUpd>format(Sys.Date(),"%Y")){
          shinyalert("Whoops!","Please enter a valid year and not a future date (1a.)",type="error")
        }
        else{
          newtab <- switch(input$survey, "Inventory" = "Operations Profile","Operations Profile" = "Inventory")
          updateTabItems(session, "survey", newtab)
        }}
      else if(input$Q6_15CanGl==y){
        if(is.na(input$Q6_16CanGl)|is.na(input$Q6_16CurCn)|is.na(input$Q6_16YrToGl)){
          shinyalert(failure,paste("Your have not answered question(s): ",if(is.na(input$Q6_16CanGl)){"2a. "},if(is.na(input$Q6_16CurCn)){"2b. "},if(is.na(input$Q6_16YrToGl)){"2c. "}),type="warning")
        }
        else if((input$Q6_16CanGl>100|input$Q6_16CurCn>100|input$Q6_16CanGl<0|input$Q6_16CurCn<0)){
          shinyalert("Whoops!","Percentages must be between 0% and 100% (See question 2)",type="error")
        }
        else{
          newtab <- switch(input$survey, "Inventory" = "Operations Profile","Operations Profile" = "Inventory")
          updateTabItems(session, "survey", newtab)
        }}
      else{
        newtab <- switch(input$survey, "Inventory" = "Operations Profile","Operations Profile" = "Inventory")
        updateTabItems(session, "survey", newtab)
      }
    })
    
    #operations profile submit button
    observeEvent(input$sub7,{
      if(is_empty(input$Q5_4)|is.na(input$Q7_1TrPlnt)|is.na(input$Q7_1TrRem)|is.na(input$Q7_2SysSch)|is.na(input$Q7_2ReaDem)|is_empty(input$Q7_14WritTrRsk)|is_empty(input$Q7_12TrRskM)|is_empty(input$Q7_15)|is_empty(input$Q7_4AppPrun)|is_empty(input$Q7_17)){
        shinyalert(failure,paste("You have not answered question(s): ",if(is_empty(input$Q5_4)){"1. "},if(is.na(input$Q7_1TrPlnt)){"2. "},if(is.na(input$Q7_1TrRem)){"3. "},if(is.na(input$Q7_2SysSch)){"4a. "},if(is.na(input$Q7_2ReaDem)){"4b. "},
                                 if(is_empty(input$Q7_12TrRskM)){"5. "},if(is_empty(input$Q7_14WritTrRsk)){"6. "},if(is_empty(input$Q7_15)){"7. "},if(is_empty(input$Q7_4AppPrun)){"8. "},if(is_empty(input$Q7_17)){"9."}),type="warning")
      }
      else if((input$Q7_2SysSch>100|input$Q7_2SysSch<0|input$Q7_2ReaDem>100|input$Q7_2ReaDem<0)){
        shinyalert("Whoops!","Percentages must be between 0% and 100%",type="error")
      }
      else if((input$Q7_2SysSch+input$Q7_2ReaDem)!=100){
        shinyalert(tryAgain,paste(not100, "Total percent is currently ",sum(input$Q7_2SysSch,input$Q7_2ReaDem),"%.",sep=""),type="error")
      }
      else if(input$Q7_4AppPrun==T7_4RegPrun){
        if(is.na(input$Q7_4CurCyc)|is.na(input$Q7_4DesCyc)){
          shinyalert("failure","You haven't answered the cycles",type="warning")
        }
        else{
          newtab<-switch(input$survey, "Operations Profile" = "Assistance Programs","Assistance Programs" = "Operations Profile")
          updateTabItems(session,"survey",newtab)  
        }}
      else{
        newtab<-switch(input$survey, "Operations Profile" = "Assistance Programs","Assistance Programs" = "Operations Profile")
        updateTabItems(session,"survey",newtab)
      }
    })
    
    #assistance programs submit button
    observeEvent(input$sub8,{
      if(is_empty(input$Q8_4AwdCm)|is_empty(input$Q8_5EduPr)|is_empty(input$Q7_10TecAst)|is_empty(input$Q7_10FinAst)|is_empty(input$Q8_10ObvsP)){
        shinyalert(failure,paste("You have not answered question(s): ",if(is_empty(input$Q8_4AwdCm)){"1. "},if(is_empty(input$Q8_5EduPr)){"2. "},if(is_empty(input$Q7_10TecAst)){"3. "},if(is_empty(input$Q7_10FinAst)){"4. "},
                                 if(is_empty(input$Q8_10ObvsP)){"5. "}),type="warning")
      }
      else{
        newtab<-switch(input$survey, "Assistance Programs" = "Tree Budget","Tree Budget" = "Assistance Programs")
        updateTabItems(session,"survey",newtab)
      }
    })
    #write total percent for budget (Q2_13)-this works but only once all  4 have numbers
    output$budPercentSum<-renderText({
      paste("<b>Total percent of budget: ", sum(input$Q2_13AdminSupr,input$Q2_13TrPlan,input$Q2_13TrPrun,input$Q2_13TrRem,input$Q2_13Othr),"</b>",sep="")
    })
    #write totalpercent for tree care schedule reactive vs systematic
    output$TCPer<-renderText({
      paste("<b>Total percent: ",sum(input$Q7_2ReaDem,input$Q7_2SysSch),"</b>",sep="")
    })
    #write population group
    output$popGroup<-renderText({
      paste("<b>Your population group is: ",groupPop(as.numeric(input$pop)),"</b>",sep="")
    })
    #write total number of publicly owned trees
    output$totalTrees<-renderText({
      paste("<b>Total number of trees: ",sum(input$Q6_19StrTr,input$Q6_19PrkTr,input$Q6_19MuniTr),"</b>",sep="")
    })
    #write total number of potential planting locations
    output$totalPlantingLoc<-renderText({
      paste("<b>Total number of planting locations: ",sum(input$Q6_20StrTr,input$Q6_20PrkTr,input$Q6_20MuniTr),"</b>",sep="")
    })
    #write region
    output$regionText<-renderText({
      paste("<b>Your region is: ", printReg(input$region),"</b>",sep="")
    })
    #write table scores
    output$tScore<-renderText({
      paste("<b>Your table score is: ",tableScores,"</b>")
    })
    
    
    #Tree Budget indicator page
    output$plotTB<-renderPlot({
      if(input$compSelTB==reg){
        grid.arrange(grobs = list(if(is_empty(input$pop)|is_empty(input$Q2_3TotTreBud)){
          plotRegion('dollarsPerCapita', 0, 'Tree Budget Per Capita', '$/Capita', '%.10s',
                     precision=2, axisPrecision=0, lowerBound=0, upperBound=15, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('dollarsPerCapita', (input$Q2_3TotTreBud/input$pop), 'Tree Budget Per Capita', '$/Capita', '%.10s',
                     precision=2, axisPrecision=0, lowerBound=0, upperBound=15, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q2_3TotTreBud)|is_empty(input$Q6_18PubTr)){
          plotRegion('dollarsPerPublicTree', 0, 'Tree Budget Per Public Tree', '$/Tree', '%.10s',
                     precision=2, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('dollarsPerPublicTree', (input$Q2_3TotTreBud/input$Q6_18PubTr), 'Tree Budget Per Public Tree', '$/Tree', '%.10s',
                     precision=2, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q2_3TotTreBud)|is_empty(input$Q2_1TotBud)){
          plotRegion('percentOfMuniBud', 0, 'Tree Budget % of Total Municipal Budget', 'Percent', '%.10s',
                     precision=2, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=8) + theme(legend.position="none")
        }
        else{
          plotRegion('percentOfMuniBud', (input$Q2_3TotTreBud/input$Q2_1TotBud), 'Tree Budget % of Total Municipal Budget', 'Percent', '%.10s',
                     precision=2, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=8) + theme(legend.position="none")
        },
        if(is_empty(input$Q2_10CurrNeeds)){
          plotRegion('budgetNeeds', 0, 'Tree Budget % of Identified Need', 'Percent', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else if(input$Q2_10CurrNeeds==n){
          plotRegion('budgetNeeds', input$Q2_10Need, 'Tree Budget % of Identified Need', 'Percent', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('budgetNeeds', 100, 'Tree Budget % of Identified Need', 'Percent', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")}),
        layout_matrix = rbind(c(1,2),
                              c(3,4)))
      }
      else if(input$compSelTB==pop){
        grid.arrange(grobs = list(if(is_empty(input$pop)|is_empty(input$Q2_3TotTreBud)){
          plotPopulation('dollarsPerCapita', 0, 'Tree Budget Per Capita', '$/Capita', '%.10s',
                         precision=2, axisPrecision=0, lowerBound=0, upperBound=15, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('dollarsPerCapita', (input$Q2_3TotTreBud/input$pop), 'Tree Budget Per Capita', '$/Capita', '%.10s',
                         precision=2, axisPrecision=0, lowerBound=0, upperBound=15, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q2_3TotTreBud)|is_empty(input$Q6_18PubTr)){
          plotPopulation('dollarsPerPublicTree', 0, 'Tree Budget Per Public Tree', '$/Tree', '%.10s',
                         precision=2, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('dollarsPerPublicTree', (input$Q2_3TotTreBud/input$Q6_18PubTr), 'Tree Budget Per Public Tree', '$/Tree', '%.10s',
                         precision=2, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q2_10CurrNeeds)){
          plotPopulation('percentOfMuniBud', 0, 'Tree Budget % of Total Municipal Budget', 'Percent', '%.10s',
                         precision=2, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=8) + theme(legend.position="none")
        }
        else{
          plotPopulation('percentOfMuniBud', (input$Q2_3TotTreBud/input$Q2_1TotBud), 'Tree Budget % of Total Municipal Budget', 'Percent', '%.10s',
                         precision=2, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=8) + theme(legend.position="none")
        },
        if(is_empty(input$Q2_10CurrNeeds)){
          plotPopulation('budgetNeeds', 0, 'Tree Budget % of Identified Need', 'Percent', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else if(input$Q2_10CurrNeeds==n){
          plotPopulation('budgetNeeds', input$Q2_10Need, 'Tree Budget % of Identified Need', 'Percent', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('budgetNeeds', 100, 'Tree Budget % of Identified Need', 'Percent', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")}),
        layout_matrix = rbind(c(1,2),
                              c(3,4)))
      }
    })
    
    #governance indicator page
    output$plotG<-renderPlot({
      if(input$compSelG==reg){
        grid.arrange(grobs = list(if(is_empty(input$Q3_2AuthTrBd)){
          plotRegion('treeBoard', 0, 'Community Tree Board or Related Group Exists', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else if(input$Q3_2AuthTrBd==y){
          plotRegion('treeBoard', 100, 'Community Tree Board or Related Group Exists', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('treeBoard', 0, 'Community Tree Board or Related Group Exists', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q3_9WrtStgPln)){
          plotRegion('writtenStratPlan', 0, 'Community has a Written Strategic Plan that Includes Trees', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")}
        else if(input$Q3_9WrtStgPln==y){
          plotRegion('writtenStratPlan', 100, 'Community has a Written Strategic Plan that Includes Trees', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('writtenStratPlan', 0, 'Community has a Written Strategic Plan that Includes Trees', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q3_5MuniOrd)){
          plotRegion('ordinance', 0, 'Community has a Tree Ordinance', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else if(input$Q3_5MuniOrd==y){
          plotRegion('ordinance', 100, 'Community has a Tree Ordinance', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('ordinance', 0, 'Community has a Tree Ordinance', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q3_5MuniOrd)){
          plotRegion('ordinanceYear', 0, 'Community Tree Ordinance up to Date', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else if((as.numeric(format(Sys.Date(),'%Y'))-input$Q3_5YrUpdt)<5){
          plotRegion('ordinanceYear', 100, 'Community Tree Ordinance up to Date', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('ordinanceYear', 0, 'Community Tree Ordinance up to Date', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }),
        layout_matrix = rbind(c(1,2),
                              c(3,4)))
      }
      else if(input$compSelG==pop){
        grid.arrange(grobs = list(if(is_empty(input$Q3_2AuthTrBd)){
          plotPopulation('treeBoard', 0, 'Community Tree Board or Related Group Exists', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else if(input$Q3_2AuthTrBd==y){
          plotPopulation('treeBoard', 100, 'Community Tree Board or Related Group Exists', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('treeBoard', 0, 'Community Tree Board or Related Group Exists', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q3_9WrtStgPln)){
          plotPopulation('writtenStratPlan', 0, 'Community has a Written Strategic Plan that Includes Trees', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")}
        else if(input$Q3_9WrtStgPln==y){
          plotPopulation('writtenStratPlan', 100, 'Community has a Written Strategic Plan that Includes Trees', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('writtenStratPlan', 0, 'Community has a Written Strategic Plan that Includes Trees', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q3_5MuniOrd)){
          plotPopulation('ordinance', 0, 'Community has a Tree Ordinance', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else if(input$Q3_5MuniOrd==y){
          plotPopulation('ordinance', 100, 'Community has a Tree Ordinance', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('ordinance', 0, 'Community has a Tree Ordinance', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q3_5MuniOrd)){
          plotPopulation('ordinanceYear', 0, 'Community Tree Ordinance up to Date', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else if((as.numeric(format(Sys.Date(),'%Y'))-input$Q3_5YrUpdt)<5){
          plotPopulation('ordinanceYear', 100, 'Community Tree Ordinance up to Date', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('ordinanceYear', 0, 'Community Tree Ordinance up to Date', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }),
        layout_matrix = rbind(c(1,2),
                              c(3,4)))
      }
    })
    
    #Trees and People indicator page
    output$plotTP<-renderPlot({
      if(input$compSelTP==reg){
        grid.arrange(grobs = list(if(is_empty(input$Q6_18PubTr)|is_empty(input$pop)){
          plotRegion('manPubTrPerCap', 0, 'Managed Public Trees per Capita', 'Trees per Capita', '%.10s',
                     precision=3, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('manPubTrPerCap', (input$Q6_18PubTr/input$pop), 'Managed Public Trees per Capita', 'Trees per Capita', '%.10s',
                     precision=3, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q6_18PubTr)|is_empty(input$Q1_15TotEmp)){
          plotRegion('manPubTrPerEmp', 0, 'Managed Public Trees per Employee', 'Trees per Employee', '%.10s',
                     precision=0, axisPrecision=0, lowerBound=0, upperBound=15000, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('manPubTrPerEmp', (input$Q6_18PubTr/input$Q1_15TotEmp), 'Managed Public Trees per Employee', 'Trees per Employee', '%.10s',
                     precision=0, axisPrecision=0, lowerBound=0, upperBound=15000, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q1_3AcMMP)|is_empty(input$Q1_3AcMGS)|is_empty(input$Q1_3AcMPWTr)|is_empty(input$pop)){
          plotRegion('greenSpaceAreaMeters', 0, bquote("Green Space Area (" *  m^2 * ") per Person"), bquote("Per Capita (" *  m^2 * ")"), '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=200, axisBreaks=8) + theme(legend.position="none")
        }
        else{
          plotRegion('greenSpaceAreaMeters', (((input$Q1_3AcMMP*sqMetersPerAcre)+(input$Q1_3AcMGS*sqMetersPerAcre)+(input$Q1_3AcMPWTr*sqMetersPerAcre))/input$pop), bquote("Green Space Area (" *  m^2 * ") per Person"), bquote("Per Capita (" *  m^2 * ")"), '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=200, axisBreaks=8) + theme(legend.position="none")
        },
        if(is_empty(input$Q1_3AcMMP)|is_empty(input$Q1_3AcMGS)|is_empty(input$Q1_3AcMPWTr)|is_empty(input$pop)){
          plotRegion('greenSpaceAreaFeet', 0, bquote("Green Space Area (" *  ft^2 * ") per Person"), bquote("Per Capita (" *  ft^2 * ")"), '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=2000, axisBreaks=8) + theme(legend.position="none")
        }
        else{
          plotRegion('greenSpaceAreaFeet', (((input$Q1_3AcMMP*sqFeetPerAcre)+(input$Q1_3AcMGS*sqFeetPerAcre)+(input$Q1_3AcMPWTr*sqFeetPerAcre))/input$pop), bquote("Green Space Area (" *  ft^2 * ") per Person"), bquote("Per Capita (" *  ft^2 * ")"), '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=2000, axisBreaks=8) + theme(legend.position="none")
        }), 
        layout_matrix = rbind(c(1,2),
                              c(3,4)))
      }
      else if(input$compSelTP==pop){
        grid.arrange(grobs = list(if(is_empty(input$Q6_18PubTr)|is_empty(input$pop)){
          plotPopulation('manPubTrPerCap', 0, 'Managed Public Trees per Capita', 'Trees per Capita', '%.10s',
                         precision=3, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('manPubTrPerCap', (input$Q6_18PubTr/input$pop), 'Managed Public Trees per Capita', 'Trees per Capita', '%.10s',
                         precision=3, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q6_18PubTr)|is_empty(input$Q1_15TotEmp)){
          plotPopulation('manPubTrPerEmp', 0, 'Managed Public Trees per Employee', 'Trees per Employee', '%.10s',
                         precision=0, axisPrecision=0, lowerBound=0, upperBound=15000, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('manPubTrPerEmp', (input$Q6_18PubTr/input$Q1_15TotEmp), 'Managed Public Trees per Employee', 'Trees per Employee', '%.10s',
                         precision=0, axisPrecision=0, lowerBound=0, upperBound=15000, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q1_3AcMMP)|is_empty(input$Q1_3AcMGS)|is_empty(input$Q1_3AcMPWTr)|is_empty(input$pop)){
          plotPopulation('greenSpaceAreaMeters', 0, bquote("Green Space Area (" *  m^2 * ") per Person"), bquote("Per Capita (" *  m^2 * ")"), '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=200, axisBreaks=8) + theme(legend.position="none")
        }
        else{
          plotPopulation('greenSpaceAreaMeters', (((input$Q1_3AcMMP*sqMetersPerAcre)+(input$Q1_3AcMGS*sqMetersPerAcre)+(input$Q1_3AcMPWTr*sqMetersPerAcre))/input$pop), bquote("Green Space Area (" *  m^2 * ") per Person"), bquote("Per Capita (" *  m^2 * ")"), '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=200, axisBreaks=8) + theme(legend.position="none")
        },
        if(is_empty(input$Q1_3AcMMP)|is_empty(input$Q1_3AcMGS)|is_empty(input$Q1_3AcMPWTr)|is_empty(input$pop)){
          plotPopulation('greenSpaceAreaFeet', 0, bquote("Green Space Area (" *  ft^2 * ") per Person"), bquote("Per Capita (" *  ft^2 * ")"), '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=2000, axisBreaks=8) + theme(legend.position="none")
        }
        else{
          plotPopulation('greenSpaceAreaFeet', (((input$Q1_3AcMMP*sqFeetPerAcre)+(input$Q1_3AcMGS*sqFeetPerAcre)+(input$Q1_3AcMPWTr*sqFeetPerAcre))/input$pop), bquote("Green Space Area (" *  ft^2 * ") per Person"), bquote("Per Capita (" *  ft^2 * ")"), '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=2000, axisBreaks=8) + theme(legend.position="none")
        }
        ),
        layout_matrix = rbind(c(1,2),
                              c(3,4)))
      }
    })
    
    #Tree Stocking indicator page
    output$plotTS<-renderPlot({
      if(input$compSelTS==reg){
        grid.arrange(grobs = list(if(is_empty(input$Q6_19StrTr)|is_empty(input$Q6_20StrTr)){
          plotRegion('strTrStockingLevel', 0, 'Street Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('strTrStockingLevel', ((input$Q6_19StrTr/(input$Q6_19StrTr+input$Q6_20StrTr))*100), 'Street Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q6_19PrkTr)|is_empty(input$Q6_20PrkTr)){
          plotRegion('prkTrStockingLevel', 0, 'Park Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('prkTrStockingLevel', ((input$Q6_19PrkTr/(input$Q6_19PrkTr+input$Q6_20PrkTr))*100), 'Park Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q6_19MuniTr)|is_empty(input$Q6_20MuniTr)){
          plotRegion('munTrStockingLevel', 0, 'Municipal Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('munTrStockingLevel', ((input$Q6_19MuniTr/(input$Q6_19MuniTr+input$Q6_20MuniTr))*100), 'Municipal Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q6_19StrTr)|is_empty(input$Q1_3MiMMS)){
          plotRegion('pubStrTrDensity', 0, 'Public Street Tree Density per Street Mile', 'Trees per Mile', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=100, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('pubStrTrDensity', (input$Q6_19StrTr/input$Q1_3MiMMS), 'Public Street Tree Density per Street Mile', 'Trees per Mile', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=100, axisBreaks=6) + theme(legend.position="none")
        }),
        layout_matrix = rbind(c(1,2),
                              c(3,4)))
      }
      else if(input$compSelTS==pop){
        grid.arrange(grobs = list(if(is_empty(input$Q6_19StrTr)|is_empty(input$Q6_20StrTr)){
          plotPopulation('strTrStockingLevel', 0, 'Street Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('strTrStockingLevel', ((input$Q6_19StrTr/(input$Q6_19StrTr+input$Q6_20StrTr))*100), 'Street Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q6_19PrkTr)|is_empty(input$Q6_20PrkTr)){
          plotPopulation('prkTrStockingLevel', 0, 'Park Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('prkTrStockingLevel', ((input$Q6_19PrkTr/(input$Q6_19PrkTr+input$Q6_20PrkTr))*100), 'Park Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q6_19MuniTr)|is_empty(input$Q6_20MuniTr)){
          plotPopulation('munTrStockingLevel', 0, 'Municipal Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('munTrStockingLevel', ((input$Q6_19MuniTr/(input$Q6_19MuniTr+input$Q6_20MuniTr))*100), 'Municipal Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q6_19StrTr)|is_empty(input$Q1_3MiMMS)){
          plotPopulation('pubStrTrDensity', 0, 'Public Street Tree Density per Street Mile', 'Trees per Mile', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=120, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('pubStrTrDensity', (input$Q6_19StrTr/input$Q1_3MiMMS), 'Public Street Tree Density per Street Mile', 'Trees per Mile', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=120, axisBreaks=6) + theme(legend.position="none")}),
        layout_matrix = rbind(c(1,2),
                              c(3,4)))
      }
    })
    
    
    #Tree Canopy Indicator page
    output$plotTC<-renderPlot({
      if(input$compSelTC==reg){
        grid.arrange(grobs = list(if(is_empty(input$Q6_15CanGl)){
          plotRegion('canCovGoal', 0, 'Community has Tree Canopy Goal', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('canCovGoal', ifelse(input$Q6_15CanGl==y,100,0), 'Community has Tree Canopy Goal', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q6_16CurCn)){
          plotRegion('percentCurCan', 0, 'Current Tree Canopy Percent', 'Tree Canopy (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('percentCurCan', input$Q6_16CurCn, 'Current Tree Canopy Percent', 'Tree Canopy (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q6_16CanGl)){
          plotRegion('percentCanGoal', 0, 'Tree Canopy Cover Goal', 'Tree Canopy (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('percentCanGoal', input$Q6_16CanGl, 'Tree Canopy Cover Goal', 'Tree Canopy (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q6_16YrToGl)){
          plotRegion('percentCanProgress', 0, 'Progress Toward Tree Canopy Goal', 'Attained (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")  
        }
        else{
          plotRegion('percentCanProgress', ((input$Q6_16CurCn/input$Q6_16CanGl)*100), 'Progress Toward Tree Canopy Goal', 'Attained (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }),
        layout_matrix = rbind(c(1,2),
                              c(3,4)))
      }
      else if(input$compSelTC==pop){
        grid.arrange(grobs = list(if(is_empty(input$Q6_16CanGl)){
          plotPopulation('canCovGoal', 0, 'Community has Tree Canopy Goal', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('canCovGoal', ifelse(input$Q6_15CanGl==y,100,0), 'Community has Tree Canopy Goal', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q6_16CurCn)){
          plotPopulation('percentCurCan', 0, 'Current Tree Canopy Percent', 'Tree Canopy (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('percentCurCan', input$Q6_16CurCn, 'Current Tree Canopy Percent', 'Tree Canopy (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q6_16CanGl)){
          plotPopulation('percentCanGoal', 0, 'Tree Canopy Cover Goal', 'Tree Canopy (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
        }
        else{
          plotPopulation('percentCanGoal', input$Q6_16CanGl, 'Tree Canopy Cover Goal', 'Tree Canopy (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
        },
        if(is_empty(input$Q6_16YrToGl)){
          plotPopulation('percentCanProgress', 0, 'Progress Toward Tree Canopy Goal', 'Attained (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=130, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('percentCanProgress', ((input$Q6_16CurCn/input$Q6_16CanGl)*100), 'Progress Toward Tree Canopy Goal', 'Attained (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=130, axisBreaks=6) + theme(legend.position="none")
        }),
        layout_matrix = rbind(c(1,2),
                              c(3,4)))
      }
    })
    
    #Inspection and Pruning Cycle page
    output$plotIPC<-renderPlot({
      if(input$compSelIPC==reg){
        grid.arrange(grobs = list(if(is_empty(input$Q7_4CurCyc)){
          plotRegion('currentPrunInsCyc', 0, 'Current Inspection and Pruning Cycle', 'Years', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=10, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('currentPrunInsCyc', input$Q7_4CurCyc, 'Current Inspection and Pruning Cycle', 'Years', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=10, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q7_4DesCyc)){
          plotRegion('desiredPrunInsCyc', 0, 'Desired Inspection and Pruning Cycle', 'Years', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=10, axisBreaks=5) + theme(legend.position="none")
        }
        else{
          plotRegion('desiredPrunInsCyc', input$Q7_4DesCyc, 'Desired Inspection and Pruning Cycle', 'Years', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=10, axisBreaks=5) + theme(legend.position="none")
        },
        if(is_empty(input$Q7_4CurCyc)|is_empty(input$Q7_4DesCyc)){
          plotRegion('yearsOfPrunInsCyc', 0, 'Inspection and Pruning Years Off Cycle', 'Years', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=5, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('yearsOfPrunInsCyc', (input$Q7_4CurCyc-input$Q7_4DesCyc), 'Inspection and Pruning Years Off Cycle', 'Years', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=5, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q7_4CurCyc)|is_empty(input$Q7_4DesCyc)){
          plotRegion('percentAttPrunInsCyc', 0, 'Attainment for Inspection and Pruning Cycle', 'Attained (%)', '%.10s',
                     precision= 1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('percentAttPrunInsCyc', ((input$Q7_4DesCyc/input$Q7_4CurCyc)*100), 'Attainment for Inspection and Pruning Cycle', 'Attained (%)', '%.10s',
                     precision= 1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }),
        layout_matrix = rbind(c(1,2),
                              c(3,4)))
      }
      else if(input$compSelIPC==pop){
        grid.arrange(grobs = list(if(is_empty(input$Q7_4CurCyc)){
          plotPopulation('currentPrunInsCyc', 0, 'Current Inspection and Pruning Cycle', 'Years', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=25, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('currentPrunInsCyc', input$Q7_4CurCyc, 'Current Inspection and Pruning Cycle', 'Years', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=25, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q7_4DesCyc)){
          plotPopulation('desiredPrunInsCyc', 0, 'Desired Inspection and Pruning Cycle', 'Years', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=10, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('desiredPrunInsCyc', input$Q7_4DesCyc, 'Desired Inspection and Pruning Cycle', 'Years', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=10, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q7_4CurCyc)|is_empty(input$Q7_4DesCyc)){
          plotPopulation('yearsOfPrunInsCyc', 0, 'Inspection and Pruning Years Off Cycle', 'Years', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=15, axisBreaks=9) + theme(legend.position="none")
        }
        else{
          plotPopulation('yearsOfPrunInsCyc', (input$Q7_4CurCyc-input$Q7_4DesCyc), 'Inspection and Pruning Years Off Cycle', 'Years', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=15, axisBreaks=9) + theme(legend.position="none")
        },
        if(is_empty(input$Q7_4CurCyc)|is_empty(input$Q7_4DesCyc)){
          plotPopulation('percentAttPrunInsCyc', 0, 'Attainment for Inspection and Pruning Cycle', 'Attained (%)', '%.10s',
                         precision= 1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('percentAttPrunInsCyc', ((input$Q7_4DesCyc/input$Q7_4CurCyc)*100), 'Attainment for Inspection and Pruning Cycle', 'Attained (%)', '%.10s',
                         precision= 1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }),
        layout_matrix = rbind(c(1,2),
                              c(3,4)))
      }
    })
    
    #Tree Inventory and Management
    output$plotTIM<-renderPlot({
      if(input$compSelTIM==reg){
        grid.arrange(grobs = list(if(is.na(input$Q7_2SysSch)){
          plotRegion('activeManagement', 0, 'Percentatge Active (Systematic) Management of Tree Population', 'Percent', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else if(input$Q7_2SysSch <= 40){
          plotRegion('activeManagement', 0, 'Percentatge Active (Systematic) Management of Tree Population', 'Percent', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('activeManagement', 100, 'Percentatge Active (Systematic) Management of Tree Population', 'Percent', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q6_1TrInv)){
          plotRegion('treeResourceInventory', 0, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else if(input$Q6_1TrInv==n|is_empty(input$Q6_3YrUpd)){
          plotRegion('treeResourceInventory', 0, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else if((as.numeric(format(Sys.Date(),'%Y'))-input$Q6_3YrUpd)>5){
          plotRegion('treeResourceInventory', 0, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotRegion('treeResourceInventory', 100, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }),
        layout_matrix = rbind(c(1,2)))
      }
      else if(input$compSelTIM==pop){
        grid.arrange(grobs = list(if(is.na(input$Q7_2SysSch)){
          plotPopulation('activeManagement', 0, 'Percentatge Active (Systematic) Management of Tree Population', 'Percent', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else if(input$Q7_2SysSch <= 40){
          plotPopulation('activeManagement', 0, 'Percentatge Active (Systematic) Management of Tree Population', 'Percent', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('activeManagement', 100, 'Percentatge Active (Systematic) Management of Tree Population', 'Percent', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        },
        if(is_empty(input$Q6_1TrInv)){
          plotPopulation('treeResourceInventory', 0, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else if(input$Q6_1TrInv==n|is_empty(input$Q6_3YrUpd)){
          plotPopulation('treeResourceInventory', 0, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else if((as.numeric(format(Sys.Date(),'%Y'))-input$Q6_3YrUpd)>5){
          plotPopulation('treeResourceInventory', 0, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }
        else{
          plotPopulation('treeResourceInventory', 100, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
        }),
        layout_matrix = rbind(c(1,2)))
      }
    })
    
    output$plotCARS<-renderPlot({
      if(input$compSelCARS==reg){
        plotRegion('carsScore', (ifelse(is_empty(input$Q1_13),0,ifelse(input$Q1_13 %in% c(T1_132yd,T1_134yd,T1_13Grad,T1_13ISACA,T1_13ISACMS,T1_13ISAAC,T1_13SAFCF),25,0))+
                                   ifelse(is_empty(input$Q3_61)|is_empty(input$Q3_62)|is_empty(input$Q3_63),0,ifelse(input$Q3_61 %in% c(T3_6RegTrPub,T3_6RegTrPri,T3_6ReqTrND,T3_6ReqTrNP,T3_6ReqTrRP,T3_6ReqRpPOT) & input$Q3_62 %in% c(T3_6ReqPreDev,T3_6ResTrCtPP,T3_6RegAbat,T3_6IdPreHer) & input$Q3_63 %in% c(T3_6RegPubTrMn,T3_6ReqTyMn,T3_6RegRemDd,T3_6EstIDCS,T3_6DefTrReq,T3_6PhrTrTop),25,0))+
                                   ifelse(is_empty(input$Q3_2AuthTrBd),0,ifelse(input$Q3_2AuthTrBd==y,25,ifelse(is_empty(input$Q3_1),0,ifelse(input$Q3_1!="None of the above",25,0))))+
                                   ifelse(is_empty(input$Q6_1TrInv)|is_empty(input$Q3_9WrtStgPln),0,ifelse(input$Q6_1TrInv==y & input$Q3_9WrtStgPln==y,25,0))),
                   'Community Accomplishment Reporting System Score', 'Score (%)', '%.10s',
                   precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
      else if(input$compSelCARS==pop){
        plotPopulation('carsScore', (ifelse(is_empty(input$Q1_13),0,ifelse(input$Q1_13 %in% c(T1_132yd,T1_134yd,T1_13Grad,T1_13ISACA,T1_13ISACMS,T1_13ISAAC,T1_13SAFCF),25,0))+
                                       ifelse(is_empty(input$Q3_61)|is_empty(input$Q3_62)|is_empty(input$Q3_63),0,ifelse(input$Q3_61 %in% c(T3_6RegTrPub,T3_6RegTrPri,T3_6ReqTrND,T3_6ReqTrNP,T3_6ReqTrRP,T3_6ReqRpPOT) & input$Q3_62 %in% c(T3_6ReqPreDev,T3_6ResTrCtPP,T3_6RegAbat,T3_6IdPreHer) & input$Q3_63 %in% c(T3_6RegPubTrMn,T3_6ReqTyMn,T3_6RegRemDd,T3_6EstIDCS,T3_6DefTrReq,T3_6PhrTrTop),25,0))+
                                       ifelse(is_empty(input$Q3_2AuthTrBd),0,ifelse(input$Q3_2AuthTrBd==y,25,ifelse(is_empty(input$Q3_1),0,ifelse(input$Q3_1!="None of the above",25,0))))+
                                       ifelse(is_empty(input$Q6_1TrInv)|is_empty(input$Q3_9WrtStgPln),0,ifelse(input$Q6_1TrInv==y & input$Q3_9WrtStgPln==y,25,0))), 
                       'Community Accomplishment Reporting System Score', 'Score (%)', '%.10s',
                       precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
    })
    
    output$plotCM<-renderPlot({
      len310<-length(input$Q3_10)
      len66<-length(input$Q6_6)
      len313<-length(input$Q3_13)
      len715<-length(input$Q7_15)
      len717<-length(input$Q7_17)
      if(input$compSelCM==reg){
        plotRegion('cmRelativeScore', (as.numeric(tableScores[1])+as.numeric(tableScores[2])+as.numeric(tableScores[3])+as.numeric(tableScores[4])+as.numeric(tableScores[5])+as.numeric(tableScores[6])+as.numeric(tableScores[7])+
                                         ifelse(is_empty(input$Q3_9WrtStgPln)|is_empty(input$Q3_10),0,ifelse(input$Q3_9WrtStgPln==n,1,ifelse(input$Q3_9WrtStgPln==d,1.5,ifelse(input$Q3_9WrtStgPln==y&is_empty(input$Q3_10),2,ifelse(input$Q3_9WrtStgPln==y&(len310==1|len310==2),2.5,ifelse(input$Q3_9WrtStgPln==y&(len310==3|len310==4),3,ifelse(input$Q3_9WrtStgPln==y&(len310==5|len310==6),3.5,ifelse(input$Q3_9WrtStgPln==y&(len310==7|len310==8),4,0))))))))+
                                         ifelse(is_empty(input$Q2_10CurrNeeds)|is_empty(input$Q7_2SysSch),0,ifelse(input$Q2_10CurrNeeds==y&input$Q7_2SysSch>=70,4,ifelse(input$Q2_10CurrNeeds==y&input$Q7_2SysSch>=40,3,ifelse(input$Q2_10CurrNeeds==y|input$Q7_2SysSch>=40,2,ifelse(input$Q2_10CurrNeeds==n|input$Q7_2SysSch<40,1,0)))))+
                                         ifelse(is_empty(input$Q1_6OvrcTr)&is_empty(input$Q1_13),0,ifelse(input$Q1_6OvrcTr==n&is_empty(input$Q1_13),1,ifelse(input$Q1_6OvrcTr==y&is_empty(input$Q1_13),2,ifelse(input$Q1_6OvrcTr==y & T1_13ISACA %in% input$Q1_13 & (T1_132yd %in% input$Q1_13|T1_134yd %in% input$Q1_13|T1_13Grad %in% input$Q1_13),4,ifelse(input$Q1_6OvrcTr==y & (T1_13ISACA %in% input$Q1_13|T1_132yd %in% input$Q1_13|T1_134yd %in% input$Q1_13|T1_13Grad %in% input$Q1_13),3,ifelse(input$Q1_6OvrcTr==y & (T1_13InHOrJ %in% input$Q1_13|T1_13AttTree %in% input$Q1_13),2.5,ifelse(input$Q1_6OvrcTr==y,2,ifelse(input$Q1_6OvrcTr==n,1,0))))))))+
                                         ifelse(is_empty(input$Q6_1TrInv),0,ifelse(input$Q6_1TrInv==n,1,ifelse(input$Q6_1TrInv==y&is_empty(input$Q6_6),2,ifelse(input$Q6_1TrInv==y & len66>=2 & is_empty(input$Q6_7),3,ifelse(input$Q6_1TrInv==y & len66>=2 & T6_8GPSGIS %in% input$Q6_7,4,ifelse(input$Q6_1TrInv==y & len66>=2,3,ifelse(input$Q6_1TrInv==y,2,0)))))))+
                                         ifelse(is_empty(input$Q3_5MuniOrd),0,ifelse(input$Q3_5MuniOrd==d,0,ifelse(input$Q3_5MuniOrd==n,1,ifelse(input$Q3_5MuniOrd==y,ifelse(is_empty(input$Q3_61)&is_empty(input$Q3_62)&is_empty(input$Q3_63),1,ifelse(T3_6ReqPreDev %in% input$Q3_62&T3_6IdPreHer %in% input$Q3_62&T3_6PhrTrTop %in% input$Q3_63,4,ifelse((T3_6ReqPreDev %in% input$Q3_62&T3_6IdPreHer %in% input$Q3_62)|(T3_6IdPreHer %in% input$Q3_62&T3_6PhrTrTop %in% input$Q3_63)|(T3_6ReqPreDev %in% input$Q3_62&T3_6PhrTrTop %in% input$Q3_63),3,ifelse(T3_6ReqPreDev %in% input$Q3_62|T3_6IdPreHer %in% input$Q3_62|T3_6PhrTrTop %in% input$Q3_63,2,1))))))))+
                                         ifelse(is_empty(input$Q3_5MuniOrd),0,ifelse(input$Q3_5MuniOrd==d,0,ifelse(input$Q3_5MuniOrd==n,1,ifelse(input$Q3_5MuniOrd==y,ifelse(!is_empty(input$Q3_61)&!is_empty(input$Q6_1TrInv),ifelse(T3_6RegTrPub %in% input$Q3_61&T3_6RegTrPri %in% input$Q3_61&(T6_7TrePlLoc %in% input$Q6_7|T6_7SelTrS %in% input$Q6_7),4,ifelse(T3_6RegTrPub %in% input$Q3_61&T3_6RegTrPri %in% input$Q3_61,3.5,ifelse(T3_6RegTrPub %in% input$Q3_61|T3_6RegTrPri %in% input$Q3_61,3,2))),1)))))+
                                         ifelse(is_empty(input$Q3_13),0,ifelse(len313>=3,4,ifelse(len313==2,3,ifelse(len313==1,2,1))))+
                                         ifelse(is_empty(input$Q7_12TrRskM),0,ifelse(input$Q7_12TrRskM==n,1,ifelse(is_empty(input$Q7_14WritTrRsk),1,ifelse(input$Q7_12TrRskM==y&input$Q7_14WritTrRsk==n,2,ifelse(input$Q7_12TrRskM==y&input$Q7_14WritTrRsk==y&len715>=3,4,ifelse(input$Q7_12TrRskM==y&input$Q7_14WritTrRsk==y&len715>=1,3,2))))))+
                                         ifelse(is_empty(input$Q7_17),0,ifelse(T7_17Landfilled %in% input$Q7_17|T7_17BrnOp %in% input$Q7_17,1,ifelse(len717>=3,4,ifelse(len717==2,3,ifelse(len717==1,2,1)))))+
                                         ifelse(is_empty(input$Q6_1TrInv),0,ifelse(input$Q6_1TrInv==n,1,ifelse(is_empty(input$Q6_7),1,ifelse(input$Q6_1TrInv==y&T6_7CanCo %in% input$Q6_7&T6_7RmSens %in% input$Q6_7&T6_8GPSGIS %in% input$Q6_7,4,ifelse(input$Q6_1TrInv==y&T6_7CanCo %in% input$Q6_7&T6_7RmSens %in% input$Q6_7,3,ifelse(input$Q6_1TrInv==y&T6_7CanCo %in% input$Q6_7,2,1))))))+
                                         ifelse(is_empty(input$Q6_1TrInv),0,ifelse(input$Q6_1TrInv==n,1,ifelse(!is_empty(input$Q6_7)&!is_empty(input$Q6_6),ifelse(input$Q6_1TrInv==y&(T6_6PriTr %in% input$Q6_6|T6_7iTrEco %in% input$Q6_7)&T6_8GPSGIS %in% input$Q6_7,4,ifelse(input$Q6_1TrInv==y&(T6_6PriTr %in% input$Q6_6|T6_7iTrEco %in% input$Q6_7),3,ifelse(!is_empty(input$Q6_6),ifelse(input$Q6_1TrInv==y&T6_6StrTr %in% input$Q6_6,2,1),1))),1)))+
                                         ifelse(is_empty(input$Q6_1TrInv),0,ifelse(input$Q6_1TrInv==n,1,ifelse(!is_empty(input$Q6_7),ifelse(T6_7SelTrS %in% input$Q6_7&T6_7iTrEco %in% input$Q6_7&T6_8GPSGIS %in% input$Q6_7,4,ifelse(T6_7SelTrS %in% input$Q6_7&T6_7iTrEco %in% input$Q6_7,3,ifelse(T6_7SelTrS %in% input$Q6_7,2,1))),1)))+
                                         ifelse(is_empty(input$QintNatVeg),0,ifelse(input$QintNatVeg==intNVLow,1,ifelse(input$QintNatVeg==intNVMod,2,ifelse(input$QintNatVeg==intNVGood,3,ifelse(input$QintNatVeg==intNVOpt,4,0)))))),
                   'Urban Forest Sustainability Relative Score', 'Score', '%.10s',
                   precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
      else if(input$compSelCM==pop){
        plotPopulation('cmRelativeScore', (as.numeric(tableScores[1])+as.numeric(tableScores[2])+as.numeric(tableScores[3])+as.numeric(tableScores[4])+as.numeric(tableScores[5])+as.numeric(tableScores[6])+as.numeric(tableScores[7])+
                                             ifelse(is_empty(input$Q3_9WrtStgPln)|is_empty(input$Q3_10),0,ifelse(input$Q3_9WrtStgPln==n,1,ifelse(input$Q3_9WrtStgPln==d,1.5,ifelse(input$Q3_9WrtStgPln==y&is_empty(input$Q3_10),2,ifelse(input$Q3_9WrtStgPln==y&(len310==1|len310==2),2.5,ifelse(input$Q3_9WrtStgPln==y&(len310==3|len310==4),3,ifelse(input$Q3_9WrtStgPln==y&(len310==5|len310==6),3.5,ifelse(input$Q3_9WrtStgPln==y&(len310==7|len310==8),4,0))))))))+
                                             ifelse(is_empty(input$Q2_10CurrNeeds)|is_empty(input$Q7_2SysSch),0,ifelse(input$Q2_10CurrNeeds==y&input$Q7_2SysSch>=70,4,ifelse(input$Q2_10CurrNeeds==y&input$Q7_2SysSch>=40,3,ifelse(input$Q2_10CurrNeeds==y|input$Q7_2SysSch>=40,2,ifelse(input$Q2_10CurrNeeds==n|input$Q7_2SysSch<40,1,0)))))+
                                             ifelse(is_empty(input$Q1_6OvrcTr)&is_empty(input$Q1_13),0,ifelse(input$Q1_6OvrcTr==n&is_empty(input$Q1_13),1,ifelse(input$Q1_6OvrcTr==y&is_empty(input$Q1_13),2,ifelse(input$Q1_6OvrcTr==y & T1_13ISACA %in% input$Q1_13 & (T1_132yd %in% input$Q1_13|T1_134yd %in% input$Q1_13|T1_13Grad %in% input$Q1_13),4,ifelse(input$Q1_6OvrcTr==y & (T1_13ISACA %in% input$Q1_13|T1_132yd %in% input$Q1_13|T1_134yd %in% input$Q1_13|T1_13Grad %in% input$Q1_13),3,ifelse(input$Q1_6OvrcTr==y & (T1_13InHOrJ %in% input$Q1_13|T1_13AttTree %in% input$Q1_13),2.5,ifelse(input$Q1_6OvrcTr==y,2,ifelse(input$Q1_6OvrcTr==n,1,0))))))))+
                                             ifelse(is_empty(input$Q6_1TrInv),0,ifelse(input$Q6_1TrInv==n,1,ifelse(input$Q6_1TrInv==y&is_empty(input$Q6_6),2,ifelse(input$Q6_1TrInv==y & len66>=2 & is_empty(input$Q6_7),3,ifelse(input$Q6_1TrInv==y & len66>=2 & T6_8GPSGIS %in% input$Q6_7,4,ifelse(input$Q6_1TrInv==y & len66>=2,3,ifelse(input$Q6_1TrInv==y,2,0)))))))+
                                             ifelse(is_empty(input$Q3_5MuniOrd),0,ifelse(input$Q3_5MuniOrd==d,0,ifelse(input$Q3_5MuniOrd==n,1,ifelse(input$Q3_5MuniOrd==y,ifelse(is_empty(input$Q3_61)&is_empty(input$Q3_62)&is_empty(input$Q3_63),1,ifelse(T3_6ReqPreDev %in% input$Q3_62&T3_6IdPreHer %in% input$Q3_62&T3_6PhrTrTop %in% input$Q3_63,4,ifelse((T3_6ReqPreDev %in% input$Q3_62&T3_6IdPreHer %in% input$Q3_62)|(T3_6IdPreHer %in% input$Q3_62&T3_6PhrTrTop %in% input$Q3_63)|(T3_6ReqPreDev %in% input$Q3_62&T3_6PhrTrTop %in% input$Q3_63),3,ifelse(T3_6ReqPreDev %in% input$Q3_62|T3_6IdPreHer %in% input$Q3_62|T3_6PhrTrTop %in% input$Q3_63,2,1))))))))+
                                             ifelse(is_empty(input$Q3_5MuniOrd),0,ifelse(input$Q3_5MuniOrd==d,0,ifelse(input$Q3_5MuniOrd==n,1,ifelse(input$Q3_5MuniOrd==y,ifelse(!is_empty(input$Q3_61)&!is_empty(input$Q6_1TrInv),ifelse(T3_6RegTrPub %in% input$Q3_61&T3_6RegTrPri %in% input$Q3_61&(T6_7TrePlLoc %in% input$Q6_7|T6_7SelTrS %in% input$Q6_7),4,ifelse(T3_6RegTrPub %in% input$Q3_61&T3_6RegTrPri %in% input$Q3_61,3.5,ifelse(T3_6RegTrPub %in% input$Q3_61|T3_6RegTrPri %in% input$Q3_61,3,2))),1)))))+
                                             ifelse(is_empty(input$Q3_13),0,ifelse(len313>=3,4,ifelse(len313==2,3,ifelse(len313==1,2,1))))+
                                             ifelse(is_empty(input$Q7_12TrRskM),0,ifelse(input$Q7_12TrRskM==n,1,ifelse(is_empty(input$Q7_14WritTrRsk),1,ifelse(input$Q7_12TrRskM==y&input$Q7_14WritTrRsk==n,2,ifelse(input$Q7_12TrRskM==y&input$Q7_14WritTrRsk==y&len715>=3,4,ifelse(input$Q7_12TrRskM==y&input$Q7_14WritTrRsk==y&len715>=1,3,2))))))+
                                             ifelse(is_empty(input$Q7_17),0,ifelse(T7_17Landfilled %in% input$Q7_17|T7_17BrnOp %in% input$Q7_17,1,ifelse(len717>=3,4,ifelse(len717==2,3,ifelse(len717==1,2,1)))))+
                                             ifelse(is_empty(input$Q6_1TrInv),0,ifelse(input$Q6_1TrInv==n,1,ifelse(is_empty(input$Q6_7),1,ifelse(input$Q6_1TrInv==y&T6_7CanCo %in% input$Q6_7&T6_7RmSens %in% input$Q6_7&T6_8GPSGIS %in% input$Q6_7,4,ifelse(input$Q6_1TrInv==y&T6_7CanCo %in% input$Q6_7&T6_7RmSens %in% input$Q6_7,3,ifelse(input$Q6_1TrInv==y&T6_7CanCo %in% input$Q6_7,2,1))))))+
                                             ifelse(is_empty(input$Q6_1TrInv),0,ifelse(input$Q6_1TrInv==n,1,ifelse(!is_empty(input$Q6_7)&!is_empty(input$Q6_6),ifelse(input$Q6_1TrInv==y&(T6_6PriTr %in% input$Q6_6|T6_7iTrEco %in% input$Q6_7)&T6_8GPSGIS %in% input$Q6_7,4,ifelse(input$Q6_1TrInv==y&(T6_6PriTr %in% input$Q6_6|T6_7iTrEco %in% input$Q6_7),3,ifelse(!is_empty(input$Q6_6),ifelse(input$Q6_1TrInv==y&T6_6StrTr %in% input$Q6_6,2,1),1))),1)))+
                                             ifelse(is_empty(input$Q6_1TrInv),0,ifelse(input$Q6_1TrInv==n,1,ifelse(!is_empty(input$Q6_7),ifelse(T6_7SelTrS %in% input$Q6_7&T6_7iTrEco %in% input$Q6_7&T6_8GPSGIS %in% input$Q6_7,4,ifelse(T6_7SelTrS %in% input$Q6_7&T6_7iTrEco %in% input$Q6_7,3,ifelse(T6_7SelTrS %in% input$Q6_7,2,1))),1)))+
                                             ifelse(is_empty(input$QintNatVeg),0,ifelse(input$QintNatVeg==intNVLow,1,ifelse(input$QintNatVeg==intNVMod,2,ifelse(input$QintNatVeg==intNVGood,3,ifelse(input$QintNatVeg==intNVOpt,4,0)))))),
                       'Urban Forest Sustainability Relative Score', 'Score', '%.10s',
                       precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
    })
    
    output$plotSMA<-renderPlot({
      if(input$compSelSMA==reg){
        plotRegion('smaScore', (ifelse(is_empty(input$Q1_13),0,ifelse(input$Q1_13 %in% T1_13ISACA,100/6,0))+
                                  ifelse(is_empty(input$Q3_9WrtStgPln),0,ifelse(input$Q3_9WrtStgPln==y,100/6,0))+
                                  ifelse(is_empty(input$Q3_2AuthTrBd)|is_empty(input$Q3_1)|is_empty(input$Q3_5MuniOrd)|is_empty(input$Q2_3TotTreBud)|is_empty(input$pop)|is_empty(input$Q8_10ObvsP),0,
                                         ifelse(input$Q3_2AuthTrBd==y&input$Q3_1!="None of the above"&input$Q3_5MuniOrd==y&(input$Q2_3TotTreBud/input$pop)>=2&input$Q8_10ObvsP==y,100/6,0))+
                                  ifelse(is_empty(input$Q8_4AwdCm),0,ifelse(input$Q8_4AwdCm==y,100/6,0))+
                                  ifelse(is_empty(input$Q5_4),0,ifelse(T5_4PrefTCI %in% input$Q5_4,100/6,0))+
                                  ifelse(is_empty(input$Q3_13),0,ifelse(T3_13AnsiZ133 %in% input$Q3_13 & T3_13AnsiA300 %in% input$Q3_13,100/6,0))), 
                   'Society of Municipal Arborists Accreditation Program Score', 'Score (%)', '%.10s',
                   precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
      else if(input$compSelSMA==pop){
        plotPopulation('smaScore', (ifelse(is_empty(input$Q1_13),0,ifelse(input$Q1_13 %in% T1_13ISACA,100/6,0))+
                                      ifelse(is_empty(input$Q3_9WrtStgPln),0,ifelse(input$Q3_9WrtStgPln==y,100/6,0))+
                                      ifelse(is_empty(input$Q3_2AuthTrBd)|is_empty(input$Q3_1)|is_empty(input$Q3_5MuniOrd)|is_empty(input$Q2_3TotTreBud)|is_empty(input$pop)|is_empty(input$Q8_10ObvsP),0,
                                             ifelse(input$Q3_2AuthTrBd==y&input$Q3_1!="None of the above"&input$Q3_5MuniOrd==y&(input$Q2_3TotTreBud/input$pop)>=2&input$Q8_10ObvsP==y,100/6,0))+
                                      ifelse(is_empty(input$Q8_4AwdCm),0,ifelse(input$Q8_4AwdCm==y,100/6,0))+
                                      ifelse(is_empty(input$Q5_4),0,ifelse(T5_4PrefTCI %in% input$Q5_4,100/6,0))+
                                      ifelse(is_empty(input$Q3_13),0,ifelse(T3_13AnsiZ133 %in% input$Q3_13 & T3_13AnsiA300 %in% input$Q3_13,100/6,0))), 
                       'Society of Municipal Arborists Accreditation Program Score', 'Score (%)', '%.10s',
                       precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
    })
    
    output$plotTCUSA<-renderPlot({
      if(input$compSelTCUSA==reg){
        plotRegion('tcScore', (ifelse(is_empty(input$Q3_2AuthTrBd),0,ifelse(input$Q3_2AuthTrBd==y,25,ifelse(is_empty(input$Q3_1),0,ifelse(input$Q3_1!="None of the above",25,0))))+
                                 ifelse(is_empty(input$Q3_5MuniOrd),0,ifelse(input$Q3_5MuniOrd==y,25,0))+
                                 ifelse(is_empty(input$Q2_3TotTreBud)|is_empty(input$pop),0,ifelse((input$Q2_3TotTreBud/input$pop)>=2,25,0))+
                                 ifelse(is_empty(input$Q8_10ObvsP),0,ifelse(input$Q8_10ObvsP==y,25,0)))
                   , 'Tree City USA Standards', 'Score (%)', '%.10s',
                   precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
      else if(input$compSelTCUSA==pop){
        plotPopulation('tcScore', (ifelse(is_empty(input$Q3_2AuthTrBd),0,ifelse(input$Q3_2AuthTrBd==y,25,ifelse(is_empty(input$Q3_1),0,ifelse(input$Q3_1!="None of the above",25,0))))+
                                     ifelse(is_empty(input$Q3_5MuniOrd),0,ifelse(input$Q3_5MuniOrd==y,25,0))+
                                     ifelse(is_empty(input$Q2_3TotTreBud)|is_empty(input$pop),0,ifelse((input$Q2_3TotTreBud/input$pop)>=2,25,0))+
                                     ifelse(is_empty(input$Q8_10ObvsP),0,ifelse(input$Q8_10ObvsP==y,25,0)))
                       , 'Tree City USA Standards', 'Score (%)', '%.10s',
                       precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
    })
    #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #Titles for sumamry pages
    output$myTSIndiHeader<-renderText({
      paste("<h3>","MyTreeScore Indicator for ",ifelse(input$city=="Other",input$tCity,input$city),"</h3>")
    })
    
    output$myTSIndeHeader<-renderText({
      paste("<h3>","MyTreeScore Indexes",ifelse(input$city=="Other",input$tCity,input$city),"</h3>")
    })
    
    #table text test
    output$myTSIndic1<-renderTable(
      data.frame("Indicators_and_Categories"=c("<b><u>Indicators & Categories</b></u>","<b><u>Tree Budget & Need</u></b>","Budget per Capita","Budget per Public Tree","Budget percent of Total Municipal Budget","Budget percent of Identified Need"#,"<hr></hr>"
                                               ,"<b><u>Resource Budget Allocation</b></u>","Administration","Planting","Pruning","Removal","Other"#,"<hr></hr>"
                                               ,"<b><u>Community Trees & Governance</b></u>","Tree Board or Related Group Exists","Written Strategic Plan Exists","Written Strategic Plan up to Date","Tree Ordinance Exists","Tree Ordinance up to Date"#,"<hr></hr>"
                                               ,"<b><u>Tree Inventory & Space Allocation</b></u>","Tree Inventory Exists","Tree Inventory up to Date","Public Green Space Area","Public Green Space Area"#,"<hr></hr>"
                                               ,"<b><u>Tree Canopy Assessment</b></u>","Tree Canopy Goal Exists","Current Tree Canopy","Tree Canopy Cover Goal Achievement","Years to Achieve Tree Canopy Goal"#,"<hr></hr>"
                                               ,"<b><u>Tree Stocking Level</b></u>","Street Tree Stocking Level Attainment","Park Tree Stocking Level Attainment","Public Tree Stocking Level Attainment","Public Street Tree Density"#,"<hr></hr>"
                                               ,"<b><u>Active (Systematic) Management</b></u>","Current Inspection and Pruning Cycle","Desired Inspectino and Pruning Cycle","Time Inspection and Pruning Off Cycle","Attainment of Inspection and Pruning Cycle","Active Management Level of Tree Population"
      ),
      "MyTreeScore"=c("<b><u>MyTreeScore</b></u>","",specify_decimal((input$Q2_3TotTreBud/input$pop),2),specify_decimal((input$Q2_3TotTreBud/input$Q6_18PubTr),2),specify_decimal((input$Q2_3TotTreBud/input$Q2_1TotBud),2),ifelse(is_empty(input$Q2_10CurrNeeds),'NA',ifelse(input$Q2_10CurrNeeds==y,100,input$Q2_10Need))#,"<hr></hr>"
                      ,"",input$Q2_13AdminSupr,input$Q2_13TrPlan,input$Q2_13TrPrun,input$Q2_13TrRem,input$Q2_13Othr#,"<hr></hr>"
                      ,"",ifelse(is_empty(input$Q3_2AuthTrBd),'NA',ifelse(input$Q3_2AuthTrBd==y,100,0)),ifelse(is_empty(input$Q3_9WrtStgPln),'NA',ifelse(input$Q3_9WrtStgPln==y,100,0)),ifelse(is_empty(input$Q3_9WrtStgPln),'NA',ifelse(input$Q3_9WrtStgPln==y,(ifelse(((as.numeric(format(Sys.Date(),'%Y'))-input$Q3_9Updt)<=5),100,0)),0)),ifelse(is_empty(input$Q3_5MuniOrd),'NA',ifelse(input$Q3_5MuniOrd==y,100,0)),ifelse(is_empty(input$Q3_5MuniOrd),ifelse(input$Q3_5MuniOrd==y,(ifelse(((as.numeric(format(Sys.Date(),'%Y'))-input$Q3_5YrUpdt)<=5),100,0)),0))#,"<hr></hr>"
                      ,"",ifelse(is_empty(input$Q6_1TrInv),'NA',ifelse(input$Q6_1TrInv==y,100,0)),ifelse(is_empty(input$Q6_1TrInv),'NA',ifelse(is_empty(input$Q6_3YrUpd),'NA',ifelse((as.numeric(format(Sys.Date(),'%Y'))-input$Q6_3YrUpd)<=5),100,0)),ifelse((is_empty(input$Q1_3AcMMP)|is_empty(input$Q1_3AcMGS)|is_empty(input$Q1_3AcMPWTr)),'NA',(((input$Q1_3AcMMP*sqMetersPerAcre)+(input$Q1_3AcMGS*sqMetersPerAcre)+(input$Q1_3AcMPWTr*sqMetersPerAcre))/input$pop)),ifelse((is_empty(input$Q1_3AcMMP)|is_empty(input$Q1_3AcMGS)|is_empty(input$Q1_3AcMPWTr)),'NA',(((input$Q1_3AcMMP*sqFeetPerAcre)+(input$Q1_3AcMGS*sqFeetPerAcre)+(input$Q1_3AcMPWTr*sqFeetPerAcre))/input$pop))#,"<hr></hr>"
                      ,"",ifelse(is_empty(input$Q6_15CanGl),'NA',ifelse(input$Q6_15CanGl==y,100,0)),ifelse(is_empty(input$Q6_16CurCn),'NA',input$Q6_16CurCn),ifelse((is_empty(input$CurCn)|is_empty(input$Q6_16CanGl)),'NA',(input$Q6_16CurCn/input$Q6_16CanGl)),ifelse(is_empty(input$Q6_16YrsToGl),'NA',(input$Q6_16YrsToGl-as.numeric(format(Sys.Date(),'%Y'))))#,"<hr></hr>"
                      ,"",(input$Q6_19StrTr/(input$Q6_19StrTr+input$Q6_20StrTr)),(input$Q6_19PrkTr/(input$Q6_19PrkTr+input$Q6_20PrkTr)),(input$Q6_19MuniTr/(input$Q6_19MuniTr+input$Q6_20MuniTr)),(input$Q6_19StrTr/input$Q1_3MiMMS)#,"<hr></hr>"
                      ,"",ifelse(is_empty(input$Q7_4AppPrun),'NA',ifelse(is_empty(input$Q7_4CurCyc),'NA',input$Q7_4CurCyc)),ifelse(is_empty(input$Q7_4AppPrun),'NA',ifelse(is_empty(input$Q7_4DesCyc),'NA',input$Q7_4DesCyc)),ifelse((is_empty(input$Q7_4CurCyc)|is_empty(input$Q7_4DesCyc)),'NA',(input$Q7_4CurCyc-input$Q7_4DesCyc)),ifelse((is_empty(input$Q7_4CurCyc)|is_empty(input$Q7_4DesCyc)),'NA',(input$Q7_4CurCyc/input$Q7_4DesCyc)),input$Q7_2SysSch
      ),
      "Regional"=c("<b><u>Regional</b></u>","",8.90,37.58,0.62,88#,"<hr></hr>"
                   ,"",9,33,21,23,14#,"<hr></hr>"
                   ,"",79,87,52,89,52#,"<hr></hr>"
                   ,"",49,51,112,678#,"<hr></hr>"
                   ,"",31,33,52,7#,"<hr></hr>"
                   ,"",78,62,97,131.8#,"<hr></hr>"
                   ,"",6.9,4.8,3.1,52,57
      ),
      "National"=c("<b><u>National</b></u>","",specify_decimal(mean(data$dollarsPerCapita,na.rm=T),2),specify_decimal(mean(data$dollarsPerPublicTree,na.rm=T),2),specify_decimal(mean(data$percentOfMuniBud,na.rm=T),2),specify_decimal(mean(data$budgetNeeds,na.rm=T),2)#,"<hr></hr>"
                   ,"",specify_decimal(mean(data$raAdmin,na.rm=T),2),specify_decimal(mean(data$raPlan,na.rm=T),2),specify_decimal(mean(data$raPrun,na.rm=T),2),specify_decimal(mean(data$raRem,na.rm=T),2),specify_decimal(mean(data$raOther,na.rm=T),2)#,"<hr></hr>"
                   ,"",specify_decimal(mean(data$treeBoard,na.rm=T),2),specify_decimal(mean(data$writtenStratPlan,na.rm=T),2),61,specify_decimal(mean(data$ordinance,na.rm=T),2),specify_decimal(mean(data$ordinanceYear,na.rm=T),2)#,"<hr></hr>"
                   ,"",specify_decimal(mean(data$treeResourceInventory,na.rm=T),2),53,specify_decimal(mean(data$greenSpaceAreaMeters,na.rm=T),2),specify_decimal(mean(data$greenSpaceAreaFeet,na.rm=T),2)#,"<hr></hr>"
                   ,"",26,22,61,11#,"<hr></hr>"
                   ,"",specify_decimal(mean(data$strTrStockingLevel,na.rm=T),2),specify_decimal(mean(data$prkTrStockingLevel,na.rm=T),2),88,specify_decimal(mean(data$pubStrTrDensity,na.rm=T),2)#,"<hr></hr>"
                   ,"",specify_decimal(mean(data$currentPrunInsCyc,na.rm=T),2),specify_decimal(mean(data$desiredPrunInsCyc,na.rm=T),2),specify_decimal(mean(data$yearsOfPrunInsCyc,na.rm=T),2),specify_decimal(mean(data$percentAttPrunInsCyc,na.rm=T),2),specify_decimal(mean(data$activeManagement,na.rm=T),2)
      ),
      "Unit"=c("<b><u>Unit</b></u>","","($/capita)","($/tree)","(%)","(%)"#,"<hr></hr>"
               ,"","(%)","(%)","(%)","(%)","(%)"#,"<hr></hr>"
               ,"","(% yes)","(% yes)","(% yes)","(% yes)","(% yes)"#,"<hr></hr>"
               ,"","(% yes)","(% yes)","(sq. m/capita)","(sq. ft/capita)"#,"<hr></hr>"
               ,"","(% yes)","(%)","(% Goal Met)","(years)"#,"<hr></hr>"
               ,"","(% Attained)","(% Attained)","(% Attained)","(Trees/Mile)"#,"<hr></hr>"
               ,"","(years)","(years)","(years)","(% Attained)","(%)"
      )
      ),width='100%',align="lcccc",colnames=F,spacing="xs",
      sanitize.text.function=function(x){x})
    
    #Bottom of indicators summary text
    output$endMsg1<-renderText({
      paste(theAnalysis,format(Sys.Date(),"%B %d, %Y"),theAnalysis2,"&copy;",copyrightDate)
    })
    output$endMsg2<-renderText({
      indPageLine2
    })
    output$endMsg3<-renderText({
      indPageLine3
    })
    output$endMsg4<-renderText({
      paste(indPageLine4a,a("click here",href="https://www.uwsp.edu/cnr/Pages/Forestry---MTCUS.aspx"),indPageLine4b)
    })
    
    #create the pdfs based on the user input
    output$indicatorReport<-downloadHandler(
      filename="indicatorReport.pdf",
      content=function(file){
        rmarkdown::render("Indicators.Rmd",output_file=file,
                          params=list(city=input$city,tcity=input$tCity,region=input$region,budget=input$Q2_3TotTreBud,pop=input$pop,trees=input$Q6_18PubTr,totBud=input$Q2_1TotBud,budmet=input$Q2_10CurrNeeds,need=input$Q2_10Need,
                                      admin=input$Q2_13AdminSupr,trplan=input$Q2_13TrPlan,trprun=input$Q2_13TrPrun,trrem=input$Q2_13TrRem,othr=input$Q2_13Othr,
                                      trboard=input$Q3_2AuthTrBd,wrtstgpln=input$Q3_9WrtStgPln,stgplnupdt=input$Q3_9Updt,ordn=input$Q3_5MuniOrd,ordnUpdt=input$Q3_5YrUpdt,
                                      trinv=input$Q6_1TrInv,trinvUpdt=input$Q6_3YrUpd,acp=input$Q1_3AcMMP,acgs=input$Q1_3AcMGS,acmpwt=input$Q1_3AcMPWTr,
                                      havcg=input$Q6_15CanGl,curcn=input$Q6_16CurCn,cangl=input$Q6_16CanGl,glyr=input$Q6_16YrToGl,
                                      strtr=input$Q6_19StrTr,strtrp=input$Q6_20StrTr,prktr=input$Q6_19PrkTr,prktrp=input$Q6_20PrkTr,munitr=input$Q6_19MuniTr,munitrp=input$Q6_20MuniTr,miles=input$Q1_3MiMMS,
                                      cipc=input$Q7_4CurCyc,dipc=input$Q7_4DesCyc,active=input$Q7_2SysSch,prun=input$Q7_4AppPrun),
                          envir=new.env(parent=globalenv()))
      }
    )
    
    output$indexReport<-downloadHandler(
      filename = "indexReport.pdf",
      content=function(file){
        rmarkdown::render("Index.Rmd",output_file=file,
                          params=list(city=input$city,tCity=input$tCity,region=input$region,budget=input$Q2_3TotTreBud,pop=input$pop,muniOrd=input$Q3_5MuniOrd,trBrd=input$Q3_2AuthTrBd,trBrdList=input$Q3_1,arbDay=input$Q8_10ObvsP,
                                      plantOrd=input$Q3_61,protOrd=input$Q3_62,maintOrd=input$Q3_63,inventory=input$Q6_1TrInv,wrtStgPln=input$Q3_9WrtStgPln,
                                      staffCred=input$Q1_13,award=input$Q8_4AwdCm,contractors=input$Q5_4,standards=input$Q3_13,
                                      t1=tableScores[1],t2=tableScores[2],t3=tableScores[3],t4=tableScores[4],t5=tableScores[5],t6=tableScores[6],t7=tableScores[7],
                                      assessmentCount=input$Q6_6,trRskM=input$Q7_12TrRskM,writTrRsk=input$Q7_14WritTrRsk,inspStrat=input$Q7_15,staff=input$Q1_6OvrcTr,planDet=input$Q3_10,budmet=input$Q2_10CurrNeeds,active=input$Q7_2SysSch,recTree=input$Q7_17,
                                      colMethod=input$Q6_7,natVeg=input$QintNatVeg),
                          envir=new.env(parent=globalenv()))
      }
    )
  }
  
