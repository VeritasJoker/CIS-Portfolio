

shinyServer(function(input, output){

  # observeEvent(input$starting == "input", {
  #   shinyjs::toggle(id = "starting_condition")
  # })
  
  output$proposal <- renderUI({
    tags$iframe(style="height:600px; width:100%",
                src= "KenWang21-ComputationalNeuroscience.pdf")
  })
  
  output$presentation <- renderUI({
    tags$iframe(style="height:600px; width:100%", src= "presentation.pdf")
  })
  
  output$input_condition <- renderUI({
    pickerInput( # column selection for patient treatment studies
      inputId = "input_cond", # input id
      label = "Input Condition",  # label
      choices = seq(1,input$number), # choices
      selected = NULL,
      options = list(
        `actions-box` = TRUE, 
        size = 10, # size of the selection box
        `selected-text-format` = "count > 2" # how to show selected
      ),
      multiple = TRUE # allow multiple selections
    )
  })
  
  mat = reactive({
    zero_matrix = matrix(rep(0,input$number*input$number), input$number, input$number)
    rownames(zero_matrix) = paste("Cell ",seq(1,input$number),sep = "")
    colnames(zero_matrix) = paste("Cell ",seq(1,input$number),sep = "")
    cell_type = rep(1,input$number)
    if(input$cell_type == "Inhibitory"){
      cell_type = rep(-1,input$number)
    }
    else if(input$cell_type == "Both"){
      for(i in 1:length(cell_type)){
        if(runif(n=1,min=0,max=1) < 0.5){
          cell_type[i] = -1
        }
      }
    }
    for(i in 1:length(cell_type)){
      for(j in 1:ncol(zero_matrix)){
        if(runif(n=1,min=0,max=1) < 0.5){
          zero_matrix[i,j] = cell_type[i]
        }
      }
    }
    return(zero_matrix)
  })
  
  start_vect = reactive({
    zero_vect = rep(0, input$number)
    if(input$starting_cond == "random"){
      for(i in 1:length(zero_vect)){
        if(runif(n=1,min=0,max=1) < 0.5){
          zero_vect[i] = 1
        }
      }
    }
    else{
      if(length(input$input_cond) > 0){
        on_cells = as.integer(input$input_cond)
        for(i in on_cells){
          zero_vect[i] = 1
        }
      }
    }
    return(zero_vect)
  })
  
  output$results = renderUI({
    matrixInput(inputId = "matinput",
                value = mat(),
                cols = list(
                  names = TRUE,
                  editableNames = FALSE
                ),
                rows = list(
                  names = TRUE,
                  editableNames = FALSE)
    )
  })
  
  observeEvent(input$matrix_toggle, {
    shinyjs::toggle(id = "matrix_display")
  })
  
  
  observeEvent(input$plot, {
    zero_vect = rep(0, input$number)
    if(input$starting_cond == "random"){
      for(i in 1:length(zero_vect)){
        if(runif(n=1,min=0,max=1) < 0.5){
          zero_vect[i] = 1
        }
      }
    }
    else{
      if(length(input$input_cond) > 0){
        on_cells = as.integer(input$input_cond)
        for(i in on_cells){
          zero_vect[i] = 1
        }
      }
    }
    
    results_final <- matrix(NA, nrow=input$duration, ncol=input$number)
    dt_final = data.frame(Timestep = integer(),
                          Neurons = integer())
    results_final[1,] = zero_vect
    for(j in 1:ncol(results_final)){
      if(results_final[1,j] == 1){
        dt_final[nrow(dt_final)+1,] = c(1,j)
      }
    }
    new_mat = input$matinput
    class(new_mat) = "numeric"
    for(i in 2:input$duration){
      results_final[i,] = results_final[i-1,] %*% new_mat
      for(j in 1:ncol(results_final)){
        if(results_final[i,j] >= input$thresh){
          results_final[i,j] = 1
          dt_final[nrow(dt_final)+1,] = c(i,j)
        }
        else{
          results_final[i,j] = 0
        }
      }
    }
    output$plot_results = renderPlotly({
      ggplot(data = dt_final)+
        geom_point(aes(x=Timestep,y=Neurons))
    })
  })
  
})


