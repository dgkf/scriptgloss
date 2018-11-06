# scriptgloss

Reconstruct static code from shiny apps

<p align="center">
<img src="https://user-images.githubusercontent.com/18220321/46750369-9f8aad00-cc6c-11e8-8852-eb0b37217c38.gif"/>
</p>


`scriptgloss` exposes functionality for building static code to recreate outputs
built in a shiny context. Construct something in shiny and generate the code
needed to produce it without needing to trudge through the app code.

# Installation

For now, due to a dependency on the `graph` package in Bioconductor by way of
`CodeDepends`, this package will fail to install via the typical `devtools`
mechanisms. Instead, please use `BiocManager` and follow any prompts to install
the package, pulling from Bioconductor when necessary. I'll be working to reduce
this impedence.

```
install.packages("BiocManager") # R (>3.5.0)
BiocManager::install("dgkf/scriptgloss")
```

>**Developer Note**  
>This was my first foray into advising installation via `BiocManager`. If you
[run into issues, please report
them](https://github.com/dgkf/scriptgloss/issues) and I'll work to make the
installation seamless until this dependency requirement has been resolved.

# Getting Started

## Start by building a `shiny` app

Let's say we're building a shiny app to explore the `mtcars` dataset. We might
start with something like this:

```r
library(shiny)

ui <- fluidPage(
  selectInput('x', 'x axis', choices = names(mtcars)),
  selectInput('y', 'y axis', choices = names(mtcars)),
  plotOutput('plot'))

srv <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(x = mtcars[[input$x]], y = mtcars[[input$y]], 
      xlab = input$x, ylab = input$y)
  })
}

shinyApp(ui, srv)
```

If you run the code above, you'll see that it's quite simple; just two drop down
menus to pick an x and y variable which will update a plot to compare the two.

## Adding code export elements

In order to add static code output, there are just a few small steps that need
to be taken. 

1. The JavaScript dependencies need to be added to the webpage header. This can
be done easily by adding the UI element, `scriptglossJS()`.
1. A UI button needs to be added to export the code. For this, `scriptgloss`
provides the functions `showCodeButton()` and `clipCodeButton()` for showing
code in a modal window or copying to clipboard respectively.
1. An observer of the UI button needs to be added to prepare the code.

>### Adding a "Show Code" button
>A minimal example showing how you would use the `showCodeButton()` UI element.
Not the most interesting code in the world, but operational!

```r
library(shiny)
library(scriptgloss)

ui <- fluidPage(
  scriptglossJS(),
  showCodeButton("show_code"))

srv <- function(input, output, session) {
  # ui observer to display a modal code window when the button is pressed
  observeEvent(input$show_code, show_code_modal(srv))
}

shinyApp(ui, srv)
```

>### Adding a clipboard button to copy code
>A minimal example of how you would add a button to copy static code to the
clipboard. Again, nothing too exciting, but hopefully enough to build it into
your own work.

```r
library(shiny)
library(scriptgloss)

ui <- fluidPage(
  scriptglossJS(),
  uiOutput('copy_code_btn'))

srv <- function(input, output, session) {
  # ui observer to copy code to the user's clipboard
  output$copy_code_btn <- renderUI(clipCodeButton(srv))
}

shinyApp(ui, srv)
```

## Reworking our `shiny` app to produce static code

Just by following the steps above, we can quickly add a button to show off our
code!

```r
library(shiny)
library(scriptgloss)

ui <- fluidPage(
  selectInput('x', 'x axis', choices = names(mtcars)),
  selectInput('y', 'y axis', choices = names(mtcars)),
  plotOutput('plot'),
  # add our UI elements ... 
  scriptglossJS(),              # <-- don't forget the JavaScript part!
  showCodeButton("show_code"),  # button to show code as a pop-up
  uiOutput("clip_code_btn"))    # button to copy to clipboard

srv <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(x = mtcars[[input$x]], y = mtcars[[input$y]], 
      xlab = input$x, ylab = input$y)
  })
  
  # observer for our modal button
  observeEvent(input$show_code, show_code_modal(srv, "plot"))
  
  # button renderer for our copy-to-clipboard button
  output$clip_code_btn <- renderUI(clipCodeButton(text = get_code(srv, "plot")))
}

shinyApp(ui, srv)
```

# Acknowledgements

Early prototypes developed at Genentech. Many thanks to my employer for their 
encouragement and accommodation in allowing me to release this work publicly.
