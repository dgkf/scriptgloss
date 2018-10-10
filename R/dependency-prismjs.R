#' PrismJS available language catalog
prismjs_languages <- c('abap', 'actionscript', 'ada', 'apacheconf',
  'apl', 'applescript', 'arduino', 'asciidoc', 'aspnet', 'autohotkey',
  'autoit', 'bash', 'basic', 'batch', 'bison', 'brainfuck', 'bro', 'c',
  'clike', 'coffeescript', 'core', 'cpp', 'crystal', 'csharp', 'css-extras',
  'css', 'd', 'dart', 'diff', 'django', 'docker', 'eiffel', 'elixir',
  'erlang', 'fortran', 'fsharp', 'gherkin', 'git', 'glsl', 'go', 'graphql',
  'groovy', 'haml', 'handlebars', 'haskell', 'haxe', 'http', 'icon',
  'inform7', 'ini', 'j', 'java', 'javascript', 'jolie', 'json', 'jsx',
  'julia', 'keyman', 'kotlin', 'latex', 'less', 'livescript', 'lolcode',
  'lua', 'makefile', 'markdown', 'markup', 'matlab', 'mel', 'mizar', 'monkey',
  'n4js', 'nasm', 'nginx', 'nim', 'nix', 'nsis', 'objectivec', 'ocaml',
  'opencl', 'oz', 'parigp', 'parser', 'pascal', 'perl', 'php-extras', 'php',
  'powershell', 'processing', 'prolog', 'properties', 'protobuf', 'pug',
  'puppet', 'pure', 'python', 'q', 'qore', 'r', 'reason', 'renpy', 'rest',
  'rip', 'roboconf', 'ruby', 'rust', 'sas', 'sass', 'scala', 'scheme', 'scss',
  'smalltalk', 'smarty', 'sql', 'stylus', 'swift', 'tcl', 'textile', 'twig',
  'typescript', 'vbnet', 'verilog', 'vhdl', 'vim', 'wiki', 'xojo', 'yaml')

#' PrismJS available themes catalog
prismjs_themes <- c('', 'coy', 'dark', 'funky', 'okaidia', 'solarizedlight', 
  'tomorrow', 'twilight')

#' PrismJS source tags
#'
#' @param language what programming language for syntax highlighting
#' @param theme what color theme to use
#'
#' @return a source header loading prism from cdn
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   prismDependencies('r', 'coy'),
#'   clipboardjsDependencies(),
#'   selectInput('x', 'x axis', choices = names(mtcars)),
#'   selectInput('y', 'y axis', choices = names(mtcars)),
#'   actionButton("show_code", "Show R Code", icon("code")),
#'   plotOutput('plot')
#' )
#' 
#' srv <- function(input, output, session) {
#'   output$plot <- renderPlot({
#'     plot(x = mtcars[[input$x]],
#'          y = mtcars[[input$y]])
#'   })
#'   observeEvent(input$show_code, {
#'     show_code_modal(srv)
#'   })
#' }
#' 
#' shinyApp(ui, srv)
#' }
#' 
#' @importFrom shiny singleton tagList tags
#' @export
#'
prismDependencies <- function(language = 'r', theme = 'coy') {
  language <- match.arg(language, prismjs_languages, several.ok = TRUE)
  theme <- match.arg(theme, prismjs_themes)

  if (theme != '') theme <- sprintf('-%s', theme)
    shiny::tagList(shiny::singleton(do.call(tags$head, c(list(
      shiny::tags$script(
        src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js"),
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = sprintf("https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism%s.min.css", theme))),
      lapply(language, function(lang) {
        shiny::tags$script(
          src = sprintf("https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-%s.min.js", lang))
      }) ))
  ))
}