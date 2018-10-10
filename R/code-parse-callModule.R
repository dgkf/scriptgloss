expand_callModule_calls <- function(x, session, envir = parent.frame()) {
  if (is.call(x)) {
    if (x[[1]] == "callModule") {
      message("found a callModule call")
      process_callModule_call(x, session, envir = envir)
    }
      
    as.call(lapply(x, expand_callModule_calls, session=session, envir=envir))
  
  } else if (is.expression(x)) 
    as.expression(expand_callModule_calls(x[[1]], session=session, envir=envir))
  else if (is.atomic(x) || is.name(x)) 
    x
  else if (is.pairlist(x)) 
    as.pairlist(lapply(x, expand_callModule_calls, session=session, envir=envir))
  else if (is.list(x))
    lapply(x, expand_callModule_calls, session=session, envir=envir)
  else stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  
  return(x)
}

process_callModule_call <- function(cm, session, envir = parent.frame()) {
  cm <- match.call(shiny::callModule, cm)
  cm_args <- as.list(cm)[-1]
  cm_args <- cm_args[which(!names(cm_args) %in% names(formals(shiny::callModule)))]
  
  module_srv <- eval(cm$module, envir = envir)
  module_id <- eval(cm$id, envir = envir)
  module_scope <<- session$makeScope(module_id)
  module_inputs <- reactiveValuesToList(module_scope$input)
  
  module_srv_args <- c(cm_args, list(
    input = module_scope$input,
    output = module_scope$output,
    session = module_scope))
  
  module_srv_call <- do.call("call", c("module_srv", module_srv_args))
  module_srv_args <- as.list(match.call(module_srv, module_srv_call))[-1]
  
  cm_envir <- as.environment(module_srv_args)
  parent.env(cm_envir) <- envir
  
  cm_x <- generate_static_code(module_srv, envir = cm_envir, files = list())
    
  message(paste(cm_x, collapse = "\n"))
  
  # 1. build environment in which module is being evaluated
  # 2. use generate_static_code() to build module code

}