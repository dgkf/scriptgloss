replace_callModule_calls <- function(x, session, envir = parent.frame()) {
  if (is.call(x))
    if (x[[1]] == quote(callModule) || x[[1]] == quote(shiny::callModule)) {
      module_meta <- get_callModule_metadata(x, session, envir=envir)
      x <- bquote(.(as.name(module_meta$id))$return)
    } else {
      x <- as.call(lapply(x, replace_callModule_calls, session=session, envir=envir))
    }
  else if (is.expression(x)) 
    x <- as.expression(replace_callModule_calls(x[[1]], session=session, envir=envir))
  else if (is.atomic(x) || is.name(x)) 
    x
  else if (is.pairlist(x)) 
    x <- as.pairlist(lapply(x, replace_callModule_calls, session=session, envir=envir))
  else if (is.list(x))
    x <- lapply(x, replace_callModule_calls, session=session, envir=envir)
  else stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  
  return(x)
}



collect_callModule_calls <- function(x, call_list = list()) {
  if (is.call(x))
    if (x[[1]] == quote(callModule) || x[[1]] == quote(shiny::callModule))
      call_list <- append(call_list, x)
    else
      call_list <- append(call_list, unlist(lapply(x, collect_callModule_calls)))
  else if (is.expression(x)) 
    call_list <- append(call_list, collect_callModule_calls(x[[1]]))
  else if (is.atomic(x) || is.name(x)) 
    x
  else if (is.pairlist(x)) 
    call_list <- append(call_list, unlist(lapply(x, collect_callModule_calls)))
  else if (is.list(x))
    call_list <- append(call_list, unlist(lapply(x, collect_callModule_calls)))
  else stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  
  return(call_list)
}



get_callModule_metadata <- function(cm, session, envir = parent.frame()) {
  # extract only non-callModule arguments without evaluating content in envir
  cm_args <- as.list(cm)[-1]
  cm_argidx <- as.list(setNames(seq_along(cm_args), names(cm_args)))
  cm_call <- do.call("call", c(as.character(cm[[1]]), cm_argidx))
  cm_call <- match.call(shiny::callModule, cm_call, envir = envir)
  cm_call_args <- as.list(cm_call)[-1]
  cm_args <- as.list(setNames(cm_args[unlist(cm_argidx)], names(cm_call_args)))
 
  module_srv    <- eval(cm_args$module, envir = envir)
  module_id     <- eval(cm_args$id, envir = envir)
  module_scope <<- session$makeScope(module_id)
  
  module_srv_args <- cm_args[which(!names(cm_args) %in% names(formals(shiny::callModule)))]
  module_srv_args <- c(module_srv_args, list(
    input   = getInitializationCode(reactiveValuesToList(module_scope$input)),
    output  = getInitializationCode(list()),
    session = getInitializationCode(NULL))) # module_scope
  
  module_srv_argidx <- as.list(setNames(seq_along(module_srv_args), names(module_srv_args)))
  module_srv_call <- do.call("call", c("module_srv", module_srv_argidx))
  module_srv_argidx <- as.list(match.call(module_srv, module_srv_call))[-1]
  module_srv_args <- as.list(setNames(module_srv_args[unlist(module_srv_argidx)], names(module_srv_argidx)))

  md <- structure(
    list(
      module = as.character(cm_args$module),
      id     = module_id,
      srv    = module_srv,
      args   = module_srv_args),
    class = "module_metadata")
  
  md <- process_callModule(md, session = session, envir = envir)
  
  md
}



process_callModule <- function(md, session, envir = parent.frame()) {
  # build environment in which module is being evaluated
  cm_envir <- as.environment(md$args)
  parent.env(cm_envir) <- envir
  
  # use generate_static_code() to build module code
  srv_body <- body(md$srv)
  srv_body <- generate_static_code(md$srv, envir = cm_envir, files = list(), 
      initialize_params = FALSE)
  
  # convert return statements from `return(...)` to `output$return <- ...` and 
  # assign last top level expression to output$return
  srv_body <- process_return_calls(srv_body)
  
  # append `return(output)` statement to function body
  if (is.call(i <- srv_body[[length(srv_body)]]) && i[[1]] == "<-")
    srv_body <- as.call(append(as.list(srv_body), quote(return(output))))
  
  body(md$srv) <- srv_body
  md
}



process_return_calls <- function(x, depth = 1) {
  if (is.call(x))
    if (x[[1]] == quote(return))
      x <- call("<-", quote(output$return), x[[2]])
    else
      x <- as.call(lapply(x, process_return_calls, depth = depth + 1))
  else if (is.expression(x)) 
    x <- as.expression(process_return_calls(x[[1]]))
  else if (is.atomic(x) || is.name(x)) 
    x 
  else if (is.pairlist(x)) 
    x <- as.pairlist(lapply(x, process_return_calls, depth = depth + 1))
  else if (is.list(x))
    x <- lapply(x, process_return_calls, depth = depth + 1)
  else stop("Don't know how to handle type ", typeof(x), call. = FALSE)
    
  return(x)
}
