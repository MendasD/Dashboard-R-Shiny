call_groq_api_function <- function(history) {
  api_key <- Sys.getenv("GROQ_API_KEY")
  if (identical(api_key, "")) {
    stop("Merci de définir la variable d'environnement GROQ_API_KEY avec votre clé Groq.")
  }
  
  resp <- httr::POST(
    url = "https://api.groq.com/openai/v1/chat/completions",
    httr::add_headers(
      Authorization = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ),
    body = jsonlite::toJSON(
      list(
        model       = "llama3-8b-8192",
        messages    = history,
        temperature = 0.7
      ),
      auto_unbox = TRUE
    ),
    encode = "json"
  )
  
  if (httr::status_code(resp) != 200) {
    return("Erreur lors de l’appel à l’API Groq.")
  }
  
  parsed <- httr::content(resp, "parsed", encoding = "UTF-8")
  parsed$choices[[1]]$message$content
}

format_markdown_console <- function(md_text) {
  md_text <- gsub("^#+\\s*(.*)", toupper("\\1"), md_text)
  md_text <- gsub("\\*\\*(.*?)\\*\\*", toupper("\\1"), md_text)
  md_text <- gsub("\\*(.*?)\\*", "\\1", md_text)
  md_text <- gsub("^\\s*\\-\\s*", "• ", md_text)
  md_text <- gsub("```", "", md_text)
  md_text <- gsub("`([^`]*)`", "'\\1'", md_text)
  gsub("\n{2,}", "\n", md_text)
}


get_api <- function(modele_ia){
  if(modele_ia == "Groq"){
    return("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")
  }else{
    return("sk-9fc3a00ae92545cc980196cd9c258d67")
  }
}
