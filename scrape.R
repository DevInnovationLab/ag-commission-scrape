# User input -------------------------------------------------------------------

variable_terms <- 
  c(
    "%22Rural%22+AND+%22Nutrition%22+AND+%22Education%22+AND+%22Campaign%22+AND+%22Food+Consumption+Score%22",
    "%22Rural%22+AND+%22Nutrition%22+AND+%22Education%22+AND+%22Campaign%22+AND+%22Agriculture%22+AND+%22Productivity%22"
  )


main_url       <- "https://cgspace.cgiar.org/discover?rpp=10&etal=0&query="
fixed_terms    <- "+AND+%28%E2%80%9Cintervention%E2%80%9D+OR+%E2%80%9Cevaluation%E2%80%9D+OR+%E2%80%9Ctrial%E2%80%9D+OR+%E2%80%9Cimpact%E2%80%9D+OR+%E2%80%9Cexperiment%E2%80%9D%29&scope=/&group_by=none&page="
filters        <- "&filtertype_0=dateIssued&filtertype_1=type&filtertype_2=iso&filter_relational_operator_1=contains&filter_relational_operator_0=contains&filter_2=en&filter_1=Journal+Article&filter_relational_operator_2=contains&filter_0=%5B2000+TO+2023%5D"

# Packages ---------------------------------------------------------------------

library("rvest")
library("tidyverse")
library("here")
library("assertthat")

# Functions --------------------------------------------------------------------

read_url <-
  function(variable_terms, page_no) {
    paste0(
      main_url,
      variable_terms,
      fixed_terms,
      page_no,
      filters
    ) %>%
      read_html()
  }

count_entries <-
  function(term, term_number) {
    
    page <- read_url(term, 1)
    
    n_entries <- 
      page %>%
      html_nodes(".pagination-info") %>%
      html_text %>%
      str_remove("Now showing items 1-10 of ") %>%
      as.integer()
    
    if (length(n_entries) == 0) {
      
      print(
        paste0(
          "There are no entries for search parameters #",
          term_number
        )
      )
      
      return(NULL)
      
    } else {
      
      print(
        paste0(
          "There are ",
          n_entries,
          " entries for search parameters #",
          term_number
        )
      )
      return(n_entries)
    }
  }

list_papers <-
  function(page) {
    
    # Get list of all URLs
    description_info <-
      page %>%
      html_nodes("a.description-info")
    
    papers_url <-
      data.frame(
        title = html_text(description_info) %>% str_squish(),
        url = html_attr(description_info, "href")
      )
    
    # Get list of all papers
    papers_list <-
      page %>%
      html_nodes("div.row.ds-artifact-item") %>% 
      html_text() %>%
      str_squish()
    
    papers_list %>%
      data.frame(
        title = str_extract(papers_list, "Title:\\s*(.*?)\\s*Authors:"),
        authors = str_extract(papers_list, "Authors:\\s*(.*?)\\s*Date:"),
        date = str_extract(papers_list, "Date:\\s*(.*?)\\s*Type:"),
        type = str_extract(papers_list, "Type:\\s*(.*?)\\s*Status:"),
        status = str_extract(papers_list, "Status:\\s*(.*)")
      ) %>%
      select(-".") %>%
      mutate(
        across(
          everything(),
          ~ . %>% 
            str_remove_all(
              paste(
                c(
                  "Title:", 
                  "Authors:",
                  "Date:",
                  "Type:",
                  "Status:"
                ),
                collapse = "|"
              )
            ) %>%
            str_trim
        ),
        title = str_sub(title, end = -3)
      )  %>%
      left_join(
        papers_url,
        by = "title"
      ) %>%
      mutate(url = paste0("https://cgspace.cgiar.org", url))
  }

extract_details <-
  function(content, string) {
    
    if (any(str_detect(content, string))) {
      content %>% 
        pluck(which(str_detect(., string))) %>% 
        str_remove(string) %>% 
        str_trim %>%
        str_replace_all("’", "'") %>%
        str_replace_all("‘", "'") %>%
        str_replace_all("–", "-") %>%
        str_remove_all(" ") %>%
        str_replace_all("“", '"') %>%
        str_replace_all("”", '"') %>%
        str_replace_all("‐", "-")
        
    } else {
      NA
    }
  }

get_paper_details <-
  function(paper_url) {
    
    content <- 
      read_html(paper_url) %>%
      html_nodes(".simple-item-view-description") %>%
      html_text
    
    data.frame(
      "citation" = extract_details(content, "Citation"),
      "abstract" = extract_details(content, "Abstract/Description"),
      "url" = paper_url
    )
  }

# Scrape -----------------------------------------------------------------------

for (terms in 1:length(variable_terms)) {
  
  term      <- variable_terms[terms]
  n_entries <- count_entries(term, terms)
  
  # Only extract results if there are any
  if (!is.null(n_entries)) {
    
    n_pages         <- ceiling(n_entries/10)
    
    # Loop through all pages to get list of papers
    for (page_no in 1:n_pages) {
      search_results  <- read_url(term, page_no)
      papers_new      <- list_papers(search_results)
      
      if (exists("papers")) {
        papers <- bind_rows(papers, papers_new)
      } else {
        papers <- papers_new
      }
    }
    
    papers <-
      unique(papers)
    
    # Check that we have the right number of entries
    validate_that(
      nrow(papers) == n_entries,
      msg = paste0(
        "Something went wrong when looking for search parameters #",
        terms,
        ": the page lists ",
        n_entries,
        " papers in the search, but only ",
        nrow(papers),
        " were included in the data set"
      )
    )
    
    # Get abstracts
    print(
      paste0(
        "Retrieving abstracts for search parameters #",
        terms
      )
    )
    
    abstracts <-
      map(
        papers$url,
        get_paper_details
      ) %>%
      bind_rows
    
    # Combine abstracts and paper info
    papers <-
      left_join(
        papers,
        abstracts,
        by = "url"
      ) %>%
      mutate(
        across(
          c(title, authors, abstract, citation),
          ~ . %>% iconv(to = 'ASCII//TRANSLIT')
        )
      )
    
    # Save resulting data set
    write_csv(
      papers,
      here(
        "data",
        paste0("search_params", terms, ".csv")
      ),
      na = ""
    )
    
    # Start a new data frame for next search
    rm(papers, abstracts, term, n_entries, n_pages, papers_new, search_results)
  }
}

