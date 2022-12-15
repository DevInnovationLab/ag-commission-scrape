library("rvest")
library("tidyverse")
library("here")
library("assertthat")

main_url       <- "https://cgspace.cgiar.org/discover?rpp=10&etal=0&query="
variable_terms <- "%22Rural%22+AND+%22Nutrition%22+AND+%22Education%22+AND+%22Campaign%22+AND+%22Dietary+Diversity%22"
fixed_terms    <- "+AND+%28%E2%80%9Cintervention%E2%80%9D+OR+%E2%80%9Cevaluation%E2%80%9D+OR+%E2%80%9Ctrial%E2%80%9D+OR+%E2%80%9Cimpact%E2%80%9D+OR+%E2%80%9Cexperiment%E2%80%9D%29&scope=/&group_by=none&page="
filters        <- "&filtertype_0=dateIssued&filtertype_1=type&filtertype_2=iso&filter_relational_operator_1=contains&filter_relational_operator_0=contains&filter_2=en&filter_1=Journal+Article&filter_relational_operator_2=contains&filter_0=%5B2000+TO+2023%5D"

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

number_of_results <-
  function(page) {
    page %>%
      html_nodes(".pagination-info") %>%
      html_text %>%
      str_remove("Now showing items 1-10 of ") %>%
      as.integer()
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

get_paper_details <-
  function(paper_url) {
    paper_page <-
      read_html(paper_url)
    
    paper_page %>%
      html_nodes(".simple-item-view-description") %>%
      html_text %>%
      t %>%
      as.data.frame() %>%
      select(1:3) %>%
      transmute(
        citation = V1 %>%
          str_remove("\nCitation\n") %>%
          str_squish,
        abstract = V3 %>%
          str_remove("\nAbstract/Description\n") %>%
          str_squish,
        url = paper_url
      )
  }

# Scrape -----------------------------------------------------------------------

for (terms in 1:length(variable_terms)) {
  
  term <- variable_terms[terms]
  
  page          <- read_url(term, 1)
  n_results     <- number_of_results(page)
  n_pages       <- ceiling(n_results/10)
  papers        <- list_papers(page)
  
  if (n_pages > 1) {
    for (page_no in 2:n_pages) {
      page       <- read_url(term, page_no)
      papers_new <- list_papers(page)
      papers     <- bind_rows(papers, papers_new)
    }
    
  }
  
  # assert_that(
  #   nrow(papers) == n_results,
  #   error = paste(
  #     "Something went wrong: the page lists",
  #     n_results,
  #     "papers in the search, but only",
  #     nrow(papers),
  #     "are included in the data set"
  #   )
  # )
  # 
  abstracts <-
    map(
      papers$url,
      get_paper_details
    ) %>%
    bind_rows
  
  papers <-
    left_join(
      papers,
      abstracts,
      by = "url"
    ) %>%
    unique
  
  write_csv(
    papers,
    here(
      "data",
      paste0(
        "search_terms",
        terms,
        ".csv"
      )
    )
  )
  
}

