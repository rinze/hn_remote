library(httr)
library(logger)
library(lubridate)
library(rvest)
library(stringr)
library(tidyverse)

theme_set(theme_bw(14))
log_threshold(DEBUG)

INITIAL_URL <- 'https://news.ycombinator.com/submitted?id=whoishiring'
HN_URL <- 'https://news.ycombinator.com'
START_DATE <- '2019-01-01'
END_DATE <- '2022-03-01'
CRAWL_DELAY <- 30 # according to https://news.ycombinator.com/robots.txt
USER_AGENT <- 'https://rinzewind.org; remote-work-parser'

httr::set_config(httr::user_agent(USER_AGENT))

local_read_html <- function(url, delay = CRAWL_DELAY, ...) {
    Sys.sleep(delay)
    return(read_html(url, ...))
}

has_more <- function(parsed_html) {
    # Checks if there's a "More" link at the bottom of the page. If so,
    # it also returns the next URL to follow. This function assumes there's 
    # either one "morelink" or none.
    
    next_page <- parsed_html %>% html_elements(".morelink") %>% html_attr("href")
    has_more <- length(next_page) > 0
    return(list(has_more = has_more, next_page = file.path(HN_URL, next_page)))
}

find_links <- function(start_date, end_date) {
    # Returns the links to the monthly posts between `start_date` and `end_date`, included.
    
    keep_reading <- TRUE
    url <- INITIAL_URL
    result <- data.frame()
    my_session <- session(url)
    while (keep_reading) {
        log_info("Parsing {url}")
        parsed_html <- local_read_html(my_session)
        parsed_post_df <- parse_post_list(parsed_html, start_date, end_date)
        result <- rbind(result, parsed_post_df)
        # Do we have all dates we want?
        min_date <- min(result$date)
        max_date <- max(result$date)
        log_debug("Have data from {min_date} to {max_date}")
        if (min_date == start_date && max_date == end_date) {
            keep_reading <- FALSE
        } else {
            hm <- has_more(parsed_html)
            if (hm$has_more) {
                my_session <- session_jump_to(my_session, hm$next_page)
            } else {
                log_warnings("Only have data between {min_date} and {max_date}, but reached the end of the post list")
                keep_reading <- FALSE
            }
        }
    }
    
    return(result)
}

parse_date_from_title <- function(title) {
    # I feel this could be a bit better, but hey.
    
    matches <- regexpr("\\(([\\w \\d]+)\\)", title, perl = TRUE)
    date_str <- regmatches(title, matches)
    dates <- as.Date(parse_date_time(date_str, "(%b %Y)"))
    return(dates)
}

parse_post_list <- function(parsed_html, start_date, end_date) {
    # Get's "Who's hiring?" posts from the post list, along with the URL and the date.
    
    link_list <- file.path(HN_URL, parsed_html %>% html_elements(".titlelink") %>% html_attr("href"))
    title_list <- parsed_html %>% html_elements(".titlelink") %>% html_text()
    dd <- data.frame(link = link_list, title = title_list)
    
    # Get only hiring posts
    dd <- dd[grepl("Ask HN: Who is hiring? (", dd$title, fixed = TRUE), ]
    
    # Parse date and filter
    dd$date <- parse_date_from_title(dd$title)
    dd <- dd[dd$date >= start_date & dd$date <= end_date,]
    
    log_debug("Got {nrow(dd)} lines, min date = {min(dd$date)}, max date = {max(dd$date)}")
    return(dd)
}

parse_post_page <- function(parsed_html) {
    # Parses a single page from the 'Who's hiring?' post.
    first_lines <- parsed_html %>% 
        html_elements(xpath = '//tr[count(td[@indent=0]) = 1]/td[3]/div[@class="comment"]/span') %>%
        html_text2() %>%
        str_trim() %>%
        str_to_lower()
    
    first_lines <- sapply(first_lines, function(x) str_split(x, '\n')[[1]][1], simplify = TRUE)
    
    n_posts <- length(first_lines)
    n_remote <- sum(grepl('remote', first_lines))
    
    return(list(n_posts = n_posts, n_remote = n_remote))
}

parse_post <- function(url) {
    # Parses a given 'Who's hiring?' post. It paginates as needed.
    log_info("Parsing {url}")
    keep_reading <- TRUE
    n_posts <- 0
    n_remote <- 0
    n_page <- 1
    while (keep_reading) {
        parsed_html <- local_read_html(url)
        page_result <- parse_post_page(parsed_html)
        log_debug("\tParsed page {n_page}, got {page_result$n_posts} posts, {page_result$n_remote} remote")
        n_posts <- n_posts + page_result$n_posts
        n_remote <- n_remote + page_result$n_remote
        n_page <- n_page + 1
        # Should we keep paginating?
        hm <- has_more(parsed_html)
        keep_reading <- hm$has_more
        url <- hm$next_page
    }
    return(list(n_posts = n_posts, n_remote = n_remote))
}

#### MAIN ####

## Data gathering
log_info("Running for dates between {START_DATE} and {END_DATE}")
links_df <- find_links(START_DATE, END_DATE)

post_count <- apply(links_df, 1, function(x) {
    log_info("Running {x[2]}")
    return(parse_post(x[1]))
    
})

n_posts <- sapply(post_count, function(x) x[[1]][1])
n_remote <- sapply(post_count, function(x) x[[2]][1])

extracted_df <- data.frame(n_posts = n_posts,
    n_remote = n_remote,
    pct_remote = 100 * n_remote / n_posts,
    date = links_df$date)

## Plotting
plt1 <- ggplot(extracted_df, aes(x = date, y = pct_remote)) + 
    geom_line() +
    geom_point() +
    ylab("Percentage of posts [%]") +
    xlab("Date") +
    scale_y_continuous(limits = c(0, NA)) +
    ggtitle("Percentage of job postings on Hacker News with remote work as an option")
plot(plt1)
ggsave('/tmp/plt1.png', dpi = 100)

plt2 <- ggplot(pivot_longer(extracted_df, c(n_posts, n_remote), names_to = 'key', values_to = 'value'),
    aes(x = date, y = value, color = key)) +
    geom_line() +
    geom_point() +
    scale_color_brewer(palette = "Set1", labels = c("Total posts", "Remote posts"), name = NULL) +
    scale_y_continuous(limits = c(0, NA)) +
    ylab("Number of postings") +
    xlab("Date") +
    ggtitle("Raw number of job postings on Hacker News") +
    theme(legend.position = "top")
plot(plt2)
ggsave('/tmp/plt2.png', dpi = 100)
