###############################################################################
#        Waseda University :: School of Political Science & Economics         #
#            - Political Text Analysis [2023 Spring Semester] -               #
#             Instructor: Rob Fahey <robfahey@aoni.waseda.jp>                 #
###############################################################################


# Week 07: APIs and Web Scraping
# ------------------------------

# The usual setup commands. Remember to change the folder location.

Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_GB.UTF-8")
setwd("~/Dropbox/Waseda/Political Text Analysis 2023/Week07 - Online")


# As ever, we'll use Tidyverse packages:
library(tidyverse)

# We also have new packages to use this week - rvest.

if(!require("rvest")) {install.packages("rvest"); library(rvest)}


#-----------------------------------------------------------------------------#
# 1.0   Scraping with rvest

# The rvest library contains commands that allow us to load a website's HTML
# file and search it for various tags and other objects.

# Let's start by loading a simple example page - the rvest sample page that
# includes a short list of information about Star Wars movies. We load the page
# with the read_html command.

sw_page <- read_html("https://rvest.tidyverse.org/articles/starwars.html")
sw_page

# This command has grabbed the HTML page from the Internet and stored it as an
# object. There are two items in the top-level list: the first is the section
# of the page enclosed by the <head></head> tags, which includes a lot of
# non-displayed information about the site. The second is the section enclosed
# by <body></body> tags - this is the actual "body" of the site which includes
# everything that's displayed in your browser.

# If you want to inspect the HTML code you've just downloaded, you can force
# it to appear as a list using this command:

xml2::as_list(sw_page)

# Needless to say that's... not very helpful. You're much better off looking at
# the HTML code in your browser while you work.


# 1.1   Selecting HTML Tags
#--------------------------

# Looking at the code, we can see that the entry for each movie is surrounded
# by the HTML tags <section></section>. We can use this to write a selector - 
# a command that's going to grab every piece of data that matches a certain
# pattern in the tags.

# This is done with the html_element() command.

sw_page %>%
  html_elements("section")

# Okay, now from the big cluttered page of HTML, we've extracted seven items
# matching the movies whose data we want to scrape.

# It looks like the title for each film is wrapped in <h2></h2> header tags.
# Let's extract those.

sw_page %>%
  html_elements("section") %>%
  html_element("h2")

# Notice that this time we're using html_element() - not html_elements(). 
# This distinction is important - the plural version returns a list of every
# tag that matches your pattern, the singular version just returns one tag.
# In this case, we're making a list of all the "sections", and then passing them
# one by one to html_element() to select the header from each of them.

# Now, we can just extract the titles using the html_text2() command, which
# strips off the HTML tags and just leaves us with nice neat text...

sw_page %>%
  html_elements("section") %>%
  html_element("h2") %>%
  html_text2()

# But notice that the sequential number of the movies is also stored in this
# data - as a parameter to the <h2> tag, called "data-id". We can access tag
# parameters using html_attr().

sw_page %>%
  html_elements("section") %>%
  html_element("h2") %>%
  html_attr("data-id")


# 1.2   Selecting HTML Classes
#-----------------------------

# Looking at our HTML in the browser again, we can see that the rest of the 
# information about each film could be a little trickier to access. The name
# of the director is contained inside a <span></span> tag, with a property of 
# class="director", while the description text is contained inside a <div></div>
# tag, with a property of class="crawl".

# Luckily, we can select elements based not just on the name of the tag which
# encloses them, but also the "class" of the tag. In the html_element() command,
# the class is indicated with a . in front of the name.

sw_page %>%
  html_elements("section") %>%
  html_element(".director") %>%
  html_text2()

sw_page %>%
  html_elements("section") %>%
  html_element(".crawl") %>%
  html_text2()

# Even though these items are contained inside different tags - <span> and <div>
# respectively - we can access them both by referring to their tag class, with
# a dot in front of the name to tell R that we mean the class, not the tag name.

# How about the release date, though? It's contained within a <p></p> tag,
# but it doesn't have a class name. However, it's the first <p> tag that appears
# for each movie, so we can just the html_element() (singular) command, which
# only returns one tag - the first one it encounters

sw_page %>%
  html_elements("section") %>%
  html_element("p") %>%
  html_text2()

# We can get rid of the "Released: " bit that we don't care about, and then turn
# this into an actual date object in R, so we could do a time series analysis
# or order the movies by release date, if we wanted:

sw_page %>%
  html_elements("section") %>%
  html_element("p") %>%
  html_text2() %>%
  str_remove("Released: ") %>%
  parse_date()


# 1.3   Putting it Together
#--------------------------

# We've now figured out how to individually extract each of these pieces of
# data about the movies. Let's put it all together into a table in R.

sw_movies <- tibble(
  Number = sw_page %>% 
    html_elements("section") %>% 
    html_element("h2") %>%
    html_attr("data-id"),
  Title = sw_page %>% 
    html_elements("section") %>% 
    html_element("h2") %>%
    html_text2(),
  Director = sw_page %>%
    html_elements("section") %>%
    html_element(".director") %>%
    html_text2(),
  ReleaseDate = sw_page %>%
    html_elements("section") %>%
    html_element("p") %>%
    html_text2() %>%
    str_remove("Released: ") %>%
    parse_date(),
  CrawlText = sw_page %>%
    html_elements("section") %>%
    html_element(".crawl") %>%
    html_text2()
)

sw_movies

# And there we go - it's just an example page, but we've gone from messy HTML
# to a table of data you can actually read in R.

# Of course we can also save this as a CSV file, so we can re-import it and use
# it in Quanteda, or do anything else we like:

write_csv(sw_movies, file = "starwars.csv")


#-----------------------------------------------------------------------------#
# 2.0   A Real-World Example


# Let's try working on a real website. We're going to scrape all of the user
# reviews for Morbius, because I suffered through this movie and I want to be
# certain that other people shared my pain.

morb <- read_html("https://www.imdb.com/title/tt5108870/reviews/?ref_=tt_ql_2")

# Okay, looking at the code for this page, it's much more complex than the
# previous example. However, looking closely suggests that every user review
# on the page is surrounded with a <div class="review-container"> tag. Let's
# check and see if that works.

# We can combine the tag name and its associated class in the selector by
# separating them with a dot.

morb %>%
  html_elements("div.review-container")

# Okay, that pulls out 25 items from the page, which seems about right.
# Now, some elements here seem simple. The title of each review is inside
# an <a> tag with a class of "title":

morb %>%
  html_elements("div.review-container") %>%
  html_element("a.title") %>%
  html_text2()

# Meanwhile, the review text itself is contained inside some nested <div>
# tags. There's a <div class="content"> which then contains <div class="text">,
# and the latter may also have some other classes (class names are separated by
# spaces, and we can search for them individually - you don't need to know the
# full list of classes a certain tag has.)

# We can actually search for nested tags as well - separating the search items
# with a space says "find the first item, then find the second item within it".

morb %>%
  html_elements("div.review-container") %>%
  html_element("div.content div.text") %>%
  html_text2()

# Similarly, if you look at the usernames of the review authors, we can see 
# that they're inside an <a> tag which has no class attached - that's a problem
# as there are tons of <a> tags on this page. However, outside that <a> tag is
# a <span class="display-name-link"> tag, so we can search for this in a nested
# way.

morb %>%
  html_elements("div.review-container") %>%
  html_element("span.display-name-link a") %>%
  html_text2()


# 2.1   Working with Dates
#-------------------------

# In this page, the date of each review is contained in a <span> tag with a 
# class of "review-date".

morb %>%
  html_elements("div.review-container") %>%
  html_element("span.review-date") %>%
  html_text2()

# ... Which is fine, but trying to read those entries as regular dates will
# raise an error:

morb %>%
  html_elements("div.review-container") %>%
  html_element("span.review-date") %>%
  html_text2() %>%
  parse_date()

# Our best bet here is to use a package called Lubridate, which is a part of the
# tidyverse software and helps to make R's handling of dates much smarter.

# It has a set of functions: dmy(), ymd(), mdy(), ydm() - each of these just 
# tells R "this date is in the order Day, Month, Year" (for dmy() - you can
# guess the rest) and lets it figure out the rest of the date format itself.

morb %>%
  html_elements("div.review-container") %>%
  html_element("span.review-date") %>%
  html_text2() %>%
  dmy()


# 2.2   Putting it Together
#--------------------------

# One final piece of data we'd like to have: the score each user gave.

# This is a little tricky to find, but in the HTML we can see it in a format
# like this:
# <span>3</span><span class="point-scale">/10</span>

# It seems to be enclosed in a <span class="rating-other-user-rating"> tag.

morb %>%
  html_elements("div.review-container") %>%
  html_element("span.rating-other-user-rating span") %>%
  html_text2()

# Note that those numbers are surrounded by quote marks. This is because R
# doesn't know they're numeric data, and is treating them as text. We can 
# convert them into numeric data with the parse_integer() command.

morb %>%
  html_elements("div.review-container") %>%
  html_element("span.rating-other-user-rating span") %>%
  html_text2() %>%
  parse_integer()


# Okay, lets write our command to put these into a table!

morb_reviews <- tibble(
  Username = morb %>%
    html_elements("div.review-container") %>%
    html_element("span.display-name-link a") %>%
    html_text2(),
  ReviewDate = morb %>%
    html_elements("div.review-container") %>%
    html_element("span.review-date") %>%
    html_text2() %>%
    dmy(),
  Score = morb %>%
    html_elements("div.review-container") %>%
    html_element("span.rating-other-user-rating span") %>%
    html_text2() %>%
    parse_integer(),
  ReviewTitle = morb %>%
    html_elements("div.review-container") %>%
    html_element("a.title") %>%
    html_text2(),
  ReviewText = morb %>%
    html_elements("div.review-container") %>%
    html_element("div.content div.text") %>%
    html_text2()
)

morb_reviews

# And there we have it - reviews of Morbius converted from a messy HTML
# webpage into a readable table in R.

# But we've only got 25 reviews. And far more than 25 people have reviewed this
# movie. The problem is that IMDB's reviews pages are dynamic - there is a 
# "load more" button at the bottom of the page, which will bring in more data.

# We've just downloaded the HTML file as a text file - we have no way to click
# the button!


#-----------------------------------------------------------------------------#
# 3.0   Loading More Pages

# This is where things get complex. Every website has its own system for
# loading in more data. On some sites, you just add "?page=2" to the URL to get
# the next page - you could easily automate that in R just by writing a loop.

# On this site, though, the "Load More" button doesn't open a new page - it 
# streams additional data to this page.


# Let's look at how we'd solve this puzzle. Bear in mind that this will only
# work for this one specific site (IMDB) - it's not a blanket solution for
# the problem, as each site is its own puzzle to solve.

# First, look at what happens when you actually click the Load More button.
# Your browser's Developer Tools probably has a Network section which shows
# network activity from the page. When you click "Load More", a new entry
# appears - this is the network connection being used to load in more reviews.

# If you check the details, you'll see that it's getting more data from here:
# https://www.imdb.com/title/tt5108870/reviews/_ajax

# What happens if we open that page in rvest?

morb2 <- read_html("https://www.imdb.com/title/tt5108870/reviews/_ajax")
morb2

# Okay, it's some HTML... Can we parse it in the same way as we were doing
# previously?

morb2 %>%
  html_elements("div.review-container") %>%
  html_element("a.title") %>%
  html_text2()

# Yes! So our code from previously is working. But wait... These are the same
# reviews we already saw. We must need to do something else in order to tell
# the site to give us the next page of reviews instead.

# Look again at the Network information we got when we clicked the link. 
# There is a hint under "payload" (this refers to the data which was sent to
# the site in order to get this new information): something was sent called a
# "paginationKey". It's a huge string of characters. Where did it come from?

# Check the HTML, and we can see that down at the bottom of the page, there was
# the code for the Load More button. It starts with a <div class="load-more-data">
# entry, and in there is an attribute called "data-key"...

morb %>%
  html_element("div.load-more-data") %>%
  html_attr("data-key")

# There it is! Okay, so can we somehow add that to our URL and get the second
# page of data?

page_key <- morb %>%
  html_element("div.load-more-data") %>%
  html_attr("data-key")

morb2 <- read_html(
  paste0("https://www.imdb.com/title/tt5108870/reviews/_ajax?paginationKey=",
         page_key)
  )

morb2

morb2 %>%
  html_elements("div.review-container") %>%
  html_element("a.title") %>%
  html_text2()

# These are all new headlines! morb2 contains the second page of data, as we
# wanted.

# Moreover, if we look at the data we just downloaded into morb2, it contains
# a new key as well - this will be the page-key we can use for Page 3.

morb2 %>%
  html_element("div.load-more-data") %>%
  html_attr("data-key")

# You could continue stepping through the pages in an R loop, extracting the
# reviews and then loading the next page with your new page key, until you
# get all of the reviews of Morbius on the site.

# Below I've written some code which will do exactly this. You don't need to run
# this code (be warned that it will take several minutes to load all the data!)
# but you might find it useful to refer to if you choose to write your own
# web scraping script.

page_key <- ""
reached_end <- FALSE
morb_reviews <- 
  morb_reviews <- tibble(
    Username = character(),
    ReviewDate = Date(),
    Score = numeric(),
    ReviewTitle = character(),
    ReviewText = character()
  )

while (reached_end == FALSE) {
  # Grab the next page (or the first page)
  print(paste0("Scraping for Key: ", page_key))
  morb_page <- read_html(
    paste0("https://www.imdb.com/title/tt5108870/reviews/_ajax?paginationKey=",
           page_key))
  # Extract the reviews from this page into a Tibble
  new_reviews <- tibble(
    Username = morb_page %>%
      html_elements("div.review-container") %>%
      html_element("span.display-name-link a") %>%
      html_text2(),
    ReviewDate = morb_page %>%
      html_elements("div.review-container") %>%
      html_element("span.review-date") %>%
      html_text2() %>%
      dmy(),
    Score = morb_page %>%
      html_elements("div.review-container") %>%
      html_element("span.rating-other-user-rating span") %>%
      html_text2() %>%
      parse_integer(),
    ReviewTitle = morb_page %>%
      html_elements("div.review-container") %>%
      html_element("a.title") %>%
      html_text2(),
    ReviewText = morb_page %>%
      html_elements("div.review-container") %>%
      html_element("div.content div.text") %>%
      html_text2()
  )
  # Add the new reviews to the existing set of reviews
  morb_reviews <- bind_rows(morb_reviews, new_reviews)
  # Check if there is a div.load-more-data item on this page
  # of data. If it doesn't exist, we're on the last page and
  # should quit out of the loop. If it does, we should extract
  # the new paginationKey and continue the loop.
  moreData <- morb_page %>% html_element("div.load-more-data")
  if (is.na(moreData) == FALSE) {
    page_key <- moreData %>% html_attr("data-key")
  } else {
    reached_end <- TRUE
  }
}

morb_reviews

