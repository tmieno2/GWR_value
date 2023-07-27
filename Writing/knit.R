rmarkdown::render(here::here("GitControlled/Writing/JAAEA/1stRoundRevision/reply_to_editor.rmd"))

pagedown::chrome_print(here::here("GitControlled/Writing/JAAEA/1stRoundRevision/reply_to_editor.html"))

rmarkdown::render(here::here("GitControlled/Writing/JAAEA/1stRoundRevision/reply_to_reviewer_1.rmd"))

pagedown::chrome_print(here::here("GitControlled/Writing/JAAEA/1stRoundRevision/reply_to_reviewer_1.html"))

rmarkdown::render(here::here("GitControlled/Writing/JAAEA/1stRoundRevision/reply_to_reviewer_2.rmd"))

pagedown::chrome_print(here::here("GitControlled/Writing/JAAEA/1stRoundRevision/reply_to_reviewer_2.html"))


