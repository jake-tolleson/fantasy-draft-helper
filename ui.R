source('tabs.R')

ui <- shinyUI({
  fluidPage(
    tabsetPanel(
      draft_board_tab(),
      roster_tab()
    )
  )
})
