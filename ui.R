source('tabs.R')

my_ui <- shinyUI({
  fluidPage(
    tabsetPanel(
      draft_board_tab,
      roster_tab
    )
  )
})