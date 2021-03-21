tk_grid_pack_tools <- function(tourBar, refreshButton, scaleRadioButtons,
                               onStepUpButton, onStepDownButton,
                               parent = NULL, pack = TRUE, by = NULL,
                               row = 20, column = 20) {

  tcltk::tkgrid(onStepUpButton,
                row = 0,
                column = column,
                rowspan = 1,
                columnspan = 1,
                sticky="nesw")

  tcltk::tkgrid(onStepDownButton,
                row = row,
                column = column,
                rowspan = 1,
                columnspan = 1,
                sticky="nesw")

  # pack the bar
  tcltk::tkgrid(tourBar,
                row = 1,
                column = column,
                rowspan = row - 1,
                columnspan = 1,
                sticky="nesw")

  # pack the refresh button
  tcltk::tkgrid(refreshButton,
                row = row,
                column = 0,
                rowspan = 1,
                columnspan = 1,
                sticky="nesw")

  num_of_radiobuttons <- length(scaleRadioButtons)
  buttonspan <- floor(column/num_of_radiobuttons)

  # pack the radio_button
  lapply(scaleRadioButtons,
         function(scale_radio_button) {

           tcltk::tkgrid(scale_radio_button,
                         row = row,
                         column = buttonspan * (which(scaleRadioButtons %in% scale_radio_button) - 1) + 1,
                         rowspan = 1,
                         columnspan = buttonspan,
                         sticky="w")
         })

  if(is.null(by)) {
    for(i in (seq(row - 1))) {
      tcltk::tkgrid.rowconfigure(parent, i, weight=1)
    }
    for(i in (seq(column) - 1)) {
      tcltk::tkgrid.columnconfigure(parent, i, weight=1)
    }
  }

  if(pack)
    tcltk::tkpack(parent, fill = "both", expand = TRUE)
}
