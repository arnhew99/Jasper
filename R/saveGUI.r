saveGUI <-
function (filename, type = "pdf", x=6,y=6) {
     if (type == "pdf") {
         dev.print(device = pdf, file = paste(filename, ".pdf", sep=""), onefile = FALSE, width = x, height = y, family = "Helvetica", title = "R Graphics Output")
     }
     return()
}
