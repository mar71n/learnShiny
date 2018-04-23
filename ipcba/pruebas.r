ipcba <- read.csv("../datasets/IPCBA_.csv", stringsAsFactors=FALSE)
# ipcba <- edit(ipcba)
    gipcba <- ipcba[which(ipcba$anno==2018),]
    gipcba <- ipcba[3:16,]
    gipcba <- tail(ipcba[ !is.na(ipcba[["ipcba"]]), ], n=12L)
    tt <- nrow(gipcba)
    gipcba["ipcba_ant"] <- gipcba[["ipcba"]]/(1 + gipcba[["mensual"]] / 100)
    gipcba["ipcba_ini"] <- rep(gipcba[1,"ipcba_ant"],tt)
    gipcba["ipcba_acu"] <- (gipcba[["ipcba"]] - gipcba[["ipcba_ini"]]) / gipcba[["ipcba_ini"]] * 100

   plot(gipcba$mensual, col = "#75AADB",
         xlab = "meses",
         ylab = "% mensual y acumulado",
         main = "ipcBA\n",
         ylim = c(-2,42),type="h", lwd=35, lend=1, axes=FALSE)
    par(new=TRUE)
    plot(gipcba[["ipcba_acu"]] , ylim = c(-2,42), ylab="", xlab="", axes=FALSE, type="l", lwd=5, col = "#FF0000")
    text(c(1:length(gipcba$mensual)),1,gipcba$mensual)
    text(c(1:length(gipcba[["ipcba_acu"]])),gipcba$ipcba_acu+2,round(gipcba$ipcba_acu, digits=1))
    axis(1, at=1:length(gipcba[["mes"]]) , labels=substr(gipcba[["mes"]],1,3))
    axis(1, at=1:length(gipcba[["anno"]]) , labels=substr(gipcba[["anno"]],1,4), outer=TRUE, pos = c(-5.5,0), tick = FALSE) 
    axis(2, at=1:42 , labels=rep("",42), lty = 3)
    axis(3, at=1:length(gipcba[["anno"]]) , labels=rep("",length(gipcba[["anno"]])), lty = 3)
    axis(4, at=1:42 , labels=rep("",42), lty = 3)
    par(adj = 0)
    title(sub = "Fuente: DGEyC")
    par(adj = 0.5)

