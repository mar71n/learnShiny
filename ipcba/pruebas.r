ipcba <- read.csv("../datasets/IPCBA_.csv", stringsAsFactors=FALSE)
# ipcba <- edit(ipcba)
    gipcba <- ipcba[which(ipcba$anno==2018),]
    gipcba <- ipcba[3:16,]
    gipcba <- tail(ipcba[ !is.na(ipcba[["ipcba"]]), ], n=12L)
    tt <- nrow(gipcba)
    gipcba["ipcba_ant"] <- gipcba[["ipcba"]]/(1 + gipcba[["mensual"]] / 100)
    gipcba["ipcba_ini"] <- rep(gipcba[1,"ipcba_ant"],tt)
    gipcba["ipcba_acu"] <- (gipcba[["ipcba"]] - gipcba[["ipcba_ini"]]) / gipcba[["ipcba_ini"]] * 100
    max(gipcba$mensual, na.rm = TRUE)
    max(gipcba[["ipcba_acu"]], na.rm = TRUE) * 1.17
    max(c(max(gipcba$mensual, na.rm = TRUE), max(gipcba[["ipcba_acu"]], na.rm = TRUE))) * 1.17
    min(c(min(gipcba$mensual, na.rm = TRUE), min(gipcba[["ipcba_acu"]], na.rm = TRUE)))

   plot(gipcba$mensual, col = "#75AADB",
         xlab = "meses",
         ylab = "% mensual y acumulado",
         main = "ipcBA\n",
         ylim = c(-2,42),type="h", lwd=40, lend=1, axes=FALSE)
    par(new=TRUE)
    plot(gipcba[["ipcba_acu"]] , ylim = c(-2,42), ylab="", xlab="", axes=FALSE, type="l", lwd=5, col = "#FF0000")
    text(c(1:length(gipcba$mensual)),1,gipcba$mensual, cex = 1.2)
    text(c(1:length(gipcba[["ipcba_acu"]])),gipcba$ipcba_acu+2,round(gipcba$ipcba_acu, digits=1), cex = 1.2)
    axis(1, at=1:length(gipcba[["mes"]]) , labels=substr(gipcba[["mes"]],1,3))
    axis(1, at=1:length(gipcba[["anno"]]) , labels=paste("\n",substr(gipcba[["anno"]],1,4)), outer=TRUE, pos = c(-5.5,0), tick = FALSE)
    axis(2, at=1:42 , labels=rep("",42), lty = 3)
    axis(3, at=1:length(gipcba[["anno"]]) , labels=rep("",length(gipcba[["anno"]])), lty = 3)
    axis(4, at=1:42 , labels=rep("",42), lty = 3)
    par(adj = 0)
    title(sub = "Fuente: DGEyC")
    par(adj = 0.5)
    box(lty=1)
    legend(1, 42, c("Mensual","Acumulado"),  col=c("#75AADB","red"), lty=c(1,1), lwd = c(8,8), bg = "#FFFFFF");
    
    abline(8,0)
    abline(25,0)
    points(4,8)
    points(8,12)
    points(8,17)
    points(7,12)
    lines(c(4,8),c(8,12))
    lines(c(3,7),c(12,22))
    lines(c(8,12), c(12,15))
    
