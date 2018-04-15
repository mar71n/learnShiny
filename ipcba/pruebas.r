ipcba <- read.csv("../datasets/IPCBA_.csv", stringsAsFactors=FALSE)
ipcba <- edit(ipcba)
    ipcba <- ipcba[which(ipcba$anno==2018),]
    tt <- nrow(ipcba)
    ipcba["ipcba_ant"] <- ipcba[["ipcba"]]/(1 + ipcba[["mensual"]] / 100)
    ipcba["ipcba_ini"] <- rep(ipcba[1,"ipcba_ant"],tt)
    ipcba["ipcba_acu"] <- (ipcba[["ipcba"]] - ipcba[["ipcba_ini"]]) / ipcba[["ipcba_ini"]] * 100

   plot(ipcba$mensual, col = "#75AADB",
         xlab = "meses",
         ylab = "% mensual y acumulado",
         main = "ipcBA",
         ylim = c(-2,42),type="h", lwd=35, lend=1, axes=TRUE,
         names = ipcba[which(ipcba$anno==2017),]$mes)
    par(new=TRUE)
    plot(ipcba$ipcba_acu , ylim = c(-2,42), ylab="", xlab="", axes=FALSE, type="l", lwd=5)
    text(c(1:12),1,ipcba$mensual)
    text(c(1:12),ipcba$ipcba_acu+2,round(ipcba$ipcba_acu, digits=1))

?text
?trunc
round(ipcba$ipcba_acu, digits=1)
round(ipcba$ipcba_acu, digits=1)
plot(c(ipcba$mensual,ipcba$ipcba_acu), ylim = c(-2,42), ylab="", xlab="", axes=FALSE, type="l", lwd=13)

meses <- factor(c(1:12), labels= c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))

meses <- c(1:12)
meses <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
?labels
labels(meses)[1] <- "Ene"

labels(meses)
