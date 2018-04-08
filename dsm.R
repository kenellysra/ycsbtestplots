#YCSB output Plots (HBase vs MySQL)

#Record Read Operations vs Average Latency

readops <- read.csv("readoperations.csv", header = T)

readops$Average.Latency <- as.numeric(readops$Average.Latency)
readops$Record.Read.Operations <- as.numeric(readops$Record.Read.Operations)
readops$Workload <- as.numeric(readops$Workload)
readops$Database<-as.numeric(readops$Database)

ndatabase <- max((readops$Database))

xrange <- range(readops$Record.Read.Operations)
yrange <- range(readops$Average.Latency)

plot(xrange, yrange, type="n", xlab = "Record Read Operations", 
     ylab = "Average Latency (us)")

colors <- rainbow(ndatabase)
linetype <- c(1:ndatabase)
plotchar <- seq(18, 18+ndatabase, 1)

for (i in 1:ndatabase) {
    databases <- subset(readops, Database == i)
    lines(databases$Record.Read.Operations, databases$Average.Latency, type = "b", lwd = 1.5, 
           lty = linetype[i], col = colors[i], pch=plotchar[i])
}
readops$Database<-as.factor(readops$Database)
title("Record Read Operations vs Average Latency ")
#linetype <- c(1:ndatabase)
legend(xrange[1], yrange[2], c("HBase", "MySQL"), cex = 1, col =colors,
       pch=plotchar, lty = linetype, title="Databases")

#Record Update Operations vs Average Latency

updateops <- read.csv("updateoperations.csv", header = T)

updateops$Average.Latency <- as.numeric(updateops$Average.Latency)
updateops$Record.Update.Operations <- as.numeric(updateops$Record.Update.Operations)
updateops$Workload <- as.numeric(updateops$Workload)
updateops$Database<-as.numeric(updateops$Database)

ndatabase <- max((updateops$Database))

xrange <- range(updateops$Record.Update.Operations)
yrange <- range(updateops$Average.Latency)

plot(xrange, yrange, type="n", xlab = "Record Update Operations", 
     ylab = "Average Latency (us)")
colors <- rainbow(ndatabase)
linetype <- c(1:ndatabase)
plotchar <- seq(18, 18+ndatabase, 1)


for (i in 1:ndatabase) {
    databases <- subset(updateops, Database == i)
    lines(databases$Record.Update.Operations, databases$Average.Latency, type = "b", lwd = 2.0, 
          lty = linetype[i], col = colors[i], pch=plotchar[i])
}
updateops$Database<-as.factor(updateops$Database)
title("Record Update Operations vs Average Latency ")
linetype <- c(1:ndatabase)
legend(xrange[1], yrange[2], c("HBase", "MySQL"), cex = 0.7, col =colors,
       pch=plotchar, lty = linetype, title="Databases")

#Throughput

throughput <- read.csv("throughput.csv", header = T)

throughput$Overall.Throughput. <- as.numeric(throughput$Overall.Throughput.)
throughput$Total.Record.Operations <- as.numeric(throughput$Total.Record.Operations)
throughput$Workload <- as.numeric(throughput$Workload)
throughput$Database<-as.numeric(throughput$Database)

ndatabase <- max((throughput$Database))

xrange <- range(throughput$Total.Record.Operations)
yrange <- range(throughput$Overall.Throughput.)

plot(xrange, yrange, type="n", xaxt = "n", xlab = "Total Record Operations", 
     ylab = "Overall Throughput (ops/sec)")
axis(1, at = c("12500", "25000", "50000", "100000") , labels = c("12500", "25000", "50000", "100000") )
colors <- rainbow(ndatabase)
linetype <- c(1:ndatabase)
plotchar <- seq(18, 18+ndatabase, 1)


for (i in 1:ndatabase) {
    databases <- subset(throughput, Database == i)
    lines(databases$Total.Record.Operations, databases$Overall.Throughput., type = "b", lwd = 2.0, 
          lty = linetype[i], col = colors[i], pch=plotchar[i])
}
updateops$Database<-as.factor(updateops$Database)
title("Overall Throughput vs Total Record Operations")
#linetype <- c(1:ndatabase)
legend(xrange[1], yrange[2], c("HBase", "MySQL"), cex = 1, col =colors,
       pch=plotchar, lty = linetype, title="Databases")

