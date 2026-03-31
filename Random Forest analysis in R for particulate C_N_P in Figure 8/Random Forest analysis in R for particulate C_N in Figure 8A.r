 
otu <- read.delim('Data-RF in R for particulate C_N.txt', row.names = 1)


library(randomForest)


set.seed(123)
otu_forest <- randomForest(POC_PON~., data = otu, importance = TRUE, ntree = 500)
otu_forest


importance_otu.scale <- data.frame(importance(otu_forest, scale = TRUE), check.names = FALSE)
importance_otu.scale


importance_otu.scale <- importance_otu.scale[order(importance_otu.scale$'%IncMSE', decreasing = TRUE), ]


library(ggplot2)

importance_otu.scale$OTU_name <- rownames(importance_otu.scale)
importance_otu.scale$OTU_name <- factor(importance_otu.scale$OTU_name, levels = importance_otu.scale$OTU_name)

p <- ggplot(importance_otu.scale, aes(OTU_name, `%IncMSE`)) +
  geom_col(width = 0.5, fill = '#FFC068', color = NA) +
  labs(title = NULL, x = NULL, y = 'Increase in MSE (%)', fill = NULL) +
  theme(panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = 'black')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand = c(0, 0), limit = c(0, 16))

p


p <- p +
  annotate('text', label = 'POC_PON', x = 2, y = 15, size = 4) +
  annotate('text', label = sprintf('italic(R^2) == %.2f', 0.49), x = 2, y = 13, size = 3, parse = TRUE)

p


library(rfPermute)


set.seed(123)
otu_rfP <- rfPermute(POC_PON~., data = otu, importance = TRUE, ntree = 500, nrep = 1000, num.cores = 1)
otu_rfP


importance_otu.scale <- data.frame(importance(otu_rfP, scale = TRUE), check.names = FALSE)
importance_otu.scale


importance_otu.scale.pval <- (otu_rfP$pval)[ , , 2]
importance_otu.scale.pval

importance_otu.scale <- importance_otu.scale[order(importance_otu.scale$'%IncMSE', decreasing = TRUE), ]


library(ggplot2)

importance_otu.scale$OTU_name <- rownames(importance_otu.scale)
importance_otu.scale$OTU_name <- factor(importance_otu.scale$OTU_name, levels = importance_otu.scale$OTU_name)

p <- ggplot() +
  geom_col(data = importance_otu.scale, aes(x = OTU_name, y = `%IncMSE`), width = 0.5, fill = '#FFC068', color = NA) +
  labs(title = NULL, x = NULL, y = 'Increase in MSE (%)', fill = NULL) +
  theme(panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = 'black')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand = c(0, 0), limit = c(0, 16))

p


for (OTU in rownames(importance_otu.scale)) {
  importance_otu.scale[OTU,'%IncMSE.pval'] <- importance_otu.scale.pval[OTU,'%IncMSE']
  if (importance_otu.scale[OTU,'%IncMSE.pval'] >= 0.05) importance_otu.scale[OTU,'%IncMSE.sig'] <- ''
  else if (importance_otu.scale[OTU,'%IncMSE.pval'] >= 0.01 & importance_otu.scale[OTU,'%IncMSE.pval'] < 0.05) importance_otu.scale[OTU,'%IncMSE.sig'] <- '*'
  else if (importance_otu.scale[OTU,'%IncMSE.pval'] >= 0.001 & importance_otu.scale[OTU,'%IncMSE.pval'] < 0.01) importance_otu.scale[OTU,'%IncMSE.sig'] <- '**'
  else if (importance_otu.scale[OTU,'%IncMSE.pval'] < 0.001) importance_otu.scale[OTU,'%IncMSE.sig'] <- '***'
}

p <- p +
  geom_text(data = importance_otu.scale, aes(x = OTU_name, y = `%IncMSE`, label = `%IncMSE.sig`), nudge_y = 1)

p


p <- p +
  annotate('text', label = 'POC_PON', x = 2, y = 15, size = 4) +
  annotate('text', label = sprintf('italic(R^2) == %.2f', 0.49), x = 2, y = 13, size = 3, parse = TRUE)

p

