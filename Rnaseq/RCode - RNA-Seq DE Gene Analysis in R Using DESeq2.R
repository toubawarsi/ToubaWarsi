#install.packages("BiocManager")
#BiocManager::install("DESeq2")
library(DESeq2)

#read in read count data (see, "RawCounts_Mus.csv" file)
data <- read.csv("~/Family Documents/Touba/Github/toubawarsi.github.io/Rnaseq/1 - Data_ RawCounts_Mus.csv", header = TRUE, row.names = NULL)

#making gene names column into row names so have a dataframe of just read counts
rownames(data) = make.names(data$Gene, unique=TRUE)

expressiondata <- data[,-1]

#visualizing ColSums for exploratory analysis
barplot(colSums(expressiondata)/1e6, las=3)
#looking at read counts before normalization
hist(expressiondata$WT_L8_rep1,br=100, main="Read Counts before Normalization for WT_L8 Replicate 1", xlab="WT_L8 Replicate 1")
#looking at read counts after log2 normalization
logCountData=log2(1+expressiondata)
hist(logCountData$WT_L8_rep1,br=100,main="Read Counts after Log2 Normalization for WT_L8 Replicate 1", xlab="WT_L8 Replicate 1")

#log2 transforming data so very low read counts and very high read counts are 
#normalized, otherwise excessive amounts of low read counts severely skew data
#the +1 is to ensure no -inf from taking log of 0 reads

#plotting replicates against each other (ideal world, perfect replicates should 
#be like a straight line; indicating 0 blgcl difference in the gene expression
#between the two replicates)
#WT_L8_rep1 vs rep2
plot(logCountData[,1],logCountData[,2], main="Plot of Replicates for WT_L8", xlab="Replicate 1", ylab="Replicate 2")
#WT_L12_rep1 vs rep2, 
plot(logCountData[,3],logCountData[,4], main="Plot of Replicates for WT_L12", xlab="Replicate 1", ylab="Replicate 2")
#KO_L8_rep1 vs rep2
plot(logCountData[,5],logCountData[,6], main="Plot of Replicates for KO_L8", xlab="Replicate 1", ylab="Replicate 2")
#KO_L12_rep1 vs rep2
plot(logCountData[,7],logCountData[,8], main="Plot of Replicates for KO_L12", xlab="Replicate 1", ylab="Replicate 2")

#if mislabled replicates:
#WT_L8_rep1 vs rep2
plot(logCountData[,1],logCountData[,6], main="Plot of Mislabled Replicates for L8", xlab="WT_L8 Replicate 1", ylab="KO_L8 Replicate 2")


#Derive experiment information from the sample names
#Sample names
snames <- colnames(expressiondata) 
snames
#Creating colData  for DESeq2
Genotype <- substr(snames, 0, 2) 
Trt <- substr(snames, 4, 5)
colData <- as.data.frame(cbind(colnames(expressiondata),Genotype,Trt))

#Creating DESeq2 dataset while specifying design of experiment
#in design we want to look at interaction (Gen*Trt) because accounting
#for effect of Treatment on Genotype and vice versa
dds <- DESeqDataSetFromMatrix(countData = expressiondata,
                              colData = colData,
                              design = ~Genotype+Trt+Genotype*Trt)
#now creating DESeq2 object
dds = DESeq((dds))
#check if rows in dds is same as expressiondata
nrow(dds)
#pre-filtering DESeq data to remove very low expressed genes, chose threshold
#of rowsum=5 based on looking at logCountData
dds = dds[rowSums(counts(dds)) > 5,]
#check how number of rows have changed
nrow(dds)

#looking at Results of DESeq
#NOTE: The log2(fold-change) is the log-ratio of a gene's or a transcript's 
#expression values in two different conditions; Fold change is a measure 
#describing how much a quantity changes between an original and a subsequent measurement
#NOTE: Keeping DESeq default of alpha=0.1 
results <- results(dds)
head(results)
summary(results)

#look at PCA after log2 transforming count data
library(ggplot2)
rld = rlog(dds)
plotPCA(rld, intgroup="Genotype")
plotPCA(rld, intgroup="Trt")
plotPCA(rld, intgroup=c("Genotype","Trt"))

#making a heatmap!
#this function helps with naming heatmap
detectGroups <- function (x){  # x are col names
  tem <- gsub("[0-9]*$","",x) # Remove all numbers from end
  #tem = gsub("_Rep|_rep|_REP","",tem)
  tem <- gsub("_$","",tem); # remove "_" from end
  tem <- gsub("_Rep$","",tem); # remove "_Rep" from end
  tem <- gsub("_rep$","",tem); # remove "_rep" from end
  tem <- gsub("_REP$","",tem)  # remove "_REP" from end
  return( tem )
}
#checking naming for heatmap 
detectGroups(colnames(expressiondata))
# distance function = 1-PCC (Pearson's correlation coefficient)
#will calculate how similar a particular gene's expression is between 
#replicates and treatments
dist2 <- function(x, ...)   
  as.dist(1-cor(t(x), method="pearson"))

#install.packages("gplots")
library(gplots)

# average linkage in hierarchical clustering
hclust2 <- function(x, method="average", ...)  
  hclust(x, method=method, ...)

# number of top genes by standard deviation
n=50 

#dataframe used to create heatmap, while used logtransformed DESeq object
x = assay(rld)

###next bit will calculate how similar a gene is within and between treatments###
# max	as data
if(n>dim(x)[1]) n = dim(x)[1] 
# sort genes by standard deviation
x = x[order(apply(x,1,sd),decreasing=TRUE),]  
# only keep the n genes
x = x[1:n,]   

# this will cutoff very large values, which could skew the color 
x=as.matrix(x[1:n,])-apply(x[1:n,],1,mean)
cutoff = median(unlist(x)) + 4*sd (unlist(x)) 
x[x>cutoff] <- cutoff
#cutoff = median(unlist(x)) - 4*sd (unlist(x)) 
#x[x< cutoff] <- cutoff

groups = detectGroups(colnames(x) )
groups.colors = rainbow(length(unique(groups) ) )


lmat = rbind(c(5,4),c(0,1),c(3,2))
lwid = c(1.5,4)
lhei = c(1,.2,4)


heatmap.2(x, distfun = dist2,hclustfun=hclust2,
          col=greenred(75), density.info="none", trace="none", scale="none", keysize=.5
          ,key=T, symkey=F
          ,ColSideColors=groups.colors[ as.factor(groups)]
          ,margins=c(8,12)
          ,cexRow=1
          ,srtCol=45
          ,cexCol=1.  # size of font for sample names
          ,lmat = lmat, lwid = lwid, lhei = lhei
)


#changing default lfc from 0 because doesn't make sense to keep genes with no 
#fold change
res <- results(dds, lfcThreshold=0.01)
summary(res)
#creating MA Plot to see significant DE genes, above and below 0 at y-axis are
#up- and down-regulated genes respectively, and grey are not significant
#while blue is signifcant DE genes 
DESeq2:: plotMA(res)

#creating volcano plot, showing significant genes in the broad sense
library(dplyr)
resvolplot <- as.data.frame(res)
#False positive adjustment of adjusted p-value 
resvolplot <- mutate(resvolplot, Significant=ifelse(resvolplot$padj<0.1,"FDR<0.1","Not Sig"))
#now ensuring that the significant genes in previous columns also have a strong
#log fold change (lfc), just an extra step 
resvolplot[which(abs(resvolplot$log2FoldChange)<1.0), "Significant"] = "Not Sig" 
library(ggplot2)
#volcano plot showing significantly expressed genes using lfc and adj p-value
#-log10 of padj is necessary to scale it
ggplot(resvolplot,aes(log2FoldChange, -log10(padj))) + geom_point(aes(col=Significant)) +
  scale_color_manual(values=c("red","black"))
#FYI: plot without -log10 adjustment
#ggplot(resvolplot,aes(log2FoldChange, padj)) + geom_point(aes(col=Significant)) +
#  scale_color_manual(values=c("red","black"))



#checking out gene with the highest fold change, this is the most different
#gene expressed between the two groups
res <- res[order(abs(res$log2FoldChange), decreasing = TRUE),]
topGene <- rownames(res)[1]
plotCounts(dds, gene=topGene, intgroup=c("Genotype","Trt"))


#Gene Ontology Analysis
#What category the genes of interest belong to; 
#cellular component (CC), biological process (BP), molecular function (MF )
library(GO.db)
library(GOstats)
library(AnnotationDbi)
#install human library, as noted in text file
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("org.Hs.eg.db")
library(org.Hs.eg.db)

#bringing back gene IDs
res$GeneSymbol <- row.names(res)

#Mapping Gene Symbol to EntrezID for GOHyperGeometric test to figure out which
#category (CC, BP, MF) a particular gene belongs to
#res$Entrez <- mapIds(org.Hs.eg.db,
#                    key=res$GeneSymbol,
#                   column="ENTREZID",
#                  keytype="SYMBOL",
#                multiVals="first")
#Far too many NAs using Human database
#human <- sum(is.na(res$Entrez))

#now trying to map using Mouse database
#BiocManager::install("org.Mm.eg.db")
library(org.Mm.eg.db)
res$EntrezMouse <- mapIds(org.Mm.eg.db,
                          key=res$GeneSymbol,
                          column="ENTREZID",
                          keytype="SYMBOL",
                          multiVals="first")

#Far less NAs using mouse database, will proceed with mouse database
mouse <- sum(is.na(res$EntrezMouse))

#Saving DESeq Results: signifcant if 0.1 padj, lfc set as 1
#write.csv(res, file="Data_Significant-0.1_lfc-1_results.csv")


#first threshold, adjusted pval: results with adjusted pval less than 0.5
res_05 <- as.data.frame(subset(res,padj<0.5))

#second threshold log fold change: results with lfc > 4
sig_lfc <- 4

#Identifying up-regulated genes according to padj and lfc thresholds
GenesUp <- unique(res_05[res_05$log2FoldChange>sig_lfc, "EntrezMouse"])

#Identifying down-regulated genes according to padj and lfc thresholds
GenesDown <- unique(res_05[res_05$log2FoldChange<(-sig_lfc), "EntrezMouse"])

#Need Universe GeneIDs for HyperG test
UniGenes <- unique(res_05$EntrezMouse)

#third threshold p-value cut off of 0.01
CO <- 0.01

#Parameters for Up-regulated Genes, in Biological Processes
upParams <- new("GOHyperGParams",
                geneIds=GenesUp,
                universeGeneIds=UniGenes,
                annotation="org.Mm.eg.db" ,
                ontology="BP",
                pvalueCutoff=CO,
                conditional=FALSE,
                testDirection="over")

#Parameters for Down-regulated Genes, in Biological Processes
downParams <- new("GOHyperGParams",
                  geneIds=GenesDown ,
                  universeGeneIds=UniGenes,
                  annotation="org.Mm.eg.db" ,
                  ontology="BP",
                  pvalueCutoff=CO,
                  conditional=FALSE,
                  testDirection="over")

#Running the HyperG Test for Up-Regulated Genes in BP category
Up_Biol_Proc <- hyperGTest(upParams)
#looking at first 10 rows to get an idea
summary(Up_Biol_Proc)[1:10,]
#looks like significantly up-regulated genes deal with neural structure/transport

#Running the HyperG Test for Down-Regulated Genes in BP category
Down_Biol_Proc <- hyperGTest(downParams)
#looking at first 10 rows to get an idea
summary(Down_Biol_Proc)[1:10,]
#looks like significantly down-regulated genes deal with heart muscle structure

#Running the HyperG Test for Up-Regulated Genes in CC category
ontology(upParams)="CC"
Up_Cell_Comp <- hyperGTest(upParams)
#looking at first 10 rows to get an idea
summary(Up_Cell_Comp)[1:10,]
#looks like significantly up-regulated genes deal with neural structure/transport

#Running the HyperG Test for Down-Regulated Genes in CC category
ontology(downParams)="CC"
Down_Cell_Comp <- hyperGTest(downParams)
#looking at first 10 rows to get an idea
summary(Down_Cell_Comp)[1:10,]
#looks like significantly down-regulated genes deal with heart muscle structure

#Running the HyperG Test for Up-Regulated Genes in MF category
ontology(upParams)="MF"
Up_Mol_Func <- hyperGTest(upParams)
#looking at first 10 rows to get an idea
summary(Up_Mol_Func)[1:10,]
#only significant one is SNARE binding, the best studied SNAREs are those 
#that mediate the neurotransmitter release of synaptic vesicles in neurons.

#Running the HyperG Test for Down-Regulated Genes in MF category
ontology(downParams)="MF"
Down_Mol_Func <- hyperGTest(downParams)
#looking at first 10 rows to get an idea
summary(Down_Mol_Func)[1:10,]
#looks like significantly down-regulated genes deal with binding functions

#Gene Ontology pathways with KEGG
#BiocManager::install("pathview")
library(pathview)
#BiocManager::install("gage")
library(gage)
#BiocManager::install("gageData")
library(gageData)

#creating variables for gage, pairing lfc to entrezIDs
foldchanges <- res$log2FoldChange
names(foldchanges) <- res$EntrezMouse

#pulling in mouse data from gage datasets
data("go.sets.mm")
data("go.subs.mm")


########Looking at KEGG pathways in Mouse
data("kegg.sets.mm")
#for signalling and metabolic pathways
data("sigmet.idx.mm")
#"cleaner" gene sets of sinaling and metabolic pathways only
kegg_mm <- kegg.sets.mm[sigmet.idx.mm]

#looking at KEGG results
Kegg_res <- gage(exprs = foldchanges, 
                 gsets = kegg_mm, 
                 same.dir = TRUE)
#View up-regulated results
View(as.data.frame(Kegg_res$greater))
#View down-regulated results
View(as.data.frame(Kegg_res$less))

