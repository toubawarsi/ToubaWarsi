
## publicly available dataset: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE140023

#Source: Conway BR, O'Sullivan ED, Cairns C, O'Sullivan J et al. Kidney Single-Cell 
#Atlas Reveals Myeloid Heterogeneity in Progression and Regression of Kidney Disease. 
#J Am Soc Nephrol 2020 Dec;31(12):2833-2854. PMID: 32978267

#5 single cell libraries, 3 mice pooled per library, deep sequencing on Nextseq 550:
#-4*10x workflows 
#-1*smartseq2) 

#install.packages("Seurat")
library(Seurat) ## your main single cell workflow toolkit
library(ggplot2) 
library(tidyverse) 
#install.packages("reticulate")
library(reticulate) ## allows python functions to be called in R
library(Matrix) 

library(AnnotationDbi) ## to convert ensembl ID to symbol
library(org.Mm.eg.db) # Mus musculus database
library(viridis) #heatmap


## read in dataset ---- may require change in file name of data

dat.UUO2<-Read10X(
  data.dir = "C:/Users/Ahsan/Desktop/Touba/Self-Directed Projects/GSE140023_RAW/UUO2",
  gene.column = 1,
  unique.features = TRUE,
  strip.suffix = FALSE
) 

dat.UUO2

##change ENSEMBL ID to genesymbol---- because genesymbol more readable by humans
#Mapping the Ensembl Gene ID back to the first symbol

x <- as.data.frame((rownames(dat.UUO2)))
colnames(x)<-c("ENSEMBL")

x$SYMBOL <- mapIds(org.Mm.eg.db,
                          keys=x$ENSEMBL,
                          column="SYMBOL",
                          keytype="ENSEMBL",
                          multiVals="first",asNA=F)

head(x)

# tackling issues with excess matches so collapse into a single column
x$FINAL <- ifelse(is.na(x$SYMBOL), x$ENSEMBL, x$SYMBOL) 

#replacing ENSEMBL rownames with x$FINAL (gene symbol where available, otherwise use
# ENSEMBL ID) as rownames in original dataset
for (name in rownames(dat.UUO2)) {
  rownames(dat.UUO2)[match(name, rownames(dat.UUO2))]<-x$FINAL[match(name, x$ENSEMBL)]
}
head(rownames(dat.UUO2)) #check and done cleaning up original dataset

##  create seurat object ---- 
#Essential commands: https://satijalab.org/seurat/essential_commands.html

dat <- CreateSeuratObject(
  dat.UUO2,
  project = "UUO2",
  assay = "RNA",
  min.cells = 0,
  min.features = 0,
  names.field = 1,
  names.delim = "_",
  meta.data = NULL
)

## review the object

dat

## QC: Addressing some sources of problems/noise in scRNAseq:

 #Low nFeature_RNA for a cell indicates that it may be dead/dying or an empty droplet

#1)"nFeature_RNA":The number of unique genes detected in each cell 
#Low nFeature_RNA indicates Low-quality (dead/dying) cells or empty droplets
#High nFeature_RNA indicates that the "cell" may in fact be a doublet


#2)"nCount_RNA": The total number of molecules detected within a cell 
#High nCount_RNA indicates that the "cell" may in fact be a doublet

#3)"percent.mt": The percentage of reads that map to the mitochondrial genome
#High percent.mt indicates cells where overall RNA is coming from mitochondria
#cells that are releasing their mtRNA are often going through apoptosis, so best to remove 

##obtaining mtRNA QC stats##: 
# Using [[ operator to stash mtDNA QC stats
#note: "pattern" is different based on source of cells (human vs mouse,etc)
dat[["percent.mt"]] <- PercentageFeatureSet(dat, pattern = "^Mt") 
rownames(dat)
range(dat[["percent.mt"]])


#Show above stated QC metrics for the first 5 cells
head(dat@meta.data, 5)


VlnPlot(dat, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)

#seurat default thresholds:
#filter cells that have unique feature counts over 2,500 or less than 200
#filter cells that have >5% mitochondrial counts
dat <- subset(dat, subset = nFeature_RNA > 200 & nFeature_RNA < 2500 & percent.mt < 5)

## normalise ---- to counteract technical noise and bias introduced in each workflow
#global-scaling normalization method "LogNormalize" that:
#1)normalizes the feature expression measurements for each cell by the total expression
#2)multiplies this by a scale factor (10,000 by default)
#3)and log-transforms the result 

dat <- NormalizeData(dat)

## Variable gene selection ---- cutting back on background noise and highlighting differences
#focusing on the genes with most variation (i.e, they are highly expressed in some cells,
#and lowly expressed in others). This helps to highlight biological signal in single-cell datasets.
dat <- FindVariableFeatures(dat, selection.method = "vst", nfeatures = 2000)

## what are our variable genes ?
top10 <- head(VariableFeatures(dat), 20)
top10

# plot variable features with (plot2) and without (plot1) labels 
plot1 <- VariableFeaturePlot(dat)
plot1 
plot2 <- LabelPoints(plot = plot1, points = top10, repel = TRUE)
plot2

## scale---- 
#standard pre-processing step of applying a linear transformation ('scaling')  prior 
#to dimensional reduction techniques (like PCA)
#The ScaleData() function:
#-Shifts the expression of each gene, so that the mean expression across cells is 0
#-Scales the expression of each gene, so that the variance across cells is 1: so that 
#highly-expressed genes do not dominate

all.genes <- rownames(dat)
dat <- ScaleData(dat, features = all.genes)

## Examine and visualize PCA results
dat <- RunPCA(dat, features = VariableFeatures(object = dat))
print(dat[["pca"]], dims = 1:5, nfeatures = 5)
VizDimLoadings(dat, dims = 1, reduction = "pca")
DimPlot(dat, reduction = "pca")

#Heatmap
#allows for easy exploration of the primary sources of heterogeneity in a dataset
DimHeatmap(dat, dims = 1, cells = 500, balanced = TRUE)
#and can be useful when trying to decide which PCs to include for further downstream analyses
DimHeatmap(dat, dims = 1:15, cells = 500, balanced = TRUE)
VizDimLoadings(dat, dims = c(1,3,5,6,11), reduction = "pca")

#a ranking of principle components based on the percentage of variance explained 
#by each one
ElbowPlot(dat) 

## cluster ---- graph-based clustering approach
# cells embedded in a graph structure (ex: K-nearest neighbor (KNN) graph)
#with edges drawn between cells with similar feature expression patterns, 
#and then partition this graph into highly interconnected 'quasi-cliques' or 'communities'.

#first construct a KNN graph based on the euclidean distance in PCA space
#edge weights refined between any two cells is based on shared overlap in 
#their local neighborhoods - can change dims
dat <- FindNeighbors(dat, dims = 1:20) 

#modularity optimization technique: trying to  define groups to maximize community structure
#"resolution":increased values leading to a greater number of clusters
dat <- FindClusters(dat, resolution = 0.1) 
# Look at cluster IDs of the first 15 cells
head(Idents(dat), 15)

#UMAP: non-linear dimensional reduction (process of reducing the number of features
#to the most relevant ones) -> visualizing clusters and their relative proximities
dat <- RunUMAP(dat, dims = 1:20, n.neighbors = 20,min.dist = .01,spread = 6) 

## dimension reduction plot
DimPlot(dat, reduction = "umap",pt.size = 1, label = TRUE)

##cluster ---- define clusters via differential expression/cluster biomarkers 

#comparing one cluster to all clusters
#min.pct: requires a feature to be detected at a minimum percentage in either of the two groups of cells
dat.markers <- FindAllMarkers(dat, only.pos = F, min.pct = 0.2, logfc.threshold = 0.2)
#looking at greatest log fold change
dat.markers_log2FC <- dat.markers[order(abs(dat.markers$avg_log2FC), decreasing = TRUE),]
head(dat.markers_log2FC)

#find all markers distinguishing cluster 0 from cluster 2
#min.pct: requires a feature to be detected at a minimum percentage in either of the two groups of cells
dat.markers.0v2 <- FindMarkers(dat, ident.1= "0", ident.2="2",only.pos = F, min.pct = 0.2, logfc.threshold = 0.2)
dat.markers.0v2_log2FC <- dat.markers[order(abs(dat.markers.0v2$avg_log2FC), decreasing = TRUE),]
head(dat.markers.0v2_log2FC)

#creating vector for heatmap
top10.dat.markers <- dat.markers %>% group_by(cluster) %>% top_n(n = 10, wt = avg_log2FC)
DoHeatmap(dat, features = top10.dat.markers$gene,) + NoLegend() + scale_fill_viridis(option = "B")+ 
  theme(text = element_text(size = 20))

## rank and save the marker genes for later GSEA
dat.markers$rank <-(dat.markers$avg_log2FC)*(-log(dat.markers$p_val_adj)) 
#write.csv(dat.markers,"dat.DEG_genes.csv")


#FeaturePlot() visualizes feature expression on a tSNE or PCA plot
FeaturePlot(dat, features = top10[1:5])

FeaturePlot(dat, features = c("Slc34a1","Umod","Tfcp2l1","Emcn","Cd3g"),order=T, pt.size = 1)

DotPlot(object = dat,features = c("Slc34a1","Umod","Tfcp2l1","Emcn","Cd3g"),dot.scale = 15) + coord_flip()

