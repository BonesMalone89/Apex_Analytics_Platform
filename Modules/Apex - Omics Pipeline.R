# ==============================================================================
# APEX PLATFORM: UNIVERSAL OMICS STAGING ENGINE
# Module: Apex_Stage_Dataset.R
# ==============================================================================

library(Seurat)
library(ggplot2)
library(glmGamPoi) # Critical for fast regularized SCTransform

# ------------------------------------------------------------------------------
# 1. CORE STAGING HELPER: Diet & Standardization
# ------------------------------------------------------------------------------
apex_standardize_and_save <- function(seurat_obj, 
                                      output_path, 
                                      data_type = c("Spatial", "SingleCell"),
                                      cell_type_col = NULL,
                                      sample_id = "Sample") {
  
  data_type <- match.arg(data_type)
  message("--> Standardizing metadata and reducing memory footprint...")
  
  # A. Standardize Cell Annotation / Identity
  if (!is.null(cell_type_col) && cell_type_col %in% colnames(seurat_obj@meta.data)) {
    seurat_obj$Apex_CellType <- as.character(seurat_obj@meta.data[[cell_type_col]])
  } else if ("seurat_clusters" %in% colnames(seurat_obj@meta.data)) {
    seurat_obj$Apex_CellType <- paste0("Cluster_", seurat_obj$seurat_clusters)
  } else {
    seurat_obj$Apex_CellType <- as.character(Idents(seurat_obj))
  }
  
  # B. Set Universal Apex Metadata Tags
  seurat_obj$Apex_DataType <- data_type
  seurat_obj$Apex_SampleID <- sample_id
  
  # C. Standardize Dimensionality Reductions
  # Preserve UMAP if present; fallback to PCA coordinates if UMAP is missing
  avail_reducs <- names(seurat_obj@reductions)
  target_reducs <- intersect(c("umap", "pca"), avail_reducs)
  
  # D. Memory Stripping (DietSeurat)
  # Removes intermediate scale.data, nearest-neighbor graphs, and dense matrices
  # typically reducing on-disk .rds footprint by 65-80%.
  active_assay <- DefaultAssay(seurat_obj)
  assays_to_keep <- unique(c("RNA", "SCT", "Spatial", active_assay))
  assays_to_keep <- intersect(assays_to_keep, names(seurat_obj@assays))
  
  diet_obj <- DietSeurat(
    seurat_obj,
    assays = assays_to_keep,
    dimreducs = target_reducs,
    graphs = NULL
  )
  
  # E. Save compressed artifact
  message("--> Writing standardized artifact to: ", output_path)
  saveRDS(diet_obj, file = output_path, compress = "xz")
  
  file_size_mb <- round(file.info(output_path)$size / (1024 * 1024), 2)
  message("--> Complete. Staged file size: ", file_size_mb, " MB\n")
}

# ------------------------------------------------------------------------------
# 2. PIPELINE: 10x Genomics Visium Spatial Stager
# ------------------------------------------------------------------------------
stage_visium_dataset <- function(visium_dir, 
                                 output_path, 
                                 sample_id = "Visium_Tissue",
                                 slice_name = "tissue_slice",
                                 min_counts = 500,
                                 max_mt_pct = 20) {
  
  message("==================================================================")
  message("Processing Spatial Visium: ", sample_id)
  message("Source Directory: ", visium_dir)
  message("==================================================================")
  
  if (!dir.exists(visium_dir)) {
    stop("Directory does not exist: ", visium_dir)
  }
  
  # Step 1: Load 10x Spatial Data
  spatial_obj <- Load10X_Spatial(
    data.dir = visium_dir,
    filename = "filtered_feature_bc_matrix.h5",
    assay = "Spatial",
    slice = slice_name
  )
  
  # Step 2: Quality Control Filtering
  spatial_obj[["percent.mt"]] <- PercentageFeatureSet(spatial_obj, pattern = "^mt-|^MT-")
  
  spots_before <- ncol(spatial_obj)
  spatial_obj <- subset(
    spatial_obj, 
    subset = nCount_Spatial >= min_counts & percent.mt <= max_mt_pct
  )
  spots_after <- ncol(spatial_obj)
  message(sprintf("--> QC filtering retained %d / %d spots (%.1f%%)", 
                  spots_after, spots_before, (spots_after / spots_before) * 100))
  
  # Step 3: Fast Regularized Normalization via SCTransform + glmGamPoi
  message("--> Running SCTransform with glmGamPoi...")
  spatial_obj <- SCTransform(
    spatial_obj, 
    assay = "Spatial", 
    method = "glmGamPoi",
    vst.flavor = "v2",
    verbose = FALSE
  )
  
  # Step 4: Clustering to establish default anatomical domains
  spatial_obj <- RunPCA(spatial_obj, assay = "SCT", verbose = FALSE)
  spatial_obj <- FindNeighbors(spatial_obj, dims = 1:20, verbose = FALSE)
  spatial_obj <- FindClusters(spatial_obj, resolution = 0.5, verbose = FALSE)
  
  # Step 5: Export to standardized format
  apex_standardize_and_save(
    seurat_obj = spatial_obj,
    output_path = output_path,
    data_type = "Spatial",
    cell_type_col = "seurat_clusters",
    sample_id = sample_id
  )
}

# ------------------------------------------------------------------------------
# 3. PIPELINE: Single-Cell / Single-Nucleus RNA-seq Stager
# ------------------------------------------------------------------------------
stage_singlecell_rds <- function(input_rds_path, 
                                 output_path, 
                                 sample_id = "scRNA_Atlas",
                                 cell_type_col = NULL) {
  
  message("==================================================================")
  message("Processing Single-Cell Dataset: ", sample_id)
  message("Input RDS: ", input_rds_path)
  message("==================================================================")
  
  if (!file.exists(input_rds_path)) {
    stop("Input file does not exist: ", input_rds_path)
  }
  
  sc_obj <- readRDS(input_rds_path)
  
  # Check if standard cell annotation column candidate exists
  if (is.null(cell_type_col)) {
    candidates <- c("cell_type", "cell_annotation", "celltype", "broad_type", "Cluster")
    match_idx <- which(candidates %in% colnames(sc_obj@meta.data))
    if (length(match_idx) > 0) {
      cell_type_col <- candidates[match_idx[1]]
      message("--> Auto-detected cell annotation column: '", cell_type_col, "'")
    }
  }
  
  apex_standardize_and_save(
    seurat_obj = sc_obj,
    output_path = output_path,
    data_type = "SingleCell",
    cell_type_col = cell_type_col,
    sample_id = sample_id
  )
}