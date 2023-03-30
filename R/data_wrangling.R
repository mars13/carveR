
#' Reshape fusions to row per fusion partner
#'
#' @param df input data
edit_fusions = function(df) {
  if ("start_fusion" %in% colnames(df)) {
    fusions = df[!is.na(df$start_fusion),]

    a_gene = fusions
    b_gene = fusions

    a_gene$gene = stringr::str_split_fixed(a_gene$start_fusion, "\\(", 2)[,1]
    a_gene$fusion = paste(a_gene$start_fusion, a_gene$end_fusion, sep = "--")

    b_gene$gene = stringr::str_split_fixed(b_gene$start_fusion, "\\(", 2)[,1]
    b_gene$fusion = paste(b_gene$start_fusion, b_gene$end_fusion, sep = "--")

    new_fusions = rbind(a_gene, b_gene)
    out = bind_rows(df[is.na(df$start_fusion),], new_fusions)
  } else {
    out = df
  }

  return(out)
}


format_data = function(data){

  data$variant_type = ""

  if("start_fusion" %in% colnames(data)) {
    data = edit_fusions(data)
    data[!is.na(data$start_fusion),]$variant_type = "SV"
  }
  if("avecopynumber" %in% colnames(data)) {
    data[!is.na(data$avecopynumber),]$variant_type = "CNA"
  }
  if("ref" %in% colnames(data)) {
    data[!is.na(data$ref),]$variant_type = "SNV"
  }
  data = data[data$variant_type != "",]
  data = data[data$gene != "",]
  return(data)
}


#' Sort and slice data
#'
#' The function modifies input data frame by grouping data by gene and dataset, calculating the number of times a particular gene appears in a dataset, and sorting the data based on user-defined columns and gene lists. The function allows to select a maximum number of genes to return.
#'
#' @param x A data frame with at least gene and dataset columns
#' @param whitelist Vector of genes to prioritise in sorting the data frame (prevails over other columns in sort_cols)
#' @param sort_cols Vector of column names to sort the data frame
#' @param order_vec Vector specifying the order of sorting for each column in sort_cols vector
#' @param ntop Numeric value specifying the number of top genes to be included in the output data frame. Defaults to NA (all genes returned).
modify_plot_data = function(x,
                            whitelist = NULL,
                            sort_cols = NULL,
                            order_vec = NULL,
                            ntop = NA) {
  x = x %>%
    group_by(gene, dataset) %>%
    mutate(value = n())

  sel_cols = c("gene", "dataset", "value", "text_info")

  if(length(unlist(whitelist)) > 0) {
    whitelist = unlist(whitelist)
    sort_cols = c("whitelist", sort_cols)
    order_vec = c("Descending", order_vec)
    sel_cols = c(sel_cols, "whitelist")
    x$whitelist = as.numeric(x$gene %in% whitelist)
    wl_not_x = whitelist[!whitelist %in% x$gene]
    extra_wl = data.frame(gene = wl_not_x, value = 0, whitelist = 0)
    x = rbind(x, extra_wl)
  }

  if (!is.null(sort_cols)){
    sel_cols = unique(c(sel_cols, sort_cols))
    sort_cols = rev(c(sort_cols, "max_value"))
    order_vec = rev(c(order_vec, "Descending"))
    gene_order = x

    for (i in c(1:length(sort_cols))) {
      v = sort_cols[i]
      if (order_vec[i] == "Descending") {
        gene_order = gene_order %>%
          group_by(gene) %>%
          mutate(max_value = max(value)) %>%
          arrange(desc(!!!rlang::syms(v)))
      } else {
        gene_order = gene_order %>%
          group_by(gene) %>%
          mutate(max_value = max(value)) %>%
          arrange(!!!rlang::syms(v))
      }
    }
    gene_order = gene_order %>%
      ungroup() %>%
      mutate(rank = row_number())

    gene_order$text_info = gsub(" " , "\n", Reduce(paste, lapply(sort_cols[sort_cols != "max_value"], function(x)
      paste(x, gene_order[, x, drop = T], sep =  ":"))))
  } else {

    gene_order = x %>%
      group_by(gene) %>%
      mutate(max_value = max(value)) %>%
      arrange(desc(value)) %>%
      ungroup() %>%
      mutate(rank = row_number(), text_info = "")
  }

  gene_factor = gene_order %>%
    group_by(gene) %>%
    arrange(rank) %>%
    filter(row_number()==1)

  if (!is.na(ntop) & ntop != 0) {
    gene_factor = gene_factor %>%
      ungroup() %>%
      arrange(rank) %>%
      dplyr::slice(1:ntop)
  }

  gene_order$gene = factor(gene_order$gene, levels = gene_factor[order(gene_factor$rank),]$gene)
  gene_order$dataset = factor(gene_order$dataset)

  gene_order = gene_order[,sel_cols] %>%
    group_by(gene, dataset) %>%
    arrange(gene, dataset) %>%
    filter(row_number()==1)

  return(gene_order)
}



#' Create a unique column of variant consequence
#'
#' @param df input data
edit_consequence_summary = function(df) {
    df$consequence_summary = "Unkown"
    if("SNV" %in% df$variant_type) {
      values = unlist(lapply( df[df$variant_type == "SNV",]$consequence, function(x) ifelse(is.na(x), "Unknown", x)))
      df[df$variant_type == "SNV",]$consequence_summary = paste("SNV", values, sep = ":")
    }
    if("CNA" %in% df$variant_type) {
      values = unlist(lapply( df[df$variant_type == "CNA",]$avecopynumber, function(x) ifelse(is.na(x), "Unknown", ifelse(sign(x) == -1, "LOSS", "GAIN"))))
      df[df$variant_type == "CNA",]$consequence_summary = paste("CNA", values, sep = ":")
    }
    if("SV" %in% df$variant_type) {
      values = unlist(lapply( df[df$variant_type == "SV",]$sv_type, function(x) ifelse(is.na(x), "Unknown", x)))
      df[df$variant_type == "SV",]$consequence_summary = paste("SV", values, sep = ":")
    }
    return(df)
}

