
#' Reshape fusions to row per fusion partner
#'
#' @param df input data
edit_fusions = function(df) {
  if ("start_fusion" %in% colnames(df)) {
    fusions = df[!is.na(df$start_fusion),]

    a_gene = fusions
    b_gene = fusions

    a_gene$gene = stringr::str_split_fixed(a_gene$start_fusion, "\\(", 2)[,1]
    a_gene$fusion = paste(a_gene$start_fusion, a_gene$start_fusion, sep = "--")

    b_gene$gene = stringr::str_split_fixed(b_gene$start_fusion, "\\(", 2)[,1]
    b_gene$fusion = paste(b_gene$start_fusion, b_gene$start_fusion, sep = "--")

    new_fusions = rbind(a_gene[, colnames(a_gene) %nin% c("start_fusion", "end_fusion")],
                        b_gene[b_gene$gene != a_gene$gene, colnames(b_gene) %nin% c("start_fusion", "end_fusion")])
    out = bind_rows(df[is.na(df$start_fusion), colnames(df) %nin% c("start_fusion", "end_fusion")],
                    new_fusions)
  } else {
    out = df
  }

  return(out)
}


format_data = function(data){
  edit_fusions(data)

  data$variant_type = ""
  if("fusion" %in% colnames(data)) {
    data[!is.na(data$fusion),]$variant_type = "SV"
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


#' Sort and slice
#'
#' @param x input data
#' @param whitelist genes in whitelist for priority sorting
#' @param sort_cols other columns to use in priority sorting
#' @param order_vec vector of same length of sort_cols indicating in the same order whether the column should be arranged in "Ascending" or "Descending" order
#' @param ntop max number of genes to show or NA
modify_plot_data = function(x,
                            whitelist = NULL,
                            sort_cols = NULL,
                            order_vec = NULL,
                            ntop = NA) {
  x = x %>%
    group_by(gene, dataset) %>%
    mutate(value = n())

  sel_cols = c("gene", "dataset", "value")

  if(length(unlist(whitelist)) > 0) {
    sort_cols = c("whitelist", sort_cols)
    order_vec = c("Descending", order_vec)
    sel_cols = c(sel_cols, "whitelist")
    x$whitelist = as.numeric(x$gene %in% unlist(whitelist))
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
  } else {
    gene_order = x %>%
      group_by(gene) %>%
      mutate(max_value = max(value)) %>%
      arrange(desc(value)) %>%
      ungroup() %>%
      mutate(rank = row_number())
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
