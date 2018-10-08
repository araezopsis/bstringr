

# test_seq <- c("ATGC", "AGCT--GT")
# gp <-
#   bstr2df(test_seq) %>%
#   ggplot(aes(pos, seq_name, label = residue, fill = residue))
# gp

# gp + geom_raster() + scale_fill_manual(values = dna_color) + geom_text()
# gp + geom_raster() + geom_text()

# temp <- bstringr::read_fasta("~/Desktop/i_blast/out_align/caf1/for_tree_HsCAF1.fas")
# temp %>%
#   bstr_sub_true(1, 1, 100) %>%
#   .[1:10] %>%
#   bstr2df() %>%
#   mutate_percentage() %>%
#   mutate_alpha_by_ident() %>%
#   ggplot(aes(pos, seq_name, label = residue, fill = residue)) +
#   geom_raster(aes(alpha = al)) +
#   scale_fill_manual(values = color_aa_zappo) +
#   geom_text()

# bstr2df(bstr(test_seq, c("A", "A")))

# temp %>%
#   bstr_sub_true(1, 1, 30) %>%
#   bstr2df() %>%
#   calc_ident_per_pos()



# mutate_percentage <-
#   function(df){
#     pos_df <-
#       df %>%
#       group_by(pos, residue) %>%
#       summarise(freq = n()) %>%
#       mutate(per = freq / sum(freq) * 100) %>%
#       ungroup() %>%
#       mutate(ID = paste0(pos, residue)) %>%
#       select(-residue, -pos)
#
#     mutate(df, ID = paste0(pos, residue)) %>%
#       left_join(pos_df, by = "ID") %>%
#       select(-ID)
#   }

# set_color_by_identity <-
#   function(df, threshold = 60){
#     df %>%
#       mutate(al = if_else(per >= threshold, residue, NA_character_))
#   }

# mutate_alpha_by_ident <-
#   function(df, threshold = 60){
#     threshold <- sort(threshold)
#     df <- mutate(df, al = NA_real_)
#     for(th in threshold){
#       df <-
#         mutate(df, al = if_else(per >= th, th, al))
#     }
#     df
#   }

# gp <-
#   hoge %>%
#   mutate(ID = paste0(seq, pos)) %>%
#   left_join(hige, by = "ID") %>%
#   create_gp() %>%
#   plot_shade() %>%
#   plot_seq(5)
#
# gp +
#   coord_cartesian(xlim = c(1, 35)) +
#   theme(
#     text = element_text(size = 5 * ggplot2::.pt),
#     axis.title = element_blank(),
#     rect = element_blank(),
#     legend.position = "none"
#   )
#
#
# hoge <- bstr(c("ATGC--CG", "ACGC-GGG"))
#
# mf2 <- function(x, g = "-") if(any(x != g)) x[1] == x[2]
# mf <- function(x) x[1] == x[2]
#
# calc_ident_2seq <-
#   function(x, y, gap_chr = "-"){
#     str_extract_all(c(x, y), ".", simplify = T) %>%
#       apply(2, mf2) %>%
#       unlist %>%
#       {sum(.)/length(.)}
#   }
#
# calc_ident_2seq(hoge[1], hoge[2])
#
# calc_ident_mat <-
#   function(bstrobj, gap_chr = "-"){
#     l <- length(bstrobj)
#     n <- names(bstrobj)
#     mat <- matrix(nrow = l, ncol = l)
#     row.names(mat) <- n
#     colnames(mat) <- n
#     cb <- combn(seq_along(bstrobj), 2, simplify = F)
#     for(i in cb){
#       mat[i[1], i[2]] <-
#         calc_ident_2seq(bstrobj[i[1]], bstrobj[i[2]], gap_chr = gap_chr)
#     }
#     return(t(mat))
#   }
#
# calc_ident_mat(
#   read_fasta("~/Desktop/i_blast/out_align/caf1/xout_HsCAF1_onlyarabi.fas")
#   ) %>%
#   round(digits = 2)
#
# calc_ident_mat(
#   read_fasta("~/Desktop/i_blast/out_align/not9/for_tree_HsNOT9.fas")
#   ) %>%
#   round(digits = 2)
#
# # read_fasta("~/Desktop/i_blast/out_align/clustalo_out_APUM.fas") %>%
# #   bstr2df() %>%
