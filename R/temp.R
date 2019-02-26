
# highlight_string <-
#   function(x, pattern, highlight_f = crayon::bgRed, do_cat = T){
#     temp <- str_replace_all(x, pattern, highlight_f)
#     if(do_cat) cat(temp)
#     invisible(temp)
#   }




# malign2df <-
#   function(multi_align){
#     df <-
#       data_frame(
#         seq_name = multi_align$forward@pattern@unaligned %>% names(),
#         seq = multi_align$forward@pattern@unaligned %>% as.character(),
#         subj_seq_f = multi_align$forward@subject %>% as.character(),
#         subj_seq_r = multi_align$reverse@subject %>% as.character(),
#         score_f = multi_align$forward@score,
#         score_r = multi_align$reverse@score,
#         start_f = multi_align$forward@subject@range@start,
#         start_r = multi_align$reverse@subject@range@start,
#         width_f = multi_align$forward@subject@range@width,
#         width_r = multi_align$reverse@subject@range@width
#       )
#
#     df <-
#       mutate(df,
#              orientation = if_else(score_f >= score_r, "forward", "reverse"),
#              subj_seq = if_else(orientation == "forward", subj_seq_f, subj_seq_r),
#              score = if_else(orientation == "forward", score_f, score_r),
#              start = if_else(orientation == "forward", start_f, start_r),
#              width = if_else(orientation == "forward", width_f, width_r)
#       ) %>%
#       select(seq_name, seq, orientation, subj_seq, score, start, width) %>%
#       mutate(is_aligned = (width / str_count(seq)) > 0.4)
#
#     return(df)
#   }

# bstr_add_attr_for_seq(
#   pum2,
#   "primer_align",
#   list(malign2df(hige) %>% filter(is_aligned) %>% .[c(1,2,7,8),])
# ) %>%
#   highlight_string("primer_align") %>%
#   walk(~ cat(.))

# highlight_string <-
#   function(bstrobj, attrs){
#     bstrobj <- as_bstr(bstrobj)
#     at <- attributes(bstrobj)
#
#     if(!any(names(at) %in% "attr_seq")) stop()
#     if(!any(names(at[["attr_seq"]]) %in% attrs)) stop()
#
#     if(attrs == "primer_align"){
#       temp_at_li <- at[["attr_seq"]][["primer_align"]]
#
#       temp_highlight <- list()
#       for(i in seq_along(bstrobj)){
#         temp_highlight[[i]] <- bstrobj[i]
#         for(j in 1:nrow(temp_at_li[[i]])){
#           if(temp_at_li[[i]][["orientation"]][j] == "forward"){
#             f <- crayon::bgRed
#           }else{
#             f <- crayon::bgGreen
#           }
#           temp_highlight[[i]] <-
#             str_replace_all(
#               temp_highlight[[i]],
#               temp_at_li[[i]][["subj_seq"]][j],
#               f
#             )
#         }
#       }
#       return(temp_highlight)
#     }
#   }
#
# highlight_mulitiple_align <-
#   function(multi_align, gap_chr = "-"){
#     comple <- multi_align$forward@subject@unaligned %>% as.character()
#     f_pattern <-
#       multi_align$forward@subject %>%
#       as.character() %>%
#       str_remove_all(gap_chr) %>%
#       {.[nchar(.) >= 5]} %>%
#       paste(collapse = "|")
#
#     r_pattern <-
#       multi_align$reverse@subject %>%
#       as.character() %>%
#       str_remove_all(gap_chr) %>%
#       {.[nchar(.) >= 5]} %>%
#       paste(collapse = "|")
#
#     highlight_string(comple, f_pattern, do_cat = F) %>%
#       highlight_string(r_pattern, crayon::bgGreen)
#   }

# is_gateway_compatible <-
#   function(dstrobj){
#     rc_dstrobj <- dstr_rc(dstrobj)
#     num_attL1 <- sum(str_count(dstrobj, "attL1"), str_count(rc_dstrobj, "attL1"))
#     num_attL2 <- sum(str_count(dstrobj, "attL2"), str_count(rc_dstrobj, "attL2"))
#
#     if(num_attL1 != 1L) stop()
#     if(num_attL2 != 1L) stop()
#
#   }
#
# dstr_digest <-
#   function(dstrobj, motif){
#     dstrobj <- as_dstr(dstrobj)
#
#
#   }
#
#
# as_digest_motif <-
#   function(enzyme_name, motif, cut_site = "|"){
#   }
#
#
# # Copy from BioEdit
# rest_list <- clipr::read_clip()
#
# rest_df <-
#   rest_list %>%
#   .[-c(1,2)] %>%
#   str_split("\\s+") %>%
#   map(~
#         data_frame(
#           enzyme_name = .x[1],
#           motif = .x[2],
#           cuts_at = .x[5],
#           leaves_overhang = .x[7],
#           isoschizomers = .x[10]
#         )
#   ) %>%
#   bind_rows()
