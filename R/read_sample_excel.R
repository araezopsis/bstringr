
df2bstr <- function(df) bstr(df$seq, df$seq_name)

read_sample_excel <-
  function(fpath){
    sheet_names <- readxl::excel_sheets(fpath)

    df_list <-
      purrr::map(sheet_names, ~ readxl::read_excel(fpath, sheet = .x))

    names(df_list) <- sheet_names
    df_list <-
      purrr::map(df_list, df2bstr)
    df_list
  }

bstr2df <-
  function(bstrobj){
    bstrobj <- as_bstr(bstrobj)
    tibble::tibble(
      seq_name = names(bstrobj),
      seq = as.character(bstrobj)
    )
  }
