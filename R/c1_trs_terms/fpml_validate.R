# c1：FpMLの構文・スキーマ検証（xml2）。
# 構文（整形式）は常に検証する。スキーマ（XSD）は、FpML公式のスキーマファイルを
# 環境変数 CURIE_FPML_XSD（または引数 xsd_path）で指定したときだけ検証する。
# 公式スキーマは手元にないため、未指定なら「XSD未設定（スキーマ検証未実施）」と記録する。

validate_fpml_c1 <- function(path, xsd_path = Sys.getenv("CURIE_FPML_XSD", "")) {
  doc <- tryCatch(xml2::read_xml(path), error = function(e) e)
  well_formed <- !inherits(doc, "error")
  root_ok <- well_formed && xml2::xml_name(doc) %in% c("dataDocument", "requestConfirmation")
  schema <- if (!well_formed) {
    list(status = "未実施（構文エラー）", errors = conditionMessage(doc))
  } else if (!nzchar(xsd_path) || !file.exists(xsd_path)) {
    list(status = "XSD未設定（スキーマ検証未実施）", errors = "")
  } else {
    res <- tryCatch(xml2::xml_validate(doc, xml2::read_xml(xsd_path)), error = function(e) e)
    if (inherits(res, "error")) list(status = "検証エラー", errors = conditionMessage(res))
    else list(status = if (isTRUE(res)) "スキーマ適合" else "スキーマ不適合",
              errors = paste(attr(res, "errors"), collapse = "\n"))
  }
  tibble::tibble(file = basename(path), well_formed = well_formed,
                 root_element = if (well_formed) xml2::xml_name(doc) else NA_character_,
                 root_ok = root_ok, schema_check = schema$status, schema_errors = schema$errors,
                 xsd = if (nzchar(xsd_path)) basename(xsd_path) else NA_character_)
}
