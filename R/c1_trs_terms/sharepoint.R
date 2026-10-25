# c1：SharePoint への配置と取得。
#
# サイト・ライブラリ・フォルダは設定（sharepoint_settings_c1）で指定し、認証情報はコードに書かない。
#   mode = "demo"          ：ローカルのフォルダを SharePoint に見立てる（デモ。実接続ではない）
#   mode = "microsoft365r" ：Microsoft365R パッケージで接続する（認証は Microsoft365R の対話・トークン管理に任せる）
#                            接続情報の提示前のため未接続・未検証。Microsoft365R も未インストール
# クライアントは upload（配置して保存先URL・識別情報を返す）と download（同じ版を取得）を持つ。

sharepoint_settings_c1 <- function(mode = c("demo", "microsoft365r"), site_url = NULL, library = NULL,
                                   folder = NULL, demo_root = file.path("output", "sharepoint_demo")) {
  mode <- match.arg(mode)
  if (mode == "microsoft365r" && (is.null(site_url) || is.null(library))) {
    stop("SharePoint のサイトURLとライブラリを設定してください（接続情報は後で提示）。", call. = FALSE)
  }
  list(mode = mode, site_url = site_url %||% "https://demo.sharepoint.invalid/sites/CURIE",
       library = library %||% "Documents", folder = folder %||% "c1_trs", demo_root = demo_root)
}

make_sharepoint_client_c1 <- function(settings) {
  if (settings$mode == "demo") {
    root <- file.path(settings$demo_root, settings$library)
    url_of <- function(remote) paste0("demo-sharepoint://", sub("^https?://", "", settings$site_url), "/",
                                      settings$library, "/", remote)
    return(list(
      mode = "demo",
      upload = function(local, remote) {
        dest <- file.path(root, remote)
        dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
        file.copy(local, dest, overwrite = TRUE)
        tibble::tibble(remote_path = remote, url = url_of(remote), item_id = paste0("demo-", substr(pdf_sha256_c1(dest), 1, 16)),
                       sha256 = pdf_sha256_c1(dest), uploaded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                       connection = "デモ（ローカルフォルダ。実際のSharePointではない）")
      },
      download = function(remote, dest) {
        file.copy(file.path(root, remote), dest, overwrite = TRUE)
        dest
      }
    ))
  }
  # 実接続（未検証）：Microsoft365R の SharePoint 機能を使う
  if (!requireNamespace("Microsoft365R", quietly = TRUE)) {
    stop("Microsoft365R がありません。SharePoint の接続情報の提示後に導入・設定します。", call. = FALSE)
  }
  site <- Microsoft365R::get_sharepoint_site(site_url = settings$site_url)
  drive <- site$get_drive(settings$library)
  list(
    mode = "microsoft365r",
    upload = function(local, remote) {
      item <- drive$upload_file(local, dest = remote)
      tibble::tibble(remote_path = remote, url = item$properties$webUrl, item_id = item$properties$id,
                     sha256 = pdf_sha256_c1(local), uploaded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                     connection = "Microsoft365R（未検証）")
    },
    download = function(remote, dest) {
      drive$download_file(remote, dest = dest, overwrite = TRUE)
      dest
    }
  )
}
