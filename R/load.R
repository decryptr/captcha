#' Load or list available models for prediction
#'
#' The `captcha_available_models()` function lists the models available for
#' prediction in the `{captcha}` package or another repository referenced
#' in the `repo` parameter.
#' The models are loaded from the [captcha repository
#' release page](https://github.com/decryptr/captcha/releases/tag/captcha_model)
#' using the [piggyback::pb_list()] function.
#' The `captcha_load_model()` function downloads a model using
#' [piggyback::pb_download()] and returns a `luz_module_fitted` object.
#'
#' @param repo repo in the form `"<user>/<captcha>"`.
#'   Defaults to `"decryptr/captcha"`
#' @param tag tag name of the release to load the file.
#'
#' @name models
#'
#' @return `captcha_available_models()` returns a character vector with the
#' names of the available models. `captcha_load_model()` returns an object
#' of class `luz_module_fitted`.
#'
#' @details
#'
#' Currently, available models are:
#'
#
#' * [trf5](https://pje.trf5.jus.br/pje/ConsultaPublica/listView.seam):
#'   Tribunal Regional Federal 5
#' * [tjmg](https://www4.tjmg.jus.br/juridico/sf/proc_resultado.jsp?comrCodigo=24&numero=1&listaProcessos=50718889720218130024&btn_pesquisar=Pesquisar):
#'   Tribunal de Justiça de Minas Gerais
#' * [trt](https://pje-consulta.trt3.jus.br/pje-consulta-api/api/processos/2104879):
#'   Tribunal Regional do Trabalho 3
#' * [esaj](http://esaj.tjba.jus.br/cpopg/open.do):
#'   Tribunal de Justiça da Bahia
#' * [jucesp](https://www.jucesponline.sp.gov.br/ResultadoBusca.aspx):
#'   Junta Comercial de São Paulo
#' * [tjpe](https://srv01.tjpe.jus.br/consultaprocessualunificada/):
#'   Tribunal de Justiça de Pernambuco
#' * [tjrs](https://www.tjrs.jus.br/site_php/consulta/verificador.php):
#'   Tribunal de Justiça do Rio Grande do Sul
#' * [sei](https://sei.economia.gov.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php?acao_externa=protocolo_pesquisar&acao_origem_externa=protocolo_pesquisar&id_orgao_acesso_externo=0):
#'   Sistema Eletrônico de Informações - ME
#' * [rfb](https://servicos.receita.fazenda.gov.br/servicos/cnpjreva/Cnpjreva_Solicitacao_CS.asp):
#'   Receita Federal do Brasil
#'
#' This list may be updated when new models are added to the release.
#'
#' @examples
#' # run only with internet connection
#' if (interactive()) {
#'
#'   captcha_models_available()
#'
#'   # loads rfb model
#'   model <- captcha_load_model("rfb")
#'   # model estimated accuracy on validation dataset
#'   utils::tail(model$records$metrics$valid, 1)[[1]][["captcha acc"]]
#'
#' }
#'
#'
#' @export
captcha_available_models <- function(repo = "decryptr/captcha",
                                     tag = "captcha_model") {
  model_list <- piggyback::pb_list(repo = repo, tag = tag)[["file_name"]]
  gsub("\\.pt$", "", model_list)
}


#' @param captcha file name or captcha name
#'
#' @rdname models
#'
#' @export
captcha_load_model <- function(captcha,
                               repo = "decryptr/captcha",
                               tag = "captcha_model") {

  if (file.exists(captcha)) {
    path <- captcha
  } else {
    f_model <- paste0(captcha, ".pt")
    dir_tmp <- tempdir("model")
    piggyback::pb_download(
      file = f_model,
      dest = dir_tmp,
      repo = repo,
      tag = tag
    )
    path <- paste0(dir_tmp, "/", f_model)
  }

  luz::luz_load(path)

}
