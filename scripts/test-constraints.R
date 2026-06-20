# ===============================
# Testes do gerador de restrições de Shadow CAT (bin/constraints.R)
# ===============================
#
# Estilo informal (stopifnot), sem dependência de framework de teste.
# Rodar a partir da raiz do projeto:
#     Rscript scripts/test-constraints.R
#
# Não requer mirtCAT: testa-se build_constr_matrix (puro) em vez do closure,
# que é a única parte que toca o objeto mirt.

source("bin/cat.R")          # abort_bad_request / abort_unprocessable / %||%
source("bin/constraints.R")  # funções sob teste

pass <- 0L
check <- function(desc, expr) {
  if (!isTRUE(expr)) stop(sprintf("FALHOU: %s", desc))
  pass <<- pass + 1L
  cat(sprintf("  ok - %s\n", desc))
}

# Espera que `expr` lance um erro cuja mensagem casa com `pattern`.
expect_error <- function(desc, expr, pattern = NULL) {
  e <- tryCatch({ force(expr); NULL }, error = function(e) e)
  if (is.null(e)) stop(sprintf("FALHOU (esperava erro): %s", desc))
  if (!is.null(pattern) && !grepl(pattern, conditionMessage(e)))
    stop(sprintf("FALHOU (mensagem inesperada): %s -> %s", desc, conditionMessage(e)))
  pass <<- pass + 1L
  cat(sprintf("  ok - %s\n", desc))
}

cat("== normalize_op ==\n")
check("== via '='",            normalize_op("=")  == "==")
check("== via 'igual a'",      normalize_op("igual a") == "==")
check("<= via '=<'",           normalize_op("=<") == "<=")
check("<= via rótulo PT",      normalize_op("menor ou igual a") == "<=")
check(">= via '=>'",           normalize_op("=>") == ">=")
check(">= via símbolo ≥",      normalize_op("≥")  == ">=")
check("espaços tolerados",     normalize_op("  >=  ") == ">=")
expect_error("operador inválido", normalize_op("!="), "Operador não reconhecido")

cat("== parse_item_query ==\n")
check("ALL -> todos os itens", identical(parse_item_query("ALL", 5), 1:5))
check("all minúsculo também",  identical(parse_item_query("all", 3), 1:3))
check("lista por vírgula",     identical(parse_item_query("1, 5, 6", 10), c(1L, 5L, 6L)))
check("intervalo a:b",         identical(parse_item_query("3:7", 10), 3:7))
check("lista + intervalo",     identical(parse_item_query("1, 3:7, 9", 10), c(1L, 3L, 4L, 5L, 6L, 7L, 9L)))
check("dedup + ordenação",     identical(parse_item_query("5, 1, 5, 2:3", 10), c(1L, 2L, 3L, 5L)))
check("espaços internos",      identical(parse_item_query("  1 ,  4 : 6 ", 10), c(1L, 4L, 5L, 6L)))
expect_error("índice acima do range", parse_item_query("1, 11", 10), "fora do intervalo")
expect_error("índice zero/baixo",     parse_item_query("0", 10),     "fora do intervalo")
expect_error("consulta vazia",        parse_item_query("   ", 10),   "Consulta vazia")
expect_error("token não numérico",    parse_item_query("a,b", 10),   "Índice inválido")
expect_error("intervalo malformado",  parse_item_query("3:", 10),    "Intervalo inválido")

cat("== build_constr_matrix ==\n")
# Restrições: exatamente 10 itens no teste; itens 1 e 2 inimigos (no máx. 1).
constraints <- list(
  list(query = "ALL",  op = "==", value = 10),
  list(query = "1, 2", op = "<=", value = 1)
)
df <- build_constr_matrix(constraints, nitems = 12)
check("dimensão N_restrições x (N_itens + 2)", all(dim(df) == c(2, 14)))
check("colunas lhs/dirs/rhs",  identical(names(df), c(paste0("X", 1:12), "dirs", "rhs")))
check("linha 1 = todos 1",     all(as.numeric(df[1, 1:12]) == 1))
check("linha 2 marca 1 e 2",   identical(which(as.numeric(df[2, 1:12]) == 1), c(1L, 2L)))
check("dirs corretas",         identical(as.character(df$dirs), c("==", "<=")))
check("rhs correto",           identical(as.numeric(df$rhs), c(10, 1)))

cat("== rows_to_constraints (ponte data.frame) ==\n")
cdf <- data.frame(
  query = c("ALL", "3:5"),
  op    = c("==",  ">="),
  value = c(8,      1),
  stringsAsFactors = FALSE
)
cons <- rows_to_constraints(cdf)
check("converte 2 linhas",     length(cons) == 2)
check("tripla preservada",     cons[[2]]$query == "3:5" && cons[[2]]$op == ">=" && cons[[2]]$value == 1)

cat("== normalize_shadow_constraints / build_shadow_constr_fun ==\n")
check("NULL -> sem restrições", is.null(build_shadow_constr_fun(NULL)))
check("lista vazia -> NULL",    is.null(build_shadow_constr_fun(list())))
check("data.frame -> closure",  is.function(build_shadow_constr_fun(cdf)))
check("tripla única (objeto)",
      length(normalize_shadow_constraints(list(query = "ALL", op = "==", value = 5))) == 1)
expect_error("tripla sem campo 'value'",
             build_shadow_constr_fun(list(list(query = "ALL", op = "=="))),
             "deve conter os campos")
expect_error("value não-numérico (eager)",
             build_shadow_constr_fun(list(list(query = "ALL", op = "==", value = "x"))),
             "value' numérico")

cat("== resolve_constr_fun (dispatcher) ==\n")
fn_shadow <- resolve_constr_fun(shadow_test_config = cdf, constr_fun_string = NULL)
check("shadow tem prioridade",  is.function(fn_shadow) && length(body(fn_shadow)) > 1)
fn_legacy <- resolve_constr_fun(shadow_test_config = NULL,
                                constr_fun_string = "function(design, person, test){ data.frame() }")
check("fallback p/ string legada", is.function(fn_legacy))
fn_empty <- resolve_constr_fun(shadow_test_config = NULL, constr_fun_string = NULL)
check("sem nada -> função vazia",  is.function(fn_empty) && length(body(fn_empty)) <= 1)

cat(sprintf("\nTODOS OS %d TESTES PASSARAM.\n", pass))
