# clean.R
# gapminder 데이터 품질 확인(Data Quality Check) 스크립트
# 실행: Rscript clean.R

# ---- 0. 설정 --------------------------------------------------------------
# 주의: 파일 확장자는 .csv 이지만 내용은 쉼표(,) 구분입니다.
data_path <- file.path("data", "gapminder.csv")

if (!file.exists(data_path)) {
  stop(sprintf("데이터 파일을 찾을 수 없습니다: %s", data_path))
}

# 문자열을 factor로 자동 변환하지 않고, 빈칸/NA 표기를 결측으로 처리
df <- read.csv(
  data_path,
  header           = TRUE,
  stringsAsFactors = FALSE,
  na.strings       = c("", "NA", "N/A", "null", "NULL"),
  check.names      = FALSE
)

cat("==========================================================\n")
cat(" gapminder 데이터 품질 확인 리포트\n")
cat("==========================================================\n\n")

# ---- 1. 기본 구조 ---------------------------------------------------------
cat("[1] 기본 구조\n")
cat(sprintf("  - 행(rows)   : %d\n", nrow(df)))
cat(sprintf("  - 열(cols)   : %d\n", ncol(df)))
cat(sprintf("  - 컬럼명     : %s\n\n", paste(names(df), collapse = ", ")))

cat("  - 컬럼별 타입:\n")
for (col in names(df)) {
  cat(sprintf("      %-12s : %s\n", col, class(df[[col]])))
}
cat("\n")

# 기대 스키마 검증
expected_cols <- c("country", "year", "pop", "continent", "lifeExp", "gdpPercap")
missing_cols  <- setdiff(expected_cols, names(df))
extra_cols    <- setdiff(names(df), expected_cols)
if (length(missing_cols) > 0)
  cat(sprintf("  [경고] 누락된 기대 컬럼: %s\n", paste(missing_cols, collapse = ", ")))
if (length(extra_cols) > 0)
  cat(sprintf("  [참고] 예상 외 추가 컬럼: %s\n", paste(extra_cols, collapse = ", ")))
cat("\n")

# ---- 2. 결측치(Missing values) -------------------------------------------
cat("[2] 결측치(NA) 개수\n")
na_counts <- sapply(df, function(x) sum(is.na(x)))
for (col in names(na_counts)) {
  pct <- round(100 * na_counts[[col]] / nrow(df), 2)
  cat(sprintf("      %-12s : %d (%.2f%%)\n", col, na_counts[[col]], pct))
}
cat(sprintf("  -> 총 결측치: %d\n\n", sum(na_counts)))

# ---- 3. 중복 행 -----------------------------------------------------------
cat("[3] 중복 확인\n")
dup_full <- sum(duplicated(df))
cat(sprintf("  - 완전 중복 행: %d\n", dup_full))

# country + year 조합은 유일해야 함(고유 키)
if (all(c("country", "year") %in% names(df))) {
  key_dup <- sum(duplicated(df[, c("country", "year")]))
  cat(sprintf("  - (country, year) 키 중복: %d\n", key_dup))
  if (key_dup > 0) {
    cat("    [경고] 동일 국가-연도 조합이 중복됩니다:\n")
    dkeys <- df[duplicated(df[, c("country", "year")]), c("country", "year")]
    print(head(unique(dkeys), 10))
  }
}
cat("\n")

# ---- 4. 수치형 컬럼 범위/이상치 ------------------------------------------
cat("[4] 수치형 컬럼 요약 및 범위 검증\n")
num_cols <- c("year", "pop", "lifeExp", "gdpPercap")
for (col in intersect(num_cols, names(df))) {
  x <- suppressWarnings(as.numeric(df[[col]]))
  cat(sprintf("  - %s\n", col))
  cat(sprintf("      min=%s  max=%s  mean=%s  median=%s\n",
              format(min(x, na.rm = TRUE)),
              format(max(x, na.rm = TRUE)),
              format(round(mean(x, na.rm = TRUE), 3)),
              format(round(median(x, na.rm = TRUE), 3))))
  # 음수/0 등 비정상 값 점검
  n_neg <- sum(x < 0, na.rm = TRUE)
  if (n_neg > 0) cat(sprintf("      [경고] 음수 값 %d개\n", n_neg))
}
cat("\n")

# 도메인 규칙 기반 이상치 검증
cat("[4-1] 도메인 규칙 위반 점검\n")
flag <- function(cond, msg) {
  n <- sum(cond, na.rm = TRUE)
  cat(sprintf("      %-45s : %d건\n", msg, n))
}
if ("year"      %in% names(df)) flag(df$year < 1900 | df$year > 2025, "year 1900~2025 범위 밖")
if ("pop"       %in% names(df)) flag(df$pop <= 0,                     "pop 0 이하")
if ("lifeExp"   %in% names(df)) flag(df$lifeExp <= 0 | df$lifeExp > 120, "lifeExp 0이하 또는 120 초과")
if ("gdpPercap" %in% names(df)) flag(df$gdpPercap <= 0,               "gdpPercap 0 이하")
cat("\n")

# ---- 5. 범주형 컬럼 -------------------------------------------------------
cat("[5] 범주형 컬럼 점검\n")
if ("continent" %in% names(df)) {
  cat("  - continent 분포:\n")
  print(table(df$continent, useNA = "ifany"))
  valid_cont <- c("Africa", "Americas", "Asia", "Europe", "Oceania")
  bad_cont <- setdiff(unique(na.omit(df$continent)), valid_cont)
  if (length(bad_cont) > 0)
    cat(sprintf("  [경고] 알 수 없는 continent 값: %s\n", paste(bad_cont, collapse = ", ")))
}
if ("country" %in% names(df)) {
  cat(sprintf("\n  - 고유 국가 수: %d\n", length(unique(df$country))))
  # 앞뒤 공백이 포함된 country 값 점검
  ws <- sum(df$country != trimws(df$country), na.rm = TRUE)
  if (ws > 0) cat(sprintf("  [경고] 앞뒤 공백 포함 country 값: %d건\n", ws))
}
cat("\n")

# ---- 6. 패널 균형성(국가별 연도 수) --------------------------------------
if (all(c("country", "year") %in% names(df))) {
  cat("[6] 패널 균형성(국가별 관측 연도 수)\n")
  per_country <- tapply(df$year, df$country, function(y) length(unique(y)))
  tbl <- table(per_country)
  cat("  - 국가별 연도 개수 분포 (연도수: 국가수):\n")
  print(tbl)
  mode_n <- as.integer(names(which.max(tbl)))
  unbalanced <- names(per_country)[per_country != mode_n]
  if (length(unbalanced) > 0) {
    cat(sprintf("  [참고] 표준(%d개) 과 다른 국가 %d개:\n", mode_n, length(unbalanced)))
    print(head(per_country[unbalanced], 20))
  } else {
    cat(sprintf("  -> 모든 국가가 %d개 연도로 균형(balanced)\n", mode_n))
  }
}
cat("\n")

cat("==========================================================\n")
cat(" 품질 확인 완료\n")
cat("==========================================================\n")
