# eda.R
# gapminder 데이터 탐색적 데이터 분석(Exploratory Data Analysis)
# 실행: Rscript eda.R
# 결과: 콘솔 요약 출력 + figures/ 폴더에 그래프(PNG) 저장

# ---- 0. 설정 --------------------------------------------------------------
data_path <- file.path("data", "gapminder.csv")
fig_dir   <- "figures"

if (!file.exists(data_path)) stop(sprintf("데이터 파일 없음: %s", data_path))
if (!dir.exists(fig_dir)) dir.create(fig_dir)

df <- read.csv(data_path, stringsAsFactors = FALSE)

cat("==========================================================\n")
cat(" gapminder 탐색적 데이터 분석(EDA) 리포트\n")
cat("==========================================================\n\n")

# ---- 1. 전반 요약 ---------------------------------------------------------
cat("[1] 데이터 개요\n")
cat(sprintf("  - 관측치: %d행 x %d열\n", nrow(df), ncol(df)))
cat(sprintf("  - 기간  : %d ~ %d (%d개 연도)\n",
            min(df$year), max(df$year), length(unique(df$year))))
cat(sprintf("  - 국가  : %d개,  대륙: %d개\n\n",
            length(unique(df$country)), length(unique(df$continent))))

cat("[1-1] 수치형 변수 기술통계(summary)\n")
print(summary(df[, c("pop", "lifeExp", "gdpPercap")]))
cat("\n")

# 표준편차/변동계수
cat("[1-2] 산포(표준편차, 변동계수)\n")
for (col in c("pop", "lifeExp", "gdpPercap")) {
  m <- mean(df[[col]]); s <- sd(df[[col]])
  cat(sprintf("  - %-10s mean=%s  sd=%s  CV=%.2f\n",
              col, format(round(m, 2)), format(round(s, 2)), s / m))
}
cat("\n")

# ---- 2. 대륙별 집계 -------------------------------------------------------
cat("[2] 대륙별 평균 (전체 연도)\n")
agg_cont <- aggregate(cbind(lifeExp, gdpPercap, pop) ~ continent, data = df, FUN = mean)
agg_cont[, -1] <- round(agg_cont[, -1], 1)
print(agg_cont)
cat("\n")

# ---- 3. 시간 추세 ---------------------------------------------------------
cat("[3] 연도별 전세계 평균 추세\n")
agg_year <- aggregate(cbind(lifeExp, gdpPercap) ~ year, data = df, FUN = mean)
agg_year[, -1] <- round(agg_year[, -1], 1)
print(agg_year)
cat(sprintf("\n  -> 기대수명: %.1f세(1952) -> %.1f세(2007), +%.1f세\n",
            agg_year$lifeExp[1], tail(agg_year$lifeExp, 1),
            tail(agg_year$lifeExp, 1) - agg_year$lifeExp[1]))
cat(sprintf("  -> 1인당GDP: %.0f(1952) -> %.0f(2007), %.1f배\n\n",
            agg_year$gdpPercap[1], tail(agg_year$gdpPercap, 1),
            tail(agg_year$gdpPercap, 1) / agg_year$gdpPercap[1]))

# ---- 4. 상관관계 ----------------------------------------------------------
cat("[4] 상관관계 (수치형 변수)\n")
cor_mat <- cor(df[, c("year", "pop", "lifeExp", "gdpPercap")])
print(round(cor_mat, 3))
cat(sprintf("\n  - lifeExp ~ log(gdpPercap) 상관: %.3f (로그 변환 시)\n\n",
            cor(df$lifeExp, log(df$gdpPercap))))

# ---- 5. 최신연도(2007) 순위 ----------------------------------------------
cat("[5] 2007년 기준 순위\n")
d07 <- df[df$year == 2007, ]
cat("  - 기대수명 상위 5개국:\n")
print(head(d07[order(-d07$lifeExp), c("country", "continent", "lifeExp")], 5), row.names = FALSE)
cat("  - 기대수명 하위 5개국:\n")
print(head(d07[order(d07$lifeExp), c("country", "continent", "lifeExp")], 5), row.names = FALSE)
cat("  - 1인당 GDP 상위 5개국:\n")
print(head(d07[order(-d07$gdpPercap), c("country", "continent", "gdpPercap")], 5), row.names = FALSE)
cat("\n")

# ---- 6. 그래프 저장 -------------------------------------------------------
cat("[6] 그래프 저장 -> figures/\n")
cont_levels <- sort(unique(df$continent))
cont_col <- setNames(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")[seq_along(cont_levels)],
                     cont_levels)

# 6-1. 기대수명 분포 히스토그램
png(file.path(fig_dir, "01_lifeExp_hist.png"), width = 800, height = 600)
hist(df$lifeExp, breaks = 30, col = "steelblue", border = "white",
     main = "기대수명 분포 (전체)", xlab = "lifeExp (years)", ylab = "빈도")
dev.off()

# 6-2. 연도별 기대수명 추세
png(file.path(fig_dir, "02_lifeExp_trend.png"), width = 800, height = 600)
plot(agg_year$year, agg_year$lifeExp, type = "b", pch = 19, col = "darkred",
     main = "연도별 전세계 평균 기대수명", xlab = "year", ylab = "평균 lifeExp")
grid()
dev.off()

# 6-3. 대륙별 기대수명 박스플롯 (2007)
png(file.path(fig_dir, "03_lifeExp_by_continent_2007.png"), width = 800, height = 600)
boxplot(lifeExp ~ continent, data = d07, col = cont_col[sort(unique(d07$continent))],
        main = "대륙별 기대수명 (2007)", xlab = "continent", ylab = "lifeExp")
dev.off()

# 6-4. GDP vs 기대수명 산점도 (2007, 로그 x축)
png(file.path(fig_dir, "04_gdp_vs_lifeExp_2007.png"), width = 800, height = 600)
plot(d07$gdpPercap, d07$lifeExp, log = "x",
     col = cont_col[d07$continent], pch = 19,
     main = "1인당 GDP vs 기대수명 (2007)",
     xlab = "gdpPercap (log scale)", ylab = "lifeExp")
legend("bottomright", legend = names(cont_col), col = cont_col, pch = 19, bty = "n")
dev.off()

# 6-5. 대륙별 평균 기대수명 시계열
png(file.path(fig_dir, "05_lifeExp_trend_by_continent.png"), width = 800, height = 600)
agg_cy <- aggregate(lifeExp ~ year + continent, data = df, FUN = mean)
plot(NA, xlim = range(agg_cy$year), ylim = range(agg_cy$lifeExp),
     main = "대륙별 평균 기대수명 추세", xlab = "year", ylab = "평균 lifeExp")
for (cont in cont_levels) {
  sub <- agg_cy[agg_cy$continent == cont, ]
  lines(sub$year, sub$lifeExp, col = cont_col[cont], lwd = 2, type = "b", pch = 19)
}
legend("bottomright", legend = cont_levels, col = cont_col, lwd = 2, bty = "n")
dev.off()

cat("  - 01_lifeExp_hist.png\n")
cat("  - 02_lifeExp_trend.png\n")
cat("  - 03_lifeExp_by_continent_2007.png\n")
cat("  - 04_gdp_vs_lifeExp_2007.png\n")
cat("  - 05_lifeExp_trend_by_continent.png\n\n")

cat("==========================================================\n")
cat(" EDA 완료\n")
cat("==========================================================\n")
