# eda.R
# gapminder 데이터 탐색적 데이터 분석(EDA) - 최종 보강판
# 실행: Rscript eda.R
# 결과: 콘솔 요약 출력 + figures/ 그래프(PNG) + tables/ 요약표(CSV)
#
# 보강 내용(비판적 재검토 반영):
#   - 인구가중 평균(단순평균의 편향 보정)
#   - 분포 형태(왜도/첨도) 및 로그변환 분포
#   - IQR 기반 이상치 탐지
#   - 국가별 성장률(CAGR) 및 기대수명 변화/역전(감소) 분석
#   - 국가 간 수렴/발산(불평등) 추세
#   - 회귀모형 lm(lifeExp ~ log(gdpPercap))으로 정량화
#   - Rosling 버블차트 등 시각화 보강

# ---- 0. 설정 및 유틸 ------------------------------------------------------
data_path <- file.path("data", "gapminder.csv")
fig_dir   <- "figures"
tab_dir   <- "tables"

if (!file.exists(data_path)) stop(sprintf("데이터 파일 없음: %s", data_path))
for (d in c(fig_dir, tab_dir)) if (!dir.exists(d)) dir.create(d)

df <- read.csv(data_path, stringsAsFactors = FALSE)
df$continent <- factor(df$continent)

# 외부 패키지 없이 쓰는 보조 함수
skewness <- function(x) { x <- x[!is.na(x)]; n <- length(x); m <- mean(x); s <- sd(x); (sum((x - m)^3) / n) / s^3 }
kurtosis <- function(x) { x <- x[!is.na(x)]; n <- length(x); m <- mean(x); s <- sd(x); (sum((x - m)^4) / n) / s^4 - 3 }
cagr     <- function(v_start, v_end, years) (v_end / v_start)^(1 / years) - 1  # 연평균 복리성장률
fmt      <- function(x, d = 2) format(round(x, d), big.mark = ",", scientific = FALSE)

section <- function(t) cat(sprintf("\n========== %s ==========\n", t))

cat("==========================================================\n")
cat(" gapminder 탐색적 데이터 분석(EDA) - 최종 보강판\n")
cat("==========================================================\n")

# ---- 1. 데이터 무결성 재확인 ---------------------------------------------
section("1. 데이터 무결성 재확인")
cat(sprintf("관측치: %d행 x %d열 | 기간: %d~%d (%d개 연도) | 국가: %d | 대륙: %d\n",
            nrow(df), ncol(df), min(df$year), max(df$year),
            length(unique(df$year)), length(unique(df$country)), nlevels(df$continent)))
cat(sprintf("결측치 총합: %d | 완전중복: %d | (country,year)키 중복: %d\n",
            sum(is.na(df)), sum(duplicated(df)), sum(duplicated(df[c("country", "year")]))))

# ---- 2. 분포 형태: 왜도/첨도 및 로그변환 ---------------------------------
section("2. 분포 형태(왜도/첨도) - 단순 요약통계가 숨기는 비대칭성")
shape <- data.frame(
  변수    = c("pop", "lifeExp", "gdpPercap"),
  평균    = sapply(c("pop", "lifeExp", "gdpPercap"), function(c) mean(df[[c]])),
  중앙값  = sapply(c("pop", "lifeExp", "gdpPercap"), function(c) median(df[[c]])),
  왜도    = sapply(c("pop", "lifeExp", "gdpPercap"), function(c) skewness(df[[c]])),
  초과첨도 = sapply(c("pop", "lifeExp", "gdpPercap"), function(c) kurtosis(df[[c]]))
)
shape[, -1] <- lapply(shape[, -1], round, 3)
print(shape, row.names = FALSE)
cat("로그변환 후 왜도:  log(pop)=", round(skewness(log(df$pop)), 3),
    " | log(gdpPercap)=", round(skewness(log(df$gdpPercap)), 3), "\n", sep = "")
cat("-> pop, gdpPercap은 강한 우편향(로그정규에 가까움). 평균보다 중앙값/로그척도가 적절.\n")

# ---- 3. 이상치 탐지 (IQR 규칙) -------------------------------------------
section("3. 이상치 탐지 (IQR 1.5배 규칙, 전체 연도 기준)")
for (col in c("lifeExp", "gdpPercap", "pop")) {
  q <- quantile(df[[col]], c(.25, .75)); iqr <- diff(q)
  lo <- q[1] - 1.5 * iqr; hi <- q[2] + 1.5 * iqr
  out <- df[df[[col]] < lo | df[[col]] > hi, ]
  cat(sprintf("- %-10s 이상치 %d건 (경계: %s ~ %s)\n", col, nrow(out), fmt(lo), fmt(hi)))
  if (nrow(out) > 0) {
    top <- out[order(-out[[col]]), ][seq_len(min(3, nrow(out))), c("country", "year", col)]
    cat("    상위 예:", paste(sprintf("%s(%d)", top$country, top$year), collapse = ", "), "\n")
  }
}

# ---- 4. 단순평균 vs 인구가중 평균 (핵심 보정) ----------------------------
section("4. 단순평균 vs 인구가중 평균 - '평균의 함정' 보정")
simple <- aggregate(cbind(lifeExp, gdpPercap) ~ year, data = df, FUN = mean)
weighted <- do.call(rbind, lapply(split(df, df$year), function(d) data.frame(
  year = d$year[1],
  lifeExp_w = weighted.mean(d$lifeExp, d$pop),
  gdpPercap_w = weighted.mean(d$gdpPercap, d$pop)
)))
comp <- merge(simple, weighted, by = "year")
comp <- data.frame(year = comp$year,
                   lifeExp_단순 = round(comp$lifeExp, 1), lifeExp_가중 = round(comp$lifeExp_w, 1),
                   gdp_단순 = round(comp$gdpPercap, 0), gdp_가중 = round(comp$gdpPercap_w, 0))
print(comp, row.names = FALSE)
cat(sprintf("-> 2007년 기대수명: 단순 %.1f vs 인구가중 %.1f (차이 %.1f세)\n",
            tail(comp$lifeExp_단순, 1), tail(comp$lifeExp_가중, 1),
            tail(comp$lifeExp_단순, 1) - tail(comp$lifeExp_가중, 1)))
cat("   인구가중이 낮음 = 인구 많은 국가(중국·인도 등)가 상대적으로 낮은 수명을 가짐.\n")
write.csv(comp, file.path(tab_dir, "weighted_vs_simple_by_year.csv"), row.names = FALSE)

# ---- 5. 국가별 성장(CAGR) 및 기대수명 변화/역전 -------------------------
section("5. 국가별 변화(1952->2007): 성장과 역전")
first <- df[df$year == min(df$year), ]
last  <- df[df$year == max(df$year), ]
chg <- merge(first[c("country", "continent", "lifeExp", "gdpPercap")],
             last[c("country", "lifeExp", "gdpPercap")], by = "country",
             suffixes = c("_1952", "_2007"))
chg$lifeExp_증감 <- chg$lifeExp_2007 - chg$lifeExp_1952
chg$gdp_CAGR <- cagr(chg$gdpPercap_1952, chg$gdpPercap_2007, 55)

cat("[GDP 연평균성장률(CAGR) 상위 5개국]\n")
print(head(chg[order(-chg$gdp_CAGR), c("country", "continent", "gdp_CAGR")], 5) |>
        within(gdp_CAGR <- sprintf("%.2f%%", gdp_CAGR * 100)), row.names = FALSE)
cat("[GDP 연평균성장률(CAGR) 하위 5개국]\n")
print(head(chg[order(chg$gdp_CAGR), c("country", "continent", "gdp_CAGR")], 5) |>
        within(gdp_CAGR <- sprintf("%.2f%%", gdp_CAGR * 100)), row.names = FALSE)

decl <- chg[chg$lifeExp_증감 < 0, c("country", "continent", "lifeExp_1952", "lifeExp_2007", "lifeExp_증감")]
decl <- decl[order(decl$lifeExp_증감), ]
cat(sprintf("\n[기대수명이 1952년보다 *감소*한 국가: %d개]\n", nrow(decl)))
if (nrow(decl) > 0) print(decl, row.names = FALSE) else cat("  없음\n")
cat("-> 전반적 향상 속에서도 일부 국가는 후퇴(에이즈/분쟁/체제붕괴 등)했음을 단순평균은 가림.\n")
write.csv(chg, file.path(tab_dir, "country_change_1952_2007.csv"), row.names = FALSE)

# ---- 6. 수렴 vs 발산: 국가 간 격차 추세 ---------------------------------
section("6. 국가 간 격차 추세 (수렴/발산)")
disp <- do.call(rbind, lapply(split(df, df$year), function(d) data.frame(
  year = d$year[1],
  lifeExp_표준편차 = sd(d$lifeExp),
  gdp_표준편차 = sd(d$gdpPercap),
  gdp_빈부격차배수 = max(d$gdpPercap) / min(d$gdpPercap)   # 최고/최저 비율
)))
disp[, -1] <- round(disp[, -1], 1)
print(disp, row.names = FALSE)
cat("-> 기대수명 표준편차는 감소(수렴)하나, GDP 빈부격차 배수는 확대(발산) 경향.\n")
write.csv(disp, file.path(tab_dir, "dispersion_by_year.csv"), row.names = FALSE)

# ---- 7. 정량 모델: 회귀분석 ---------------------------------------------
section("7. 회귀모형 - lifeExp ~ log(gdpPercap)")
m1 <- lm(lifeExp ~ log(gdpPercap), data = df)
s1 <- summary(m1)
cat(sprintf("lifeExp = %.2f + %.2f * log(gdpPercap)\n", coef(m1)[1], coef(m1)[2]))
cat(sprintf("R^2 = %.3f  (log GDP가 기대수명 분산의 %.1f%% 설명)\n",
            s1$r.squared, 100 * s1$r.squared))
m2 <- lm(lifeExp ~ log(gdpPercap) + continent + year, data = df)
cat(sprintf("대륙+연도 통제 시 R^2 = %.3f\n", summary(m2)$r.squared))
cat(sprintf("선형 상관 lifeExp~gdpPercap=%.3f, 로그변환 후=%.3f\n",
            cor(df$lifeExp, df$gdpPercap), cor(df$lifeExp, log(df$gdpPercap))))

# ---- 8. 대륙별 요약 (인구가중) ------------------------------------------
section("8. 대륙별 요약 (2007, 인구가중 평균)")
d07 <- df[df$year == 2007, ]
cont07 <- do.call(rbind, lapply(split(d07, d07$continent), function(d) data.frame(
  continent = as.character(d$continent[1]),
  국가수 = nrow(d),
  총인구 = sum(d$pop),
  기대수명_가중 = round(weighted.mean(d$lifeExp, d$pop), 1),
  GDP_가중 = round(weighted.mean(d$gdpPercap, d$pop), 0)
)))
print(cont07, row.names = FALSE)
write.csv(cont07, file.path(tab_dir, "continent_summary_2007.csv"), row.names = FALSE)

# ---- 9. 시각화 ----------------------------------------------------------
section("9. 시각화 저장 -> figures/")
cont_levels <- levels(df$continent)
pal <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
cont_col <- setNames(pal[seq_along(cont_levels)], cont_levels)

# 9-1. gdpPercap 원척도 vs 로그척도 히스토그램 (우편향 시각화)
png(file.path(fig_dir, "01_gdp_raw_vs_log.png"), width = 1000, height = 450)
par(mfrow = c(1, 2))
hist(df$gdpPercap, breaks = 40, col = "tomato", border = "white",
     main = "gdpPercap (원척도) - 우편향", xlab = "gdpPercap")
hist(log10(df$gdpPercap), breaks = 40, col = "steelblue", border = "white",
     main = "log10(gdpPercap) - 정규화", xlab = "log10(gdpPercap)")
par(mfrow = c(1, 1))
dev.off()

# 9-2. 단순 vs 인구가중 평균 기대수명 추세
png(file.path(fig_dir, "02_simple_vs_weighted_lifeExp.png"), width = 800, height = 600)
plot(comp$year, comp$lifeExp_단순, type = "b", pch = 19, col = "gray40", ylim = range(c(comp$lifeExp_단순, comp$lifeExp_가중)),
     main = "전세계 평균 기대수명: 단순 vs 인구가중", xlab = "year", ylab = "lifeExp")
lines(comp$year, comp$lifeExp_가중, type = "b", pch = 17, col = "darkred", lwd = 2)
legend("bottomright", c("단순평균", "인구가중평균"), col = c("gray40", "darkred"), pch = c(19, 17), lwd = c(1, 2), bty = "n")
dev.off()

# 9-3. Rosling 버블차트 (2007): x=GDP(log), y=lifeExp, 크기=pop, 색=대륙
png(file.path(fig_dir, "03_rosling_bubble_2007.png"), width = 900, height = 650)
ord <- order(-d07$pop)  # 큰 버블 먼저 그려 작은 버블이 위로
bubble_cex <- 1 + 8 * sqrt(d07$pop[ord]) / sqrt(max(d07$pop))  # 인구 sqrt 비례 크기
plot(d07$gdpPercap[ord], d07$lifeExp[ord], log = "x", pch = 21,
     cex = bubble_cex, col = "white",
     bg = adjustcolor(cont_col[as.character(d07$continent[ord])], 0.7),
     main = "Rosling 버블차트 (2007): GDP vs 기대수명 (버블=인구)",
     xlab = "gdpPercap (log scale)", ylab = "lifeExp")
legend("bottomright", legend = cont_levels, pt.bg = cont_col, pch = 21, col = "white", bty = "n")
dev.off()

# 9-4. 대륙별 기대수명 분포 변화 (1952 vs 2007 박스플롯 비교)
png(file.path(fig_dir, "04_lifeExp_dist_shift.png"), width = 900, height = 600)
df$grp <- interaction(df$continent, ifelse(df$year == 1952, "1952", ifelse(df$year == 2007, "2007", NA)))
sub <- df[df$year %in% c(1952, 2007), ]
boxplot(lifeExp ~ year + continent, data = sub, col = c("lightgray", "tomato"),
        las = 2, main = "대륙별 기대수명 분포: 1952 vs 2007", xlab = "", ylab = "lifeExp")
legend("topleft", c("1952", "2007"), fill = c("lightgray", "tomato"), bty = "n")
dev.off()

# 9-5. 국가 간 격차 추세 (수렴/발산)
png(file.path(fig_dir, "05_dispersion_trend.png"), width = 900, height = 450)
par(mfrow = c(1, 2))
plot(disp$year, disp$lifeExp_표준편차, type = "b", pch = 19, col = "darkgreen",
     main = "기대수명 국가간 표준편차 (수렴)", xlab = "year", ylab = "SD of lifeExp")
plot(disp$year, disp$gdp_빈부격차배수, type = "b", pch = 19, col = "darkred",
     main = "GDP 최고/최저 배수 (발산)", xlab = "year", ylab = "max/min ratio")
par(mfrow = c(1, 1))
dev.off()

# 9-6. 상관 산점도 + 회귀선
png(file.path(fig_dir, "06_lifeExp_vs_logGDP_fit.png"), width = 800, height = 600)
plot(df$gdpPercap, df$lifeExp, log = "x", col = adjustcolor(cont_col[as.character(df$continent)], 0.5),
     pch = 19, cex = 0.6, main = sprintf("lifeExp vs log(GDP) | R^2=%.3f", s1$r.squared),
     xlab = "gdpPercap (log)", ylab = "lifeExp")
xs <- seq(min(df$gdpPercap), max(df$gdpPercap), length.out = 200)
lines(xs, predict(m1, newdata = data.frame(gdpPercap = xs)), col = "black", lwd = 2)
legend("bottomright", legend = cont_levels, col = cont_col, pch = 19, bty = "n")
dev.off()

cat("저장: 01_gdp_raw_vs_log, 02_simple_vs_weighted_lifeExp, 03_rosling_bubble_2007,\n")
cat("      04_lifeExp_dist_shift, 05_dispersion_trend, 06_lifeExp_vs_logGDP_fit (.png)\n")
cat("표 저장 -> tables/: weighted_vs_simple_by_year, country_change_1952_2007,\n")
cat("                    dispersion_by_year, continent_summary_2007 (.csv)\n")

section("EDA 완료")
cat("R 버전:", R.version.string, "\n")
