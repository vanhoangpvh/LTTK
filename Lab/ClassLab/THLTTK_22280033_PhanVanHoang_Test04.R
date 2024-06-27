# Phan Van Hoang
# 22280033

# Bai 1
# (1.1)
# H0: p = 0.5
# H1: p < 0.5
donggoi = rep(c(0.95,0.97,0.99,1.01,1.03,1.05),c(9,31,40,15,3,2))
khong = donggoi[donggoi>1.01|donggoi<0.99]

khongdonggoichinhxacTest = prop.test(x = length(khong), n = 100, alternative = "less", p = 0.5)
print(khongdonggoichinhxacTest$p.value)
# p-value = 0.1841 > alpha = 0.05
# KL: Khong du co so de bac bo H0 -> chap nhan H0 -> p = 0.5

# (1.2)
# H0: mu = 1 kg
# H1: mu != 1kg
hoatdongbinhthuongTest = t.test(donggoi, alternative = "two.sided", mu = 1)
print(hoatdongbinhthuongTest$p.value)
# p-value = 4.522419e-10 < alpha = 0.01
# KL: Bac bo H0, may hoat dong khong binh thuong


# Bai 2
# H0: mu = 75
# h1: mu != 75
diemthi = rep(c(30,65,75,85,95),c(12,36,90,44,18))
diemthiTest = t.test(diemthi, alternative = "two.sided", mu = 75)
print(diemthiTest$p.value)
# p-value = 0.6161 > alpha = 0.05
# KL: Khong du co so de bac bo H0 -> chap nhan H0 -> mu = 75


# Bai 3
# H0: viec chon hang ve doc lap voi chuyen bay la noi dia hay quoc te
# H1: viec chon hang ve phu thuoc voi chuyen bay la noi dia hay quoc te
maybay = data.frame(
  noi_dia = c(29,95,518),
  quoc_te = c(22,121,135)
)

row.names(maybay) = c("hang_thuong","hang_trung","hang_doanh_nhan")
maybayTest = chisq.test(maybay)
print(maybayTest$p.value)
# p-value = 1.552861e-22 < alpha = 0.01
# KL: Bac bo H0, viec chon hang ve phu thuoc voi chuyen bay la noi dia hay quoc te co y nghia thong ke


install.packages("pcalg")

