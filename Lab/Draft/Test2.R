data = rep(c(155,165,175,185,195,205),c(3,9,11,3,2,1))


choresterolTestTwoSided = t.test(data,alternative = "two.sided", conf.level = 0.99, mu = 175)
choresterolTest$conf.int

HutThuoc = c(124,131,134,133,136,125,125,118,133,127,135)
KhongHutThuoc = c(130,116,122,127,128,135,129,120,118,122,122,115,123)

huyetapTest = t.test(HutThuoc,KhongHutThuoc,alternative = "two.sided", var.equal = TRUE, mu = 0, conf.level = 0.95)
huyetapTest

huyetapTestLess = t.test(HutThuoc,KhongHutThuoc,alternative = "greater", var.equal = TRUE, mu = 0)
huyetapTestLess

huyetapTest$conf.int

