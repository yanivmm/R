# ����� ����� �� ������
chi=rep(0,4)

# ����� ���� �����
names = c("color","tires","doors","type")

# �������  ������� ����� (���� ������) �
total = list(c(4,4,6), c(6,3,5),c(7,7),c(6,8))

# ������� ���� ����� "������" �
observe = list(c(2,1,2,2,3,4), c(2,0,3,4,3,2),c(4,1,3,6), c(3,2,3,6))

for (i in 1:4) {
  expected = c(total[[i]]*(5/14),total[[i]]*(9/14)) #����� ���������� �� �����
  chi[i]=sum((expected-observe[[i]])^2/expected)}   #����� �� ������

data.frame(names,chi)