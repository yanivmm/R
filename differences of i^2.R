m=matrix(0, nrow=100,ncol=1)
y=0
z=0

for (x in 1:100)
{
  
  z=z+x^2
  
  m[x,1]=z

}
m

a=rep(0,99)
for (x1 in 1:99)
{
  a[x1]=m[(x1+1),1]-m[x1,1]
}
r=cbind(m,a)


b=rep(0,98)
for (x2 in 1:98)
{
  b[x2]=r[(x2+1),2]-r[x2,2]
}
s=cbind(r,b)

c=rep(0,97)
for (x3 in 1:97)
{
  c[x3]=s[(x3+1),3]-s[x3,3]
}
t=cbind(s,c)