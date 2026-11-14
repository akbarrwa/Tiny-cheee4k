import os,json,urllib.request as R
NK=(33,31,18,14,-33,-31,-18,-14)
KG=(1,-1,16,-16,15,-15,17,-17)
MK={0:13,7:14,4:12,112:7,119:11,116:3}
Q=(5,4,3,2)
B=[0]*128;K=[0,0];S=1;C=0;E=-1;H=0
T={};HST=[];N=[0];BUD=50000
A=Exception
def G(t,by):
 for o in NK:
  x=t+o
  if not x&0x88 and B[x]==by*2:return 1
 for i,o in enumerate(KG):
  x=t+o
  while not x&0x88:
   q=B[x]
   if q:
    if q*by>0 and(abs(q)in(5,4-(i>3))or(abs(q)==6 and x==t+o)):return 1
    break
   x+=o
 for x in(t-16*by-1,t-16*by+1):
  if not x&0x88 and B[x]==by:return 1
 return 0
def J(cp):
 b=B;s=S;m=[]
 for q,p in enumerate(b):
  if q&0x88 or p*s<=0:continue
  a=abs(p)
  if a==1:
   d=16*s;t=q+d;pr=(q>>4)==6-5*(s<0)
   if not b[t]:
    if pr:m+=[q|t<<7|v<<14 for v in Q]
    elif not cp:
     m.append(q|t<<7)
     if(q>>4)==1+5*(s<0)and not b[t+d]:m.append(q|t+d<<7)
   for t in(q+d-1,q+d+1):
    if t&0x88:continue
    if b[t]*s<0 or t==E:
     if pr:m+=[q|t<<7|v<<14 for v in Q]
     else:m.append(q|t<<7)
  else:
   for o in(NK,KG[4:],KG[:4],KG,KG)[a-2]:
    t=q+o
    while not t&0x88:
     if b[t]:
      if b[t]*s<0:m.append(q|t<<7)
      break
     if not cp:m.append(q|t<<7)
     if a in(2,6):break
     t+=o
 if not cp:
  z=112*(s<0);w=4-3*(s>0)
  for k in(1,-1):
   if C&w*(1+(k<0))and not(b[z+4+k]|b[z+4+2*k])and(k>0 or not b[z+4+3*k])and not(G(z+4,-s)|G(z+4+k,-s)|G(z+4+2*k,-s)):m.append(z+4|z+4+2*k<<7)
 return m
def M(m):
 global S,C,E,H
 f=m&127;t=m>>7&127;p=m>>14;u=(B[:],S,C,E,H,K[:]);pc=B[f]
 if pc*pc==1 and t==E and not B[t]:B[t-16*S]=0
 if abs(pc)==6 and abs(t-f)==2:
  rf=f+3-7*(t<f);rt=f+1-2*(t<f);B[rt]=B[rf];B[rf]=0
 B[t]=p*S or pc;B[f]=0
 if abs(pc)==6:K[S<0]=t
 for s in(f,t):C&=MK.get(s,15)
 E=f+16*S if pc*pc==1 and abs(t-f)==32 else-1
 H=0 if pc*pc==1 or u[0][t]else H+1
 S=-S;return u
def W(u):
 global S,C,E,H
 B[:],S,C,E,H,K[:]=u
def Y():return(tuple(B),S,C,E)
def D():
 s=0;wb=0;bb=0;n=0;k=0;c=0;u=0
 for q,p in enumerate(B):
  if q&0x88 or not p:continue
  a=abs(p);f=q&7;r=q>>4^7*(p<0);d=abs(f-3)+abs(r-3)
  if a==1:v=100+8*r;u=1
  elif a==2:v=340-3*d;n+=1;k+=1
  elif a==3:v=338-d;n+=1;c|=1<<(f+(q>>4)&1);wb+=p>0;bb+=p<0
  elif a==4:v=500;u=1
  elif a==5:v=900;u=1
  else:v=10-2*r+abs(f-3)
  s+=v*p//a
 if not u and(n<2 or(not k and c in(1,2))):return 0
 s+=25*(wb>1)-25*(bb>1)
 return s*S
def O(ms,tm):
 def sc(m):
  t=m>>7&127;v=B[t];a=B[m&127]
  return(m==tm)*99999+(v!=0)*(10000+abs(v)*16-abs(a))+(m>>14)*100+(t==E and a*a==1)*9000
 ms.sort(key=sc,reverse=True);return ms
def X(d,a,b,pl):
 N[0]+=1
 if N[0]>BUD:raise A
 k=Y()
 if k in HST or H>99:return 0
 if d<1:
  st=D()
  if st>=b:return st
  if st>a:a=st
 tm=T.get(k,0)*(d>0);bs=-999999;bm=0
 for m in O(J(d<1),tm):
  u=M(m)
  if G(K[S>0],S):W(u);continue
  HST.append(k);sc=-X(max(d-1,0),-b,-a,pl+1);HST.pop();W(u)
  if sc>bs:bs=sc;bm=m
  if sc>a:a=sc
  if a>=b:break
 if d<1:return a
 if not bm:return-100000+pl if G(K[S<0],-S)else 0
 T[k]=bm
 return bs
def F():
 N[0]=0;del HST[:];ms=[]
 for m in J(0):
  u=M(m)
  if not G(K[S>0],S):ms.append(m)
  W(u)
 if len(ms)<2:return ms[0]if ms else 0
 ms=O(ms,0);best=ms[0];d=1
 while d<65:
  try:
   a=-999999;bs=-999999;bb=0
   for m in ms:
    u=M(m);sc=-X(d-1,-999999,-a,1);W(u)
    if sc>bs:bs=sc;bb=m
    if sc>a:a=sc
   best=bb;d+=1
  except A:break
 return best
def V(mv):
 global S,C,E,H
 Z=[4,2,3,5,6,3,2,4];B[:]=Z+[0]*8+[1]*8+[0]*72+[-1]*8+[0]*8+[-x for x in Z]+[0]*8
 K[:]=4,116;S=1;C=15;E=-1;H=0
 q=lambda i:(int(s[i+1])-1)*16+ord(s[i])-97
 for s in mv:
  f=q(0);t=q(2);p='nbrq'.find(s[4:])+2*(len(s)>4)
  M(f|t<<7|p<<14)
U=os.getenv('MAKEFUN_ENV_URL')
def P(p,d):
 q=R.Request(U+p,json.dumps(d).encode(),{'Content-Type':'application/json'});return json.load(R.urlopen(q))
while 1:
 g=P('/game',{})
 if g['status']=='done':break
 V(g['moves'])
 while g['status']=='your_move':
  l=g['legal_moves']
  if not l:g=P('/resign',{'game':g['game']});break
  m=F();f=m&127;t=m>>7&127;p=m>>14
  u=chr(97+f%8)+str(f//16+1)+chr(97+t%8)+str(t//16+1)+('nbrq'[p-2]if p else'')
  g=P('/move',{'game':g['game'],'move':u if u in l else l[0]})
  if g['status']=='your_move':V(g['moves'])
