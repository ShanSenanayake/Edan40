 module Mystery where
 import Haskore
 import AutoComp
 
 -- note updaters for mappings
 fd d n = n d v
 vol  n = n   v
 v      = [Volume 80]
 lmap f l = line (map f l)

 -- repeat something n times
 times 1 m = m
 times n m = m :+: (times (n - 1) m)





 -- Main Voice:
 --v1a = g 4 hn v :+: g 4 qn v :+: lmap(fd en) [e 4, f 4] :+: lmap(fd en) [g 4, f 4, e 4, c 4] 
 --v1b = d 4 hn v :+: d 4 qn v :+: lmap(fd en) [d 4, e 4, f 4, e 4, d 4, c 4]:+:lmap(fd qn) [d 4 , e 4]
 --v1 = v1a :+: v1b

 --v2a = c 5 hn v :+: lmap(fd en) [g 4, a 4] :+: lmap(fd qn) [c 5, e 5] :+: d 5 hn v :+: lmap(fd en) [b 5, a 5] :+: lmap(fd qn) [g 5, f 5]
 --v2b = e 5 hn v :+: lmap(fd en) [e 5, e 5, f 5, g 5, f 5] :+: e 5 hn v :+: d 5 en v :+: d 5 qn v
 --v2 = v2a :+: v2b
 --mainVoice = (times 2 v1):+:(times 2 v2):+:(times 2 v1)

 v1a = lmap(fd qn) [g 4, g 4, g 4] :+: ef 4 den v :+: as 4 en v:+: g 4 qn v :+: ef 4 den v :+: as 4 en v:+: g 4 hn v
 v2a = lmap(fd qn) [d 5, d 5, d 5] :+:ef 5 den v :+: bf 4 en v :+: gf 4 qn v :+: ef 4 den v :+: as 4 en v :+: g 4 hn v



-- Putting it all together:
 --mystery = Instr "piano" (mainVoice)
 --progression1 = [(C,bn),(G,bn),(C,bn),(G,bn)]
 --progression2 = [(C,bn),(G,bn),(C,bn),(G,bn),(C,bn),(G,bn),(C,bn),(G,bn)]
 --progression = progression1 ++ progression2 ++ progression1
 --mysteryBasic   = mystery :=: autoComp Basic (C, Major) progression
 --mysteryCalypso = mystery :=: autoComp Calypso (C, Major) progression
 --mysteryBoogie  = mystery :=: autoComp Boogie (C, Major) progression