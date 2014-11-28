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
 v1a = g 4 hn v :+: g 4 qn v :+: lmap(fd en) [e 4, f 4] :+: lmap(fd en) [g 4, f 4, e 4, c 4] 
 v1b = d 4 hn v :+: d 4 qn v :+: lmap(fd en) [d 4, e 4, f 4, e 4, d 4, c 4]:+:lmap(fd qn) [d 4 , e 4]
 v1 = v1a :+: v1b

 v2a = c 5 hn v :+: lmap(fd en) [g 4, a 4] :+: lmap(fd qn) [c 5, e 5] :+: d 5 hn v :+: lmap(fd en) [b 5, a 5] :+: lmap(fd qn) [g 5, f 5]
 v2b = e 5 hn v :+: lmap(fd en) [e 5, e 5, f 5, g 5, f 5] :+: e 5 hn v 

 mainVoice = v1a 
-- Putting it all together:
 twinkle = Instr "piano" (mainVoice)
 progression1 = [(C,wn),(F,hn),(C,hn),(G,hn),(C,hn),(G,hn),(C,hn)]
 progression2 = [(C,hn),(G,hn),(C,hn),(G,hn),(C,hn),(G,hn),(C,hn),(G,hn)]
 progression = progression1 ++ progression2 ++ progression1
 twinkleBasic   = twinkle :=: autoComp Basic (C, Major) progression
 twinkleCalypso = twinkle :=: autoComp Calypso (C, Major) progression
 twinkleBoogie  = twinkle :=: autoComp Boogie (C, Major) progression