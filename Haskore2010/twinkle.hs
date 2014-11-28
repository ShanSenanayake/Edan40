 module Twinkle where
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
 v1a = lmap (fd qn) [c 5, c 5, g 5, g 5, a 5, a 5] :+: g 5 hn v :+: lmap (fd qn) [f 5, f 5, e 5 , e 5, d 5, d 5] :+: c 5 hn v 
 v1b = lmap (fd qn)    [g 5, g 5, f 5, f 5, e 5, e 5] :+: d 5 hn v

 mainVoice = v1a :+: (times 2 v1b) :+: v1a

-- Putting it all together:
 twinkle = Instr "piano" (mainVoice)
 progression1 = [(C,wn),(F,hn),(C,hn),(G,hn),(C,hn),(G,hn),(C,hn)]
 progression2 = [(C,hn),(G,hn),(C,hn),(G,hn),(C,hn),(G,hn),(C,hn),(G,hn)]
 progression = progression1 ++ progression2 ++ progression1
 twinkleBasic   = twinkle :=: autoComp Basic (C, Major) progression
 twinkleCalypso = twinkle :=: autoComp Calypso (C, Major) progression
 twinkleBoogie  = twinkle :=: autoComp Boogie (C, Major) progression