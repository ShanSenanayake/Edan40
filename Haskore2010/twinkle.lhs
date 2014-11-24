 module Twinkle where
 import Haskore
 
 -- note updaters for mappings
 fd d n = n d v
 vol  n = n   v
 v      = [Volume 80]
 lmap f l = line (map f l)

 -- repeat something n times
times  1    m = m
times n m = m :+: (times (n - 1) m)




 
 -- Main Voice:
v1a = lmap (fd en) [c 5, c 5, g 5, g 5, a 5, a 5] :+: g 5 qn v :+: lmap (fd en) [f 5, f 5, e 5 , e 5, d 5, d 5] :+: c 5 qn v
v1b = lmap (fd en)    [g 5, g 5, f 5, f 5, e 5, e 5] :+: d 5 qn v

mainVoice = v1a :+: (times 2 v1b) :+: v1a

-- Putting it all together:
twinkle = Instr "piano" (Tempo 1 (mainVoice))

progression1 = [(C,wn),(F,hn),(C,hn),(G,hn),(C,hn),(G,hn),(C,hn)]
progression2 = [(C,hn),(G,hn),(C,hn),(G,hn),(C,hn),(G,hn),(C,hn),(G,hn)]
progression = progression1 :+: progression2 :+: progression1