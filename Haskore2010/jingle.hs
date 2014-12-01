 module JingleBells where
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
 v1a = lmap(fd qn) [d 4, b 4, a 4, g 4] :+: d 4 dhn v :+:lmap(fd en) [d 4, d 4] :+: lmap(fd qn) [d 4, b 4, a 4, g 4]:+: e 4 wn v
 v1b = lmap(fd qn) [e 4, c 5, b 4, a 4] :+: fs 4 wn v :+:(fs 4 qn v :=: d 5 qn v) :+: lmap(fd qn) [c 5, a 4]:+: (g 4 wn v :=: b 4 wn v)
 v1c = lmap(fd qn) [d 4, b 4, a 4, g 4] :+: d 4 wn v :+: lmap(fd qn) [d 4, b 4, a 4, g 4]:+: e 4 dhn v :+: e 4 qn v 
 v1d = lmap(fd qn) [e 4, c 5, b 4, a 4, d 5, d 5, d 5, d 5, e 5, d 5, c 5, a 4, g 4] :+: qnr :+: d 5 hn v
 v1 = v1a :+: v1b :+: v1c :+: v1d

 c1a = lmap(fd qn) [b 4, b 4] :+: b 4 hn v
 c1b = lmap(fd qn) [b 4, d 5] :+: g 4 dqn v :+: a 4 en v :+: b 4 wn v
 c1c = lmap(fd qn) [c 5, c 5] :+: c 5 dqn v :+:d 5 en v  :+: lmap (fd qn) [c 5, b 4] :+: b 4 dqn v :+: c 5 en v  :+: lmap (fd qn) [b 4, a 4, a 4, b 4] :+: lmap (fd hn) [a 4, d 5]
 c1d = lmap(fd qn) [c 5, c 5] :+: c 5 dqn v :+:d 5 en v  :+: lmap (fd qn) [c 5, b 4, b 4, c 5, d 5 , d 5, c 5, a 4] :+: g 4 wn v
 c1 = (times 2 c1a) :+: c1b :+: c1c :+: (times 2 c1a) :+: c1b :+: c1d
 mainVoice = v1 :+: c1
-- Putting it all together:
 jingle = Instr "piano" (mainVoice)
 progression1 = [(G,bn),(G,wn),(C,bn),(D,bn)]
 progression2 = [(G,hn),(D,hn)]
 progression3 = [(G,wn),(G,wn),(G,bn)]
 progression4 = [(D,bn),(A,wn),(D,wn)]
 progression5 = [(D,wn),(G,wn),(D,wn),(G,wn)]
 progression = progression1 ++ progression1 ++ progression2 ++ progression3 ++ progression4 ++ progression3 ++ progression4
 jingleBasic   = Tempo 4 (jingle :=: autoComp Basic (G, Major) progression)
 jingleCalypso = Tempo 4 (jingle :=: autoComp Calypso (G, Major) progression)
 jingleBoogie  = Tempo 4 (jingle :=: autoComp Boogie (G, Major) progression)