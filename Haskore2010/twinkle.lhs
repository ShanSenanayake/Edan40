\section{Partial Encoding of Chick Corea's ``Children's Song No. 6''}
\label{chick}

{\small\begin{verbatim} 

> module Twinkle where
> import Haskore
> 
> -- note updaters for mappings
> fd d n = n d v
> vol  n = n   v
> v      = [Volume 80]
> lmap f l = line (map f l)
> 
> -- repeat something n times
> times  1    m = m
> times n m = m :+: (times (n - 1) m)
> 
>
> 
> 
> 
> -- Main Voice:
> v1  = v1a :+: v1b
> v1a = lmap (fd en) [c 5, c 5, g 5, g 5, a 5, a 5, g 5, f 5, f 5, e 5 , e 5, d 5, c 5]
> v1b = lmap (fd en)    [g 5, g 5, f 5, f 5, e 5, e 5, d 5, d 5, c 5]
> 
> v2  = v2a :+: v2b
> v2a = lmap vol [cs 5 (dhn+dhn), d 5 dhn, 
>                 f 5 hn, gs 5 qn, fs 5 (hn+en), g 5 en]
> v2b = lmap (fd en) [fs 5, e 5, cs 5, as 4] :+: a 4 dqn v :+:
>       lmap (fd en) [as 4, cs 5, fs 5, e 5, fs 5, g 5, as 5]
> v2c = lmap vol [cs 6 (hn+en), d 6 en, cs 6 en, e 5 en] :+: enr :+: 
>       lmap vol [as 5 en, a 5 en, g 5 en, d 5 qn, c 5 en, cs 5 en]
> v2d = lmap (fd en) [fs 5, cs 5, e 5, cs 5, a 4, as 4, d 5, e 5, fs 5] :+:
>       lmap vol [fs 5 tn, e 5 (qn-tn), d 5 en, e 5 tn, d 5 (qn-tn),
>                 cs 5 en, d 5 tn, cs 5 (qn-tn), b 4 (en+hn)]
> v2e = lmap vol [cs 5 en, b 4 en, fs 5 en, a 5 en, b 5 (hn+qn), a 5 en,
>                 fs 5 en, e 5 qn, d 5 en, fs 5 en, e 5 hn, d 5 hn, fs 5 qn]
> v2f = Tempo (3/2) (lmap vol [cs 5 en, d 5 en, cs 5 en]) :+: b 4 (3*dhn+hn) v
> 
> mainVoice = v1a :+: (times 2 v1b)
> 
> -- Putting it all together:
> twinkle = Instr "piano" (Tempo 1 (Phrase [Dyn SF] mainVoice))

\end{verbatim} }
