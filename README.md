Musical-chord-progressions
==========================

An analysis of hooktheory's db, represented as a sankey plot
<p>This represents an visualization of all normalized chord progressions in the <a href="http://www.hooktheory.com">
hooktheory database</a>. The scales are normalized so that for example C -> G -> Am   is the same as A -> E -> Fm 
(what's important for this is the progression not the key. </p>
<p><b>Notes:</b> The database 'starts' with 29 chords. For every subsequent step the number of chords transitioned into
keeps growing (which is expected), but for the purpose of this visualization, I only keep the original 29 chords. Also, 
since the thickness of the lines I'm plotting are in and of themselves probabilities, it's not fully correct to treat
them the same. Very lazily, I just normalized all probabilities across each level so that it adds up to 1, I'm sure 
there's a better way. Community is invited to improve!
