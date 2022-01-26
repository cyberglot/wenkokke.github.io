<p>Quite a while ago, <a href="https://github.com/UnicornPower">UnicornPower</a> introduced me to a game called <em>Breadbox</em>. It’s an experimental cousin of <em>20 Questions</em>, also known as <em>Plenty Questions</em>, which is played by two players—or more, really—who we’ll name Allie and Blake:</p>
<ul>
<li>Allie thinks of something.</li>
<li>As their first question, Blake asks <em>“Is it a breadbox?”</em></li>
<li>Allie—who, seeing the mandatory first question, obviously wouldn’t choose a breadbox—answers <em>“No, it’s not!”</em></li>
</ul>
<p>From there on out, all Blake’s questions have to be of the form…</p>
<ul>
<li><em>“Is it more like a breadbox, or more like…?”</em></li>
</ul>
<p>…where <em>breadbox</em> is replaced by whatever the current guess is, and the dots are filled in with whatever Blake wants. Let’s see if we can write an AI for playing this game!</p>
<!--more-->
<p>Okay, before we get started, let’s look at an example of a quick game:</p>
<dl>
<dt>Allie</dt>
<dd>I’m thinking of something…
</dd>
<dt>Blake</dt>
<dd>Is it a breadbox?
</dd>
<dt>Allie</dt>
<dd>No, it’s not.
</dd>
<dt>Blake</dt>
<dd>Is it more like a breadbox or more like a dog?
</dd>
<dt>Allie</dt>
<dd>It’s more like a dog…
</dd>
<dt>Blake</dt>
<dd>Is it more like a dog or more like a cat?
</dd>
<dt>Allie</dt>
<dd>It’s more like a cat…
</dd>
<dt>Blake</dt>
<dd>Is it more like a cat or more like a unicorn?
</dd>
<dt>Allie</dt>
<dd>It’s more like a cat…
</dd>
<dt>Blake</dt>
<dd>Is it more like a cat or more like a garden?
</dd>
<dt>Allie</dt>
<dd>It’s more like a garden…
</dd>
<dt>Blake</dt>
<dd>Is it more like a garden or more like a house?
</dd>
<dt>Allie</dt>
<dd>It’s more like a house…
</dd>
<dt>Blake</dt>
<dd>Is it more like a house or more like a friend?
</dd>
<dt>Allie</dt>
<dd>It’s more like a friend…
</dd>
<dt>Blake</dt>
<dd>Is it more like a friend or more like a lover?
</dd>
<dt>Allie</dt>
<dd>It’s more like a friend…
</dd>
<dt>Blake</dt>
<dd>Is it more like a friend or more like a relative?
</dd>
<dt>Allie</dt>
<dd>It’s more like a friend…
</dd>
<dt>Blake</dt>
<dd>Is it more like a friend or more like a neighbour?
</dd>
<dt>Allie</dt>
<dd>That’s exactly what I was thinking of!
</dd>
</dl>
<p>Since this game tends to bring out the… best in people, Blake might just have to give up. After this, Allie may be called on to explain what the word she chose even means:</p>
<dl>
<dt>Blake</dt>
<dd>I give up… what was it?
</dd>
<dt>Allie</dt>
<dd>Reverberation!
</dd>
<dt>Allie</dt>
<dd>It’s the repetition of a sound resulting from reflection of the sound waves!
</dd>
</dl>
<p>The game does a <em>fantastic</em> job of revealing how hierarchical people tend to think. For instance, I would say that a dolphin is more like an orca than it is like a mammal, <em>even though a dolphin isn’t an orca</em>. But while playing the game, I often find myself slipping into hierarchical thinking: “Oh, it’s not like an animal… so we can exclude all animals.”</p>
<p>It is exactly this—the fact that the game forces you to consider a distance metric for your internal ontology, insofar as one exists—which makes it so fascinating to play. I heartily recommend you try it!</p>
<p>If you’re upset because Breadbox is hard, or because you think that the choices it makes are weird or wrong… try playing it with a person!</p>
<hr />
<blockquote>
<p><em>Hiya! It’s Wen from the future, here to tell you that I used to have a copy of Breadbox running on OpenShift, where you could try it out for yourself. Unfortunately, that died with the deprecation of OpenShift 2, and I haven’t been able to find anywhere else that would let me host a 2GB web app for free…</em><a href="#fn1" class="footnote-ref" id="fnref1" role="doc-noteref"><sup>1</sup></a></p>
</blockquote>
<hr />
<p>In the summer of 2014, I took a course on distributional semantics (by Marco Baroni and Georgiana Dinu) and the first thing I thought to do was to use their [dataset][semantic-vectors] to implement an AI for Breadbox. <del>Try it here!</del></p>
<p>So how does it work? The core hypothesis of distributional semantics is</p>
<blockquote>
<p>You can know the meaning of a word by the company it keeps</p>
</blockquote>
<p>Have a look at the two sentences below:<a href="#fn2" class="footnote-ref" id="fnref2" role="doc-noteref"><sup>2</sup></a></p>
<ol type="1">
<li>He filled the <em>wampimuk</em>, passed it around and we all drunk some.</li>
<li>We found a little, hairy <em>wampimuk</em> sleeping behind the tree.</li>
</ol>
<p>While you’ve probably never read the word wampimuk before, it’s likely that either sentence will give you a pretty clear idea of what a wampurnik is <em>in that context</em>.</p>
<p>So if you want to know what a word (e.g. “bear”) means, you take a <em>huge</em> corpus, and you look for all the occurances of that word…<a href="#fn3" class="footnote-ref" id="fnref3" role="doc-noteref"><sup>3</sup></a></p>
<pre style="margin-left: 3em;">
       over the mountains. A <a style="color:dark-orange;">bear</a> also features prominentl
    rnejakt&quot; (An Unfortunate <a style="color:dark-orange;">Bear</a> Hunt) by Theodor Kittels
       to his hagiography, a <a style="color:dark-orange;">bear</a> killed Saint Corbinian&#39;s
         however, he let the <a style="color:dark-orange;">bear</a> go. The saddled &quot;bear
       bear go. The saddled &quot;<a style="color:dark-orange;">bear</a> of St. Corbinian&quot; the
    tails on this topic, see <a style="color:dark-orange;">Bear</a> in heraldry. The British
         Cat and the Russian <a style="color:dark-orange;">Bear</a> (see The Great Game)
     Great Game) The Russian <a style="color:dark-orange;">bear</a> is a common national
    Soviet Union). The brown <a style="color:dark-orange;">bear</a> is also Finland&#39;s nation
    animals and had the same <a style="color:dark-orange;">bear</a> carry him from his hermi
         thus Christianised, <a style="color:dark-orange;">bear</a> clasping each gable
     evidence of prehistoric <a style="color:dark-orange;">bear</a> worship. Anthropologists
     peoples, considered the <a style="color:dark-orange;">bear</a> as the spirit of one&#39;s
    fathers. This is why the <a style="color:dark-orange;">bear</a> (karhu) was a greatly
    ikämmen and kontio). The <a style="color:dark-orange;">bear</a> is the national animal
      tries to kill a mother <a style="color:dark-orange;">bear</a> and her cubs—and is
         society. &quot;The Brown <a style="color:dark-orange;">Bear</a> of Norway&quot; is a Scottish
     magically turned into a <a style="color:dark-orange;">bear</a>, and who managed to get
     television. Evidence of <a style="color:dark-orange;">bear</a> worship has been found
      mythology identify the <a style="color:dark-orange;">bear</a> as their ancestor and
    shopric of Freising, the <a style="color:dark-orange;">bear</a> is the dangerous totem
</pre>
<p>…and then you count, for every other word, how often it occurs together with your word. The above text, for instance, gives us a number of obvious co-occurances for bear: mountain, kill, hunt, brown, animal and cub. The idea is that, given a large enough corpus, these co-occurances will drown out the noisier ones.</p>
<p>By doing this for <em>every</em> word, you build up a co-occurance matrix, which lists how often every word occurs with every other word. For instance,<a href="#fn4" class="footnote-ref" id="fnref4" role="doc-noteref"><sup>4</sup></a></p>
<table>
<thead>
<tr class="header">
<th></th>
<th style="text-align: left;">leash</th>
<th style="text-align: left;">walk</th>
<th style="text-align: left;">run</th>
<th style="text-align: left;">owner</th>
<th style="text-align: left;">pet</th>
<th style="text-align: left;">bark</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>dog</td>
<td style="text-align: left;">3</td>
<td style="text-align: left;">5</td>
<td style="text-align: left;">2</td>
<td style="text-align: left;">5</td>
<td style="text-align: left;">3</td>
<td style="text-align: left;">2</td>
</tr>
<tr class="even">
<td>cat</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">3</td>
<td style="text-align: left;">3</td>
<td style="text-align: left;">2</td>
<td style="text-align: left;">3</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="odd">
<td>lion</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">3</td>
<td style="text-align: left;">2</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="even">
<td>light</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="odd">
<td>bark</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">2</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="even">
<td>car</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">3</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">0</td>
</tr>
</tbody>
</table>
<p>Now the meaning for a word is determined by its co-occurances with some select group of words. For instance, if we look at ‘dog’ we see that it strongly co-occurs with things such as ‘leash’, ‘walk’, ‘owner’, ‘pet’ and ‘bark’. For ‘cat’, we lose ‘leash’ and ‘bark’—since cats don’t bark, and are rarely leashed. And for ‘lion’, we also lose ‘owner’ and ‘pet’—while a lion could conceivably be a pet, it’d be a lot rarer than having a cat or dog… and we could never really feel like we <em>owned</em> the lion.</p>
<p>You can think of the rows in this matrix as points, in a six dimensional space—one dimension for every column. And because it’s a space, you can measure the distance between words. And this is where we get back to Breadbox: in order to play this bizarre game, we needed a distance metric for meanings, to be able to compare and order <em>any two objects</em>. And this is <em>exactly</em> what a co-occurance matrix gives us!</p>
<p>Obviously, there’s a lot more to distributional semantics than just this. For instance, the matrices that you derive this way tend to be huge—one axis per word, one point per word—so there’s a whole bunch of work which goes into selecting exactly which set of words should be the axes. Then there’s the difficultly of <em>composing</em> word meanings. You may have noticed that my breadbox implementation doesn’t work all too well for compound nouns: that’s because I’m not taking the effort to compose meaning vectors.<a href="#fn5" class="footnote-ref" id="fnref5" role="doc-noteref"><sup>5</sup></a></p>
<p>If you wish to read more about distributional semantics, there’s a pretty good overview of introductions and surveys [here][slides]. Additionally, there’s a whole branch of work which uses neural networks to learn the word meanings: for instance, have a look at [Word2Vec][word2vec].</p>
<hr />
<section class="footnotes footnotes-end-of-document" role="doc-endnotes">
<hr />
<ol>
<li id="fn1" role="doc-endnote"><p>You can still try to run it [from source][breadbox], but be warned… I did not future-proof it. [DontCountPredict]: http://clic.cimec.unitn.it/marco/assetsations/acl2014/baroni-etal-countpredict-acl2014.pdf [word2vec]: http://deeplearning4j.org/word2vec [slides]: https://www.cs.utexas.edu/~mooney/cs388/slides/dist-sem-intro-NLP-class-UT.pdf [semantic-vectors]: http://clic.cimec.unitn.it/composes/semantic-vectors.html [breadbox]: https://github.com/wenkokke/Breadbox<a href="#fnref1" class="footnote-back" role="doc-backlink">↩︎</a></p></li>
<li id="fn2" role="doc-endnote"><p>Taken from <a href="https://www.cs.utexas.edu/~mooney/cs388/slides/dist-sem-intro-NLP-class-UT.pdf" class="uri">https://www.cs.utexas.edu/~mooney/cs388/slides/dist-sem-intro-NLP-class-UT.pdf</a>.<a href="#fnref2" class="footnote-back" role="doc-backlink">↩︎</a></p></li>
<li id="fn3" role="doc-endnote"><p>Taken from <a href="http://www.webcorp.org.uk" class="uri">http://www.webcorp.org.uk</a>.<a href="#fnref3" class="footnote-back" role="doc-backlink">↩︎</a></p></li>
<li id="fn4" role="doc-endnote"><p>Taken from <a href="https://www.cs.utexas.edu/~mooney/cs388/slides/dist-sem-intro-NLP-class-UT.pdf" class="uri">https://www.cs.utexas.edu/~mooney/cs388/slides/dist-sem-intro-NLP-class-UT.pdf</a>.<a href="#fnref4" class="footnote-back" role="doc-backlink">↩︎</a></p></li>
<li id="fn5" role="doc-endnote"><p>A bit of a shame, really, since the course I took was <em>about</em> composing meaning vectors. Also, full disclosure, the vectors that I used were created using [Word2Vec][word2vec], using neural networks. Such vectors generally outperform counting vectors in tasks of relatedness (see <em>[Don’t Count, Predict!][DontCountPredict]</em> by Baroni et al.).<a href="#fnref5" class="footnote-back" role="doc-backlink">↩︎</a></p></li>
</ol>
</section>