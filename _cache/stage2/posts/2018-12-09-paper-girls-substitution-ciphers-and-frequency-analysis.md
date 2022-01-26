<p><span class="font-papergirls"> WARNING! IF YOU’RE READING THIS, YOU’RE DOING IT WRONG. CLICK THE LINK, READ IT ON MY BLOG. IT’LL BE WAY LESs CONFUSING. ANYWAY. THIS WEeKEND, I PICKED UP A COPY OF PAPER GIRLS, A MYSTERY/SCIENCE FICTION COMIC ABOUT FOUR GIRLS ON A PAPER ROUTE. WAIT, YOU CAN’T UNDERSTAND ME? OH, FUCK, DOESN’T LOoK LIKE IT. SORrY, LET ME ADJUST. </span></p>
<!--more-->
<p>Ah, I’m so sorry, is this any better? Once again, so sorry, I think I’ve been reading this stuff for too long, think I got carried away… Anyway, I’ll take it from the start. So this weekend, I picked up a copy of <em><a href="https://imagecomics.com/comics/releases/paper-girls-1">Paper Girls</a></em>, a mystery/science fiction comic about four girls on a paper route…</p>
<p><img src="/assets/images/paper-girls-1.jpg" alt="Image from Paper Girls. MacKenzie, smoking, sits on her bicycle, while Erin, KJ, and Tiffany stand behind her." /><br />
</p>
<p>It starts simple. Our four heroes band together to deliver papers on Halloween morning. But it quickly escalates. Testament to this fact, thirty-five pages in we meet three time traveling teenagers, wrapped in black bandages, speaking in <em>these things</em>…</p>
<p><img src="/assets/images/paper-girls-2.png" alt="Image from Paper Girls. Two figures wrapped in dark bandages bend over their fallen friend. Their speech is written using strange glyphs." /><br />
</p>
<p>These glyphs are used for the rest of the book, and—best I can tell—the rest of the series without any real explanation… which leaves me with the following question:</p>
<blockquote>
<p>What in fuck’s name are they saying?</p>
</blockquote>
<p>Wanna guess what today’s topic is? Yep. Welcome to my class, “Help! These letters are all funky! (Part 1, Substitution Ciphers)”.</p>
<p>Now, I’ve done this before. Several years ago, I was given a copy of <em><a href="https://www.goodreads.com/book/show/11161195-johnny-23">Johnny 23</a></em> by Charles Burns which is, euh, a book put together using panels from <em><a href="https://www.goodreads.com/book/show/7814774-x-ed-out">X’ed Out</a></em>, with an entirely new “story” written in strange glyphs… Sound, well, semi-familiar at least?</p>
<p><img src="/assets/images/xed-out.jpg" alt="Image from X’ed Out. A person is shown a crying worm by a hooded figure with a wound for a nose. The hooded figure then eats the worm. Their speech is written using strange glyphs." /><br />
</p>
<p>It turns out that what Charles had done was basically a substitution cipher—you make up one crazy glyph for each letter of the alphabet, and then instead of writing letters from the alphabet, you use the alien glyphs you came up with…</p>
<p>Honestly, there’s some pretty good reasons to assume that <em>Paper Girls</em> does the same. Substitution ciphers are really easy to come up with—you basically have to come up with one weird glyph for ever letter in your alphabet. Plus, they get the job done! At least, it’s pretty clear to me that our time traveling teenagers are speaking some arcane language!</p>
<p>Substitution ciphers are pretty easy to solve, but compare that more complex ciphers. We could have the cipher evolve over time, say, shift those sinister glyphs by one every time we turn a page. But… do the characters know what page they’re on? And could you speak a language where the sounds you make to say, say <em>chair</em>, change every few minutes?</p>
<p>What about actual cryptographic ciphers? We might as well be putting random noise on the page! There’s no way any reader would crack anything like that, and no way any human could actually speak, say, <a href="https://en.wikipedia.org/wiki/RSA_(cryptosystem)">RSA-encrypted</a> English.</p>
<p>Ideally, you would construct an entirely new language. Constructed languages have a long and rich history, stretching from the 17th century search for the <a href="https://en.wikipedia.org/wiki/Solresol">Perfect Language</a> to the modern desire to make our <a href="https://en.wikipedia.org/wiki/Klingon_language">Bad Space Russians</a> and <a href="https://en.wikipedia.org/wiki/Valyrian_languages">Magic Romans</a> more compelling. However, constructing a languages takes a <em>serious</em> amount of time and effort, and heaps of linguistics knowledge, so you’ll probably only find this kind of stuff in <em>huge</em> and well-funded projects—or in the works of a guy who really just writes fantasy to justify <a href="https://en.wikipedia.org/wiki/Quenya">his conlanging hobby</a>.</p>
<p>All in all, ciphers aren’t the best way to emulate a new and unfamiliar language, but they are the cheapest… <em>“But how do I crack this stuff?!”</em>, you cry out in frustration. Right, good point. Let’s get back to today’s lecture!</p>
<h3 id="how-do-you-crack-this-stuff">How do you crack this stuff?</h3>
<p>First off, I’ll be writing some Python code in this section. Really, I’m just doing that ’cuz I’m bad at counting. You can do everything I’ll be doing by hand, with just some pen and paper.</p>
<p>The first thing you need to do is get some data. Grab your favourite comic with a cipher in there, and start writing down words. I’ll be using the first five issues of <em>Paper Girls</em> for this, because that’s how far I’ve read.</p>
<pre><code class="highlight"><span class="n">data</span> <span class="o">=</span> <span class="s">&quot;</span><span class="se">\n</span><span class="s">&quot;</span><span class="o">.</span><span class="n">join</span><span class="p">([</span>
    <span class="s font-papergirls">&quot;GODdAMNIT.&quot;,</span>
    <span class="s font-papergirls">&quot;SPLIT UP. WE&#39;Ll MEeT AT THE SECOND FOLDING.&quot;,</span>
    <span class="s font-papergirls">&quot;STAY AWAY FROM ME!&quot;,</span>
    <span class="s font-papergirls">&quot;STOP!&quot;,</span>
    <span class="s font-papergirls">&quot;PLEASE!&quot;,</span>
    <span class="s font-papergirls">&quot;YOURE GOING TO GET YOURSELVES KILlED!&quot;,</span>
    <span class="s font-papergirls">&quot;YOU PEOPLE ARE OUT OF YOUR&quot;,</span>
    <span class="s font-papergirls">&quot;NO&quot;,</span>
    <span class="s">…</span>
<span class="p">])</span></code></pre>
<p>And then?</p>
<h4 id="frequency-analysis">Frequency analysis</h4>
<p>Let’s do a wee bit of counting, shall we? First off, we’re gonna blindly assume that spaces mean spaces, apostrophe means apostrophe—I’ve only found one in the entire series so far—and comma means comma. That means we’re not really interested in any of those, so let’s replace them with the empty string. Then, we tally up the number of times each letter occurs, and list them, sorted by frequenceny in descending order.</p>
<div class="sourceCode" id="cb1"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb1-1"><a href="#cb1-1" aria-hidden="true" tabindex="-1"></a>letters <span class="op">=</span> re.sub(<span class="vs">r&quot;[\s&#39;,]&quot;</span>,<span class="st">&quot;&quot;</span>,data)</span>
<span id="cb1-2"><a href="#cb1-2" aria-hidden="true" tabindex="-1"></a>letters_counts <span class="op">=</span> collections.Counter(letters)</span>
<span id="cb1-3"><a href="#cb1-3" aria-hidden="true" tabindex="-1"></a>letters_counts <span class="op">=</span> <span class="bu">sorted</span>(letters_counts.items(), key<span class="op">=</span><span class="kw">lambda</span> x: x[<span class="dv">1</span>], reverse<span class="op">=</span><span class="va">True</span>)</span></code></pre></div>
<pre><code class="highlight"><span class="p">[(</span><span class="s font-papergirls">&#39;E&#39;</span><span class="p">,</span> <span class="mi">64</span><span class="p">),(</span><span class="s font-papergirls">&#39;T&#39;</span><span class="p">,</span> <span class="mi">50</span><span class="p">),(</span><span class="s font-papergirls">&#39;O&#39;</span><span class="p">,</span> <span class="mi">39</span><span class="p">),(</span><span class="s font-papergirls">&#39;A&#39;</span><span class="p">,</span> <span class="mi">35</span><span class="p">),(</span><span class="s font-papergirls">&#39;I&#39;</span><span class="p">,</span> <span class="mi">33</span><span class="p">),(</span><span class="s font-papergirls">&#39;.&#39;</span><span class="p">,</span> <span class="mi">30</span><span class="p">),(</span><span class="s font-papergirls">&#39;S&#39;</span><span class="p">,</span> <span class="mi">29</span><span class="p">),(</span><span class="s font-papergirls">&#39;L&#39;</span><span class="p">,</span> <span class="mi">29</span><span class="p">),(</span><span class="s font-papergirls">&#39;H&#39;</span><span class="p">,</span> <span class="mi">27</span><span class="p">),(</span><span class="s font-papergirls">&#39;N&#39;</span><span class="p">,</span> <span class="mi">23</span><span class="p">),(</span><span class="s font-papergirls">&#39;R&#39;</span><span class="p">,</span> <span class="mi">22</span><span class="p">),(</span><span class="s font-papergirls">&#39;Y&#39;</span><span class="p">,</span> <span class="mi">20</span><span class="p">),(</span><span class="s font-papergirls">&#39;D&#39;</span><span class="p">,</span> <span class="mi">19</span><span class="p">),(</span><span class="s font-papergirls">&#39;U&#39;</span><span class="p">,</span> <span class="mi">18</span><span class="p">),(</span><span class="s font-papergirls">&#39;G&#39;</span><span class="p">,</span> <span class="mi">14</span><span class="p">),(</span><span class="s font-papergirls">&#39;W&#39;</span><span class="p">,</span> <span class="mi">13</span><span class="p">),(</span><span class="s font-papergirls">&#39;C&#39;</span><span class="p">,</span> <span class="mi">11</span><span class="p">),(</span><span class="s font-papergirls">&#39;M&#39;</span><span class="p">,</span> <span class="mi">10</span><span class="p">),(</span><span class="s font-papergirls">&#39;B&#39;</span><span class="p">,</span> <span class="mi">10</span><span class="p">),(</span><span class="s font-papergirls">&#39;P&#39;</span><span class="p">,</span> <span class="mi">9</span><span class="p">),(</span><span class="s font-papergirls">&#39;V&#39;</span><span class="p">,</span> <span class="mi">8</span><span class="p">),(</span><span class="s font-papergirls">&#39;F&#39;</span><span class="p">,</span> <span class="mi">6</span><span class="p">),(</span><span class="s font-papergirls">&#39;!&#39;</span><span class="p">,</span> <span class="mi">6</span><span class="p">),(</span><span class="s font-papergirls">&#39;K&#39;</span><span class="p">,</span> <span class="mi">6</span><span class="p">),(</span><span class="s font-papergirls">&#39;o&#39;</span><span class="p">,</span> <span class="mi">4</span><span class="p">),(</span><span class="s font-papergirls">&#39;e&#39;</span><span class="p">,</span> <span class="mi">2</span><span class="p">),(</span><span class="s font-papergirls">&#39;J&#39;</span><span class="p">,</span> <span class="mi">2</span><span class="p">),(</span><span class="s font-papergirls">&#39;?&#39;</span><span class="p">,</span> <span class="mi">2</span><span class="p">),(</span><span class="s font-papergirls">&#39;X&#39;</span><span class="p">,</span> <span class="mi">2</span><span class="p">),(</span><span class="s font-papergirls">&#39;d&#39;</span><span class="p">,</span> <span class="mi">1</span><span class="p">),(</span><span class="s font-papergirls">&#39;s&#39;</span><span class="p">,</span> <span class="mi">1</span><span class="p">),(</span><span class="s font-papergirls">&#39;Q&#39;</span><span class="p">,</span> <span class="mi">1</span><span class="p">)]</span></code></pre>
<p>So if we wanna stick to our guess that this is really just English, but written with silly squiggles, then it’s looking pretty likely that ‘<span class="font-papergirls">E</span>’ is the letter ‘e’, since off the top of my head that’s the most commonly used letter in English. But what’s the rest? I always forget, though I know that the rest of the vowels and the ‘n’ are pretty high up there. We could Google, but, like, why would we? Let’s just grab some English text and count!</p>
<p>(Note: I strongly recommend not doing the next part by hand.)</p>
<p>Cool. Let’s pick some random English text, say <em><a href="https://en.wikipedia.org/wiki/Emma_(novel)">Emma</a></em> by Jane Austen—why not?—and do the exact same thing. We remove spaces, apostrophes, and commas, and while we’re at it, let’s convert the text to lowercase—our eldritch glyphs don’t really <em>look</em> like they have a case distinction.</p>
<div class="sourceCode" id="cb2"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb2-1"><a href="#cb2-1" aria-hidden="true" tabindex="-1"></a>letters_emma <span class="op">=</span> <span class="st">&quot; &quot;</span>.join(nltk.corpus.gutenberg.words(<span class="st">&#39;austen-emma.txt&#39;</span>))</span>
<span id="cb2-2"><a href="#cb2-2" aria-hidden="true" tabindex="-1"></a>letters_emma <span class="op">=</span> re.sub(<span class="st">&quot;[\s&#39;,]&quot;</span>,<span class="st">&quot;&quot;</span>,emma).lower()</span>
<span id="cb2-3"><a href="#cb2-3" aria-hidden="true" tabindex="-1"></a>letters_counts_emma <span class="op">=</span> collections.Counter(letters_emma)</span>
<span id="cb2-4"><a href="#cb2-4" aria-hidden="true" tabindex="-1"></a>letters_counts_emma <span class="op">=</span></span>
<span id="cb2-5"><a href="#cb2-5" aria-hidden="true" tabindex="-1"></a>    <span class="bu">sorted</span>(letters_counts_emma.items(), key<span class="op">=</span><span class="kw">lambda</span> x: x[<span class="dv">1</span>], reverse<span class="op">=</span><span class="va">True</span>)</span></code></pre></div>
<div class="sourceCode" id="cb3"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb3-1"><a href="#cb3-1" aria-hidden="true" tabindex="-1"></a>[(<span class="st">&#39;e&#39;</span>, <span class="dv">86021</span>), (<span class="st">&#39;t&#39;</span>, <span class="dv">59201</span>), (<span class="st">&#39;a&#39;</span>, <span class="dv">54379</span>), (<span class="st">&#39;o&#39;</span>, <span class="dv">53199</span>), (<span class="st">&#39;n&#39;</span>, <span class="dv">47288</span>), … ]</span></code></pre></div>
<p>Let’s plot those side by side, see if anything looks off about this.</p>
<p><img src="/assets/images/paper-girls-frequency-analysis.png" alt="Frequency analysis graphs for single characters in the speech in Paper Girls compared to the speech in Jane Austen’s Emma. The graphs look similar." /><br />
</p>
<p>Great news! The distribution of the letters looks like <em>language</em>. For comparison, if we encrypt <em>Emma</em> and then do frequency analysis, it looks something like this:</p>
<p><img src="/assets/images/paper-girls-frequency-analysis-encrypted.png" alt="Frequency analysis graphs for single characters in the speech in Jane Austen’s Emma encrypted compared to the speech in Jane Austen’s Emma unencrypted. The frequency counts for the encrypted speech are virtually uniform." /><br />
</p>
<p>Ok, so hypothesis:</p>
<blockquote>
<p>The frequencies for <em>Paper Girls</em> overlap <em>exactly</em> with those of <em>Emma</em>.</p>
</blockquote>
<p>Let’s test it. First, we build a translation table, and then we apply it to our data.</p>
<div class="sourceCode" id="cb4"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb4-1"><a href="#cb4-1" aria-hidden="true" tabindex="-1"></a>trans <span class="op">=</span> {</span>
<span id="cb4-2"><a href="#cb4-2" aria-hidden="true" tabindex="-1"></a>    l1: l2 <span class="cf">for</span> (l1, c1), (l2, c2) <span class="kw">in</span> <span class="bu">zip</span>(letters_counts, letters_counts_emma)}</span>
<span id="cb4-3"><a href="#cb4-3" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb4-4"><a href="#cb4-4" aria-hidden="true" tabindex="-1"></a>data_transed <span class="op">=</span> [</span>
<span id="cb4-5"><a href="#cb4-5" aria-hidden="true" tabindex="-1"></a>    <span class="st">&#39;&#39;</span>.join(trans.get(c,c) <span class="cf">for</span> c <span class="kw">in</span> line) <span class="cf">for</span> line <span class="kw">in</span> data.splitlines()]</span></code></pre></div>
<div class="sourceCode" id="cb5"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb5-1"><a href="#cb5-1" aria-hidden="true" tabindex="-1"></a>[</span>
<span id="cb5-2"><a href="#cb5-2" aria-hidden="true" tabindex="-1"></a>    <span class="st">&#39;yauqogdnti&#39;</span>,</span>
<span id="cb5-3"><a href="#cb5-3" aria-hidden="true" tabindex="-1"></a>    <span class="st">&quot;hpsnt wpi ce&#39;ss ge;t ot tre hefadu vasundyi&quot;</span>,</span>
<span id="cb5-4"><a href="#cb5-4" aria-hidden="true" tabindex="-1"></a>    <span class="st">&#39;htom ocom vlag ge-&#39;</span>,</span>
<span id="cb5-5"><a href="#cb5-5" aria-hidden="true" tabindex="-1"></a>    <span class="st">&#39;htap-&#39;</span>,</span>
<span id="cb5-6"><a href="#cb5-6" aria-hidden="true" tabindex="-1"></a>    <span class="st">&#39;pseohe-&#39;</span>,</span>
<span id="cb5-7"><a href="#cb5-7" aria-hidden="true" tabindex="-1"></a>    <span class="st">&#39;mawle yandy ta yet mawlhes.eh knsseu-&#39;</span>,</span>
<span id="cb5-8"><a href="#cb5-8" aria-hidden="true" tabindex="-1"></a>    <span class="st">&#39;maw peapse ole awt av mawl&#39;</span>,</span>
<span id="cb5-9"><a href="#cb5-9" aria-hidden="true" tabindex="-1"></a>    <span class="st">&#39;da&#39;</span>,</span>
<span id="cb5-10"><a href="#cb5-10" aria-hidden="true" tabindex="-1"></a>    …</span>
<span id="cb5-11"><a href="#cb5-11" aria-hidden="true" tabindex="-1"></a>]</span></code></pre></div>
<p>Ok, so, maybe we’re not so lucky? Eugh, why do these things always have to be hard.</p>
<p>Well, our data is pretty sparse, so it makes sense that these wouldn’t line up precisely, even if our teenagers <em>were</em> speaking English… And I’m not particularily keen on going in and transcribing more, so I guess we’ll just have to do more thinking.</p>
<p>Let’s just assume that we got one or two characters right. Those first peaks look pretty convincing, so let’s assume ‘<span class="font-papergirls">E</span>’ is ‘e’ and ‘<span class="font-papergirls">T</span>’ is ‘t’. What else can we do?</p>
<h4 id="common-words">Common words</h4>
<p>We can have a look at the single-letter words! Let’s pretend we don’t know English. What are common single-letter words, according to <em>Emma</em>?</p>
<div class="sourceCode" id="cb6"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb6-1"><a href="#cb6-1" aria-hidden="true" tabindex="-1"></a>single_letter_words_counts <span class="op">=</span> collections.Counter(</span>
<span id="cb6-2"><a href="#cb6-2" aria-hidden="true" tabindex="-1"></a>    word <span class="cf">for</span> word <span class="kw">in</span> emma.split() <span class="cf">if</span> <span class="bu">len</span>(word) <span class="op">==</span> <span class="dv">1</span> <span class="kw">and</span> word.isalpha())</span>
<span id="cb6-3"><a href="#cb6-3" aria-hidden="true" tabindex="-1"></a>single_letter_words_counts <span class="op">=</span> <span class="bu">sorted</span>(</span>
<span id="cb6-4"><a href="#cb6-4" aria-hidden="true" tabindex="-1"></a>    single_letter_words_counts.items(), key<span class="op">=</span><span class="kw">lambda</span> x: x[<span class="dv">1</span>], reverse<span class="op">=</span><span class="va">True</span>)</span></code></pre></div>
<div class="sourceCode" id="cb7"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb7-1"><a href="#cb7-1" aria-hidden="true" tabindex="-1"></a>[(<span class="st">&#39;I&#39;</span>, <span class="dv">3178</span>), (<span class="st">&#39;a&#39;</span>, <span class="dv">3004</span>), (<span class="st">&#39;s&#39;</span>, <span class="dv">933</span>), (<span class="st">&#39;A&#39;</span>, <span class="dv">125</span>), (<span class="st">&#39;t&#39;</span>, <span class="dv">19</span>), (<span class="st">&#39;d&#39;</span>, <span class="dv">12</span>),</span>
<span id="cb7-2"><a href="#cb7-2" aria-hidden="true" tabindex="-1"></a> (<span class="st">&#39;E&#39;</span>, <span class="dv">11</span>),   (<span class="st">&#39;o&#39;</span>, <span class="dv">8</span>),    (<span class="st">&#39;F&#39;</span>, <span class="dv">4</span>),   (<span class="st">&#39;W&#39;</span>, <span class="dv">4</span>),   (<span class="st">&#39;V&#39;</span>, <span class="dv">3</span>),  … ]</span></code></pre></div>
<p>Ah, yes, the well-known word ‘W’. But at least we can be fairly sure that the only single-letter words which matter in English are ‘I’ and ‘a’, and we didn’t even have to think! Let’s see what single-letter words our data has on offer…</p>
<div class="sourceCode" id="cb8"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb8-1"><a href="#cb8-1" aria-hidden="true" tabindex="-1"></a>single_letter_words <span class="op">=</span> <span class="bu">set</span>(word <span class="cf">for</span> word <span class="kw">in</span> data.split() <span class="cf">if</span> <span class="bu">len</span>(word) <span class="op">==</span> <span class="dv">1</span>)</span></code></pre></div>
<pre><code class="highlight"><span class="p">[</span><span class="s font-papergirls">&#39;I&#39;</span><span class="p">]</span></code></pre>
<p>Well, that’s disappointing, now we have to pick. At least we’re fairly sure now that ‘<span class="s font-papergirls">I</span>’ will turn out to be either ‘i’ or ‘a’.</p>
<p>We can repeat this trick for two letter words, three letter words, four letter words, etc. However, we’ll probably need quite a lot of data for word counts to start looking like your average word counts for English—like those found in <em>Emma</em> and only <em>Emma</em>. Let’s count up the two-letter words, and plot their frequencies:</p>
<p><img src="/assets/images/paper-girls-frequency-analysis-two-letter.png" alt="Frequency analysis graphs for two-letter words in the speech in Paper Girls compared to the speech in Jane Austen’s Emma. The graphs look similar." /><br />
</p>
<p>It’s tempting to assume that those plots align exactly, but we’re dealing with <em>really</em> sparse data at this point. Instead, let’s use the knowledge that we’ve uncovered so far to see if we can find any of those common words. The words ‘<span class="font-papergirls">TO</span>’, ‘<span class="font-papergirls">IT</span>’, and ‘<span class="font-papergirls">AT</span>’ all feature a ‘<span class="font-papergirls">T</span>’. They seem like pretty likely candidates for ‘to’, ‘it’, and ‘at’, so it seems quite likely that ‘<span class="font-papergirls">O</span>’ is ‘o’, and that ‘<span class="font-papergirls">I</span>’ is ‘i’ and ‘<span class="font-papergirls">A</span>’ is ‘a’, or vice versa. Note that ‘<span class="font-papergirls">A</span>’ is the fourth most character, like we’d expect from an ‘i’ or an ‘a’. Based on the frequencies—‘a’ is a more common character than ‘i’, but ‘I’ is a more common word than ‘a’—we can guess that ‘<span class="font-papergirls">I</span>’ is ‘i’ and ‘<span class="font-papergirls">a</span>’ is ‘a’.</p>
<p>We could continue with this trick for quite a while, using three-letter words, four-letter words, bi-grams, tri-grams, etc. However, we could quite quickly run into the problem that the data we have is really quite sparse, and we can only really solve that by reading more <em>Paper Girls</em> and adding more phrases to our database… which… well, the first part of that sounds like fun. However, there is one trick which we can still quite easily exploit…</p>
<h4 id="double-letters">Double letters</h4>
<p>Let’s do some counting of <em>repeated letters</em>, such as the ‘tt’ in letters, and see how those patterns compare to those found in English. We do this by zipping the data with the data offset by one–e.g. from <code>"hello"</code> we would get <code>[('h','e'), ('e','l'), ('l','l'), ('l','o')]</code>—and then picking only the pairs where both letters are the same—in our example, <code>('l','l')</code>.</p>
<div class="sourceCode" id="cb9"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb9-1"><a href="#cb9-1" aria-hidden="true" tabindex="-1"></a>doubles <span class="op">=</span> [ l1 <span class="op">+</span> l2 <span class="cf">for</span> l1, l2 <span class="kw">in</span> <span class="bu">zip</span>(data,data[<span class="dv">1</span>:]) <span class="cf">if</span> l1 <span class="op">==</span> l2 ]</span>
<span id="cb9-2"><a href="#cb9-2" aria-hidden="true" tabindex="-1"></a>doubles_counts <span class="op">=</span> collections.Counter(doubles)</span>
<span id="cb9-3"><a href="#cb9-3" aria-hidden="true" tabindex="-1"></a>doubles_counts <span class="op">=</span> <span class="bu">sorted</span>(doubles_counts.items(), key<span class="op">=</span><span class="kw">lambda</span> x: x[<span class="dv">1</span>], reverse<span class="op">=</span><span class="va">True</span>)</span></code></pre></div>
<p>You’ve got the hang of this by now—we count doubles in <em>Emma</em> as well, and plot both distributions.</p>
<p><img src="/assets/images/paper-girls-frequency-analysis-doubles-1.png" alt="Frequency analysis graphs for bigrams in the speech in Paper Girls compared to the speech in Jane Austen’s Emma. The graph for paper girls has only two entries." /><br />
</p>
<p>Whoa, so something kinda weird is going on here. First off, really? There’s only <em>two</em> characters that are ever repeated? Like, our sample size is small, but not <em>that</em> small. Also, neither of these characters are anything we have a guess for yet… so if our guesses are correct, we don’t have <em>any</em> occurances of ‘ee’ or ‘tt’. Look at the chart for <em>Emma</em>, they’re pretty common.</p>
<p>It may be a good idea to have a closer look at the data. If you scroll up to where we defined <code>data</code>, you’ll find ‘<span class="font-papergirls">Dd</span>’ and ‘<span class="font-papergirls">Ee</span>’, and actually, if we look at all the data, we’ll find ‘<span class="font-papergirls">Oo</span>’ and ‘<span class="font-papergirls">Ss</span>’ as well. These mirrored versions of the characters only seem to appear directly after a neutral versions of the same character, and they look <em>suspiciously</em> like repeated characters. Actually, we should’ve asked this <em>a long time ago</em>. How many characters do we even have?</p>
<div class="sourceCode" id="cb10"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb10-1"><a href="#cb10-1" aria-hidden="true" tabindex="-1"></a>num_characters <span class="op">=</span> <span class="bu">len</span>(letters_counts)</span></code></pre></div>
<div class="sourceCode" id="cb11"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb11-1"><a href="#cb11-1" aria-hidden="true" tabindex="-1"></a><span class="dv">32</span></span></code></pre></div>
<p><em>WHAT?!</em> 32 CHARACTERS?! Right, that doesn’t mash with our whole “Latin alphabet” thing. Eugh, let’s finish this “double characters” thing first, and then we’ll look into our surplus of characters.</p>
<p>Let’s count up the double characters <em>and</em> the characters followed by their mirrored image, and plot those against the double characters in <em>Emma</em>.</p>
<div class="sourceCode" id="cb12"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb12-1"><a href="#cb12-1" aria-hidden="true" tabindex="-1"></a>doubles_or_mirrored <span class="op">=</span> [</span>
<span id="cb12-2"><a href="#cb12-2" aria-hidden="true" tabindex="-1"></a>    l1 <span class="op">+</span> l2 <span class="cf">for</span> l1, l2 <span class="kw">in</span> <span class="bu">zip</span>(data,data[<span class="dv">1</span>:]) <span class="cf">if</span> l1 <span class="op">==</span> l2 <span class="kw">or</span> l1 <span class="op">==</span> l2.mirrored() ]</span>
<span id="cb12-3"><a href="#cb12-3" aria-hidden="true" tabindex="-1"></a>doubles_or_mirrored_counts <span class="op">=</span> collections.Counter(doubles_or_mirrored)</span>
<span id="cb12-4"><a href="#cb12-4" aria-hidden="true" tabindex="-1"></a>doubles_or_mirrored_counts <span class="op">=</span> <span class="bu">sorted</span>(</span>
<span id="cb12-5"><a href="#cb12-5" aria-hidden="true" tabindex="-1"></a>    doubles_or_mirrored_counts.items(), key<span class="op">=</span><span class="kw">lambda</span> x: x[<span class="dv">1</span>], reverse<span class="op">=</span><span class="va">True</span>)</span></code></pre></div>
<p><img src="/assets/images/paper-girls-frequency-analysis-doubles-2.png" alt="Frequency analysis graphs for bigrams in the speech in Paper Girls compared to the speech in Jane Austen’s Emma. The graphs look similar." /><br />
</p>
<p>Ah, that makes <em>way</em> more sense. We can still probably assume that ‘<span class="font-papergirls">LL</span>’ is ‘ll’, and it probably makes sense to stick to our guns and say that ‘<span class="font-papergirls">Oo</span>’ is ‘oo’ and ‘<span class="font-papergirls">Ee</span>’ is ‘ee’.</p>
<h4 id="too-many-characters">Too many characters</h4>
<p>Eek! So let’s get to that whole “too many characters thing”. See, if our hypothesis is true, and this is really English, then we’d kinda expect to see at most 26 characters. Realistically, we’d expect more like 20 to 23 characters. For instance, if we check the frequency analysis for letters a while back, we see that ‘v’, ‘k’, ‘x’, ‘j’, ‘q’, and ‘z’ are rarer in English than the full stop.</p>
<p>So what do we do with our bonus characters? Well, if we go with the idea that doubles are mirrored, then that at least takes care of four characters: ‘<span class="font-papergirls">o</span>’, ‘<span class="font-papergirls">e</span>’, ‘<span class="font-papergirls">d</span>’, and ‘<span class="font-papergirls">s</span>’. That takes us down to only 28 characters.</p>
<p>So… four down, at least two to go? The most common non-letter is the full stop, but it’d be a bit weird if the full stop were included in the alphabet, given that the comma and the apostrophe were just written as usual. However, if we look at the data, we find three characters—<span class="font-papergirls">.</span>, <span class="font-papergirls">!</span>, and <span class="font-papergirls">?</span>—which <em>exclusively</em> appear at the end of words, and <em>almost exclusively</em> at the end of the entire speech bubble. Moreover, almost every speech bubble ends with one of these characters, the only exceptions being chapter titles and <em>two</em> other utterances. We can assume that these are ‘.’, ‘!’, and ‘?’, in order of frequency. Honestly, it doesn’t hurt too much if we get these wrong, the only important thing is that—if these truly are punctuation—we don’t try to assign letters to them.</p>
<h4 id="write-down-everything-weve-got">Write down everything we’ve got!</h4>
<p>Right, so, we’ve made quite a few guesses by now, let’s see if that gets us anywhere. First, let’s write down everything we’ve got…</p>
<pre><code class="highlight"><span class="n">trans</span> <span class="o">=</span> <span class="p">{</span>
    <span class="s">&quot; &quot;</span><span class="p">:</span> <span class="s">&quot; &quot;</span><span class="p">,</span> <span class="s">&quot;&#39;&quot;</span><span class="p">:</span> <span class="s">&quot;&#39;&quot;</span><span class="p">,</span> <span class="s">&quot;,&quot;</span><span class="p">:</span> <span class="s">&quot;,&quot;</span><span class="p">,</span>
    <span class="s font-papergirls">&#39;A&#39;</span><span class="p">:</span> <span class="s">&#39;a&#39;</span><span class="p">,</span> <span class="s font-papergirls">&#39;E&#39;</span><span class="p">:</span> <span class="s">&#39;e&#39;</span><span class="p">,</span> <span class="s font-papergirls">&#39;e&#39;</span><span class="p">:</span> <span class="s">&#39;e&#39;</span><span class="p">,</span> <span class="s font-papergirls">&#39;T&#39;</span><span class="p">:</span> <span class="s">&#39;t&#39;</span><span class="p">,</span> <span class="s font-papergirls">&#39;I&#39;</span><span class="p">:</span> <span class="s">&#39;i&#39;</span><span class="p">,</span> <span class="s font-papergirls">&#39;o&#39;</span><span class="p">:</span> <span class="s">&#39;o&#39;</span><span class="p">,</span> <span class="s font-papergirls">&#39;O&#39;</span><span class="p">:</span> <span class="s">&#39;o&#39;</span><span class="p">,</span> <span class="s font-papergirls">&#39;L&#39;</span><span class="p">:</span> <span class="s">&#39;l&#39;</span><span class="p">,</span> <span class="s font-papergirls">&#39;.&#39;</span><span class="p">:</span> <span class="s">&#39;.&#39;</span><span class="p">,</span> <span class="s font-papergirls">&#39;!&#39;</span><span class="p">:</span> <span class="s">&#39;!&#39;</span><span class="p">,</span> <span class="s font-papergirls">&#39;?&#39;</span><span class="p">:</span> <span class="s">&#39;?&#39;</span><span class="p">}</span></code></pre>
<p>…and then use that to try and translate our data, replacing anything for which we haven’t made a guess yet with an underscore.</p>
<div class="sourceCode" id="cb13"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb13-1"><a href="#cb13-1" aria-hidden="true" tabindex="-1"></a>data_transed <span class="op">=</span> [</span>
<span id="cb13-2"><a href="#cb13-2" aria-hidden="true" tabindex="-1"></a>    <span class="st">&#39;&#39;</span>.join(trans.get(c,<span class="st">&#39;_&#39;</span>) <span class="cf">for</span> c <span class="kw">in</span> line) <span class="cf">for</span> line <span class="kw">in</span> data.splitlines()]</span></code></pre></div>
<div class="sourceCode" id="cb14"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb14-1"><a href="#cb14-1" aria-hidden="true" tabindex="-1"></a>[<span class="st">&#39;_o__a__it.&#39;</span>,</span>
<span id="cb14-2"><a href="#cb14-2" aria-hidden="true" tabindex="-1"></a> <span class="st">&quot;__lit __. _e&#39;ll _eet at t_e _e_o__ _ol_i__.&quot;</span>,</span>
<span id="cb14-3"><a href="#cb14-3" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_ta_ a_a_ __o_ _e!&#39;</span>,</span>
<span id="cb14-4"><a href="#cb14-4" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_to_!&#39;</span>,</span>
<span id="cb14-5"><a href="#cb14-5" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_lea_e!&#39;</span>,</span>
<span id="cb14-6"><a href="#cb14-6" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_o__e _oi__ to _et _o___el_e_ _ille_!&#39;</span>,</span>
<span id="cb14-7"><a href="#cb14-7" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_o_ _eo_le a_e o_t o_ _o__&#39;</span>,</span>
<span id="cb14-8"><a href="#cb14-8" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_o&#39;</span>,</span>
<span id="cb14-9"><a href="#cb14-9" aria-hidden="true" tabindex="-1"></a> …]</span></code></pre></div>
<p>From here, it’s a bit of a word puzzle. For instance, “_e’ll _eet at t_e” looks like it’s saying “we’ll meet at the”. Let’s add those guesses, and translate again, see if they make sense?</p>
<div class="sourceCode" id="cb15"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb15-1"><a href="#cb15-1" aria-hidden="true" tabindex="-1"></a>[<span class="st">&#39;_o__am_it.&#39;</span>,</span>
<span id="cb15-2"><a href="#cb15-2" aria-hidden="true" tabindex="-1"></a> <span class="st">&quot;__lit __. we&#39;ll meet at the _e_o__ _ol_i__.&quot;</span>,</span>
<span id="cb15-3"><a href="#cb15-3" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_ta_ awa_ __om me!&#39;</span>,</span>
<span id="cb15-4"><a href="#cb15-4" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_to_!&#39;</span>,</span>
<span id="cb15-5"><a href="#cb15-5" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_lea_e!&#39;</span>,</span>
<span id="cb15-6"><a href="#cb15-6" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_o__e _oi__ to _et _o___el_e_ _ille_!&#39;</span>,</span>
<span id="cb15-7"><a href="#cb15-7" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_o_ _eo_le a_e o_t o_ _o__&#39;</span>,</span>
<span id="cb15-8"><a href="#cb15-8" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_o&#39;</span>,</span>
<span id="cb15-9"><a href="#cb15-9" aria-hidden="true" tabindex="-1"></a> …]</span></code></pre></div>
<p>Does “awa_ __om me” say “away from me”? Let’s see if that makes sense!</p>
<div class="sourceCode" id="cb16"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb16-1"><a href="#cb16-1" aria-hidden="true" tabindex="-1"></a>[<span class="st">&#39;_o__am_it.&#39;</span>,</span>
<span id="cb16-2"><a href="#cb16-2" aria-hidden="true" tabindex="-1"></a> <span class="st">&quot;__lit __. we&#39;ll meet at the _e_o__ fol_i__.&quot;</span>,</span>
<span id="cb16-3"><a href="#cb16-3" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_tay away from me!&#39;</span>,</span>
<span id="cb16-4"><a href="#cb16-4" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_to_!&#39;</span>,</span>
<span id="cb16-5"><a href="#cb16-5" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_lea_e!&#39;</span>,</span>
<span id="cb16-6"><a href="#cb16-6" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;yo_re _oi__ to _et yo_r_el_e_ _ille_!&#39;</span>,</span>
<span id="cb16-7"><a href="#cb16-7" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;yo_ _eo_le are o_t of yo_r&#39;</span>,</span>
<span id="cb16-8"><a href="#cb16-8" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_o&#39;</span>,</span>
<span id="cb16-9"><a href="#cb16-9" aria-hidden="true" tabindex="-1"></a> …]</span></code></pre></div>
<p>Right, <em>stay</em>, as in “stay away from me!” If we’d kept the source text in mind, we would’ve probably seen that. Though I often find it easier to stare only at the underscored text, to not be distracted by these otherworldly glyphs.</p>
<div class="sourceCode" id="cb17"><pre class="sourceCode python"><code class="sourceCode python"><span id="cb17-1"><a href="#cb17-1" aria-hidden="true" tabindex="-1"></a>[<span class="st">&#39;_o__am_it.&#39;</span>,</span>
<span id="cb17-2"><a href="#cb17-2" aria-hidden="true" tabindex="-1"></a> <span class="st">&quot;s_lit __. we&#39;ll meet at the se_o__ fol_i__.&quot;</span>,</span>
<span id="cb17-3"><a href="#cb17-3" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;stay away from me!&#39;</span>,</span>
<span id="cb17-4"><a href="#cb17-4" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;sto_!&#39;</span>,</span>
<span id="cb17-5"><a href="#cb17-5" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_lease!&#39;</span>,</span>
<span id="cb17-6"><a href="#cb17-6" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;yo_re _oi__ to _et yo_rsel_es _ille_!&#39;</span>,</span>
<span id="cb17-7"><a href="#cb17-7" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;yo_ _eo_le are o_t of yo_r&#39;</span>,</span>
<span id="cb17-8"><a href="#cb17-8" aria-hidden="true" tabindex="-1"></a> <span class="st">&#39;_o&#39;</span>,</span>
<span id="cb17-9"><a href="#cb17-9" aria-hidden="true" tabindex="-1"></a> …]</span></code></pre></div>
<p>Things are starting to get pretty clear. “Stop!”, “Please!”, “youre going to get yourselves killed!”, “you people are out of your”… We can keep doing this, and eventually we’ll manage to find guesses for most of the letters. The authors have even included an ‘x’ and a ‘q’ in such natural words such as ‘vertex’ and ‘tuplequad’. Honestly, that last one took me a while. The ‘ua’ and the fact that it was either gonna be a ‘q’ or a ‘z’ helped.</p>
<h4 id="and-then-you-make-a-nice-table">…and then you make a nice table</h4>
<p>Woo! We’ve done it! We can read the bizzare glyphs in <em>Paper Girls</em> now! And the only letter we’re missing is the ‘z’. Hope I’ll find one of those in one of the later books.</p>
<table style="font-size:xx-large;">
<tbody>
<tr>
<td>
<span class="font-papergirls">A</span>
</td>
<td>
<span class="font-papergirls">B</span>
</td>
<td>
<span class="font-papergirls">C</span>
</td>
<td>
<span class="font-papergirls">D</span>
</td>
<td>
<span class="font-papergirls">E</span>
</td>
<td>
<span class="font-papergirls">F</span>
</td>
<td>
<span class="font-papergirls">G</span>
</td>
<td>
<span class="font-papergirls">H</span>
</td>
<td>
<span class="font-papergirls">I</span>
</td>
<td>
<span class="font-papergirls">J</span>
</td>
</tr>
<tr>
<td>
a
</td>
<td>
b
</td>
<td>
c
</td>
<td>
d
</td>
<td>
e
</td>
<td>
f
</td>
<td>
g
</td>
<td>
h
</td>
<td>
i
</td>
<td>
j
</td>
</tr>
<tr>
<td>
<span class="font-papergirls">K</span>
</td>
<td>
<span class="font-papergirls">L</span>
</td>
<td>
<span class="font-papergirls">M</span>
</td>
<td>
<span class="font-papergirls">N</span>
</td>
<td>
<span class="font-papergirls">O</span>
</td>
<td>
<span class="font-papergirls">P</span>
</td>
<td>
<span class="font-papergirls">Q</span>
</td>
<td>
<span class="font-papergirls">R</span>
</td>
<td>
<span class="font-papergirls">S</span>
</td>
<td>
<span class="font-papergirls">T</span>
</td>
</tr>
<tr>
<td>
k
</td>
<td>
l
</td>
<td>
m
</td>
<td>
n
</td>
<td>
o
</td>
<td>
p
</td>
<td>
q
</td>
<td>
r
</td>
<td>
s
</td>
<td>
t
</td>
</tr>
<tr>
<td>
<span class="font-papergirls">U</span>
</td>
<td>
<span class="font-papergirls">V</span>
</td>
<td>
<span class="font-papergirls">W</span>
</td>
<td>
<span class="font-papergirls">X</span>
</td>
<td>
<span class="font-papergirls">Y</span>
</td>
<td>
<span class="font-papergirls">_</span>
</td>
<td>
<span class="font-papergirls"> </span>
</td>
<td>
<span class="font-papergirls">.</span>
</td>
<td>
<span class="font-papergirls">!</span>
</td>
<td>
<span class="font-papergirls">?</span>
</td>
</tr>
<tr>
<td>
u
</td>
<td>
v
</td>
<td>
w
</td>
<td>
x
</td>
<td>
y
</td>
<td>
z
</td>
<td>
</td>
<td>
.
</td>
<td>
!
</td>
<td>
?
</td>
</tr>
</tbody>
</table>
<p>Wrapping up, what have we learned? I guess the important things are that you can pretty easily crack any substitution cipher by boldly making some assumptions about what the language is, and then exploiting letter frequencies from that language… and that you can exploit way more than just <em>plain</em> letter freqencies—short words, n-grams, repeated characters, they’re all good candidates for frequency analysis.</p>
<p>Also… <em>Paper Girls</em> is fucking rad, go read that shit.</p>