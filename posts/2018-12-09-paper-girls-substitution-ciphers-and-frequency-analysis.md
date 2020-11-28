---
title      : "Paper Girls, Substitution Ciphers, and Frequency Analysis"
date       : 2018-12-09 12:00:00
tags       : [linguistics, cryptography, python]
---

<span class="font-papergirls">
WARNING!
IF YOU'RE READING THIS, YOU'RE DOING IT WRONG.
CLICK THE LINK, READ IT ON MY BLOG.
IT'LL BE WAY LESs CONFUSING.
ANYWAY.
THIS WEeKEND, I PICKED UP A COPY OF PAPER GIRLS, A MYSTERY/SCIENCE FICTION COMIC ABOUT FOUR GIRLS ON A PAPER ROUTE.
WAIT, YOU CAN'T UNDERSTAND ME?
OH, FUCK, DOESN'T LOoK LIKE IT.
SORrY, LET ME ADJUST.
</span>

<!--more-->

Ah, I'm so sorry, is this any better? Once again, so sorry, I think I've been reading this stuff for too long, think I got carried away… Anyway, I'll take it from the start. So this weekend, I picked up a copy of *[Paper Girls][papergirls]*, a mystery/science fiction comic about four girls on a paper route…

![Image from Paper Girls. MacKenzie, smoking, sits on her bicycle, while Erin, KJ, and Tiffany stand behind her.](/public/images/paper-girls-1.jpg)\

It starts simple. Our four heroes band together to deliver papers on Halloween morning. But it quickly escalates. Testament to this fact, thirty-five pages in we meet three time traveling teenagers, wrapped in black bandages, speaking in *these things*…

![Image from Paper Girls. Two figures wrapped in dark bandages bend over their fallen friend. Their speech is written using strange glyphs.](/public/images/paper-girls-2.png)\

These glyphs are used for the rest of the book, and---best I can tell---the rest of the series without any real explanation… which leaves me with the following question: 

> What in fuck's name are they saying?

Wanna guess what today's topic is? Yep. Welcome to my class, "Help! These letters are all funky! (Part 1, Substitution Ciphers)".

Now, I've done this before. Several years ago, I was given a copy of *[Johnny 23][johnny23]* by Charles Burns which is, euh, a book put together using panels from *[X'ed Out][xedout]*, with an entirely new "story" written in strange glyphs… Sound, well, semi-familiar at least?

![Image from X'ed Out. A person is shown a crying worm by a hooded figure with a wound for a nose. The hooded figure then eats the worm. Their speech is written using strange glyphs.](/public/images/xed-out.jpg)\

It turns out that what Charles had done was basically a substitution cipher---you make up one crazy glyph for each letter of the alphabet, and then instead of writing letters from the alphabet, you use the alien glyphs you came up with…

Honestly, there's some pretty good reasons to assume that *Paper Girls* does the same. Substitution ciphers are really easy to come up with---you basically have to come up with one weird glyph for ever letter in your alphabet. Plus, they get the job done! At least, it's pretty clear to me that our time traveling teenagers are speaking some arcane language!

Substitution ciphers are pretty easy to solve, but compare that more complex ciphers. We could have the cipher evolve over time, say, shift those sinister glyphs by one every time we turn a page. But… do the characters know what page they're on? And could you speak a language where the sounds you make to say, say *chair*, change every few minutes? 

What about actual cryptographic ciphers? We might as well be putting random noise on the page! There's no way any reader would crack anything like that, and no way any human could actually speak, say, [RSA-encrypted][rsa] English.

Ideally, you would construct an entirely new language.
Constructed languages have a long and rich history, stretching from the 17th century search for the [Perfect Language][solresol] to the modern desire to make our [Bad Space Russians][klingon] and [Magic Romans][valyrian] more compelling.
However, constructing a languages takes a *serious* amount of time and effort, and heaps of linguistics knowledge, so you'll probably only find this kind of stuff in *huge* and well-funded projects---or in the works of a guy who really just writes fantasy to justify [his conlanging hobby][quenya].

All in all, ciphers aren't the best way to emulate a new and unfamiliar language, but they are the cheapest…
*"But how do I crack this stuff?!"*, you cry out in frustration. Right, good point. Let's get back to today's lecture!


# How do you crack this stuff?

First off, I'll be writing some Python code in this section.
Really, I'm just doing that 'cuz I'm bad at counting.
You can do everything I'll be doing by hand, with just some pen and paper.

The first thing you need to do is get some data. 
Grab your favourite comic with a cipher in there, and start writing down words. 
I'll be using the first five issues of *Paper Girls* for this, because that's how far I've read.

<pre><code class="highlight"><span class="n">data</span> <span class="o">=</span> <span class="s">"</span><span class="se">\n</span><span class="s">"</span><span class="o">.</span><span class="n">join</span><span class="p">([</span>
    <span class="s font-papergirls">"GODdAMNIT.",</span>
    <span class="s font-papergirls">"SPLIT UP. WE'Ll MEeT AT THE SECOND FOLDING.",</span>
    <span class="s font-papergirls">"STAY AWAY FROM ME!",</span>
    <span class="s font-papergirls">"STOP!",</span>
    <span class="s font-papergirls">"PLEASE!",</span>
    <span class="s font-papergirls">"YOURE GOING TO GET YOURSELVES KILlED!",</span>
    <span class="s font-papergirls">"YOU PEOPLE ARE OUT OF YOUR",</span>
    <span class="s font-papergirls">"NO",</span>
    <span class="s">…</span>
<span class="p">])</span></code></pre>

And then?


### Frequency analysis

Let's do a wee bit of counting, shall we?
First off, we're gonna blindly assume that spaces mean spaces, apostrophe means apostrophe---I've only found one in the entire series so far---and comma means comma.
That means we're not really interested in any of those, so let's replace them with the empty string.
Then, we tally up the number of times each letter occurs, and list them, sorted by frequenceny in descending order.

```python
letters = re.sub(r"[\s',]","",data)
letters_counts = collections.Counter(letters)
letters_counts = sorted(letters_counts.items(), key=lambda x: x[1], reverse=True)
```

<pre><code class="highlight"><span class="p">[(</span><span class="s font-papergirls">'E'</span><span class="p">,</span> <span class="mi">64</span><span class="p">),(</span><span class="s font-papergirls">'T'</span><span class="p">,</span> <span class="mi">50</span><span class="p">),(</span><span class="s font-papergirls">'O'</span><span class="p">,</span> <span class="mi">39</span><span class="p">),(</span><span class="s font-papergirls">'A'</span><span class="p">,</span> <span class="mi">35</span><span class="p">),(</span><span class="s font-papergirls">'I'</span><span class="p">,</span> <span class="mi">33</span><span class="p">),(</span><span class="s font-papergirls">'.'</span><span class="p">,</span> <span class="mi">30</span><span class="p">),(</span><span class="s font-papergirls">'S'</span><span class="p">,</span> <span class="mi">29</span><span class="p">),(</span><span class="s font-papergirls">'L'</span><span class="p">,</span> <span class="mi">29</span><span class="p">),(</span><span class="s font-papergirls">'H'</span><span class="p">,</span> <span class="mi">27</span><span class="p">),(</span><span class="s font-papergirls">'N'</span><span class="p">,</span> <span class="mi">23</span><span class="p">),(</span><span class="s font-papergirls">'R'</span><span class="p">,</span> <span class="mi">22</span><span class="p">),(</span><span class="s font-papergirls">'Y'</span><span class="p">,</span> <span class="mi">20</span><span class="p">),(</span><span class="s font-papergirls">'D'</span><span class="p">,</span> <span class="mi">19</span><span class="p">),(</span><span class="s font-papergirls">'U'</span><span class="p">,</span> <span class="mi">18</span><span class="p">),(</span><span class="s font-papergirls">'G'</span><span class="p">,</span> <span class="mi">14</span><span class="p">),(</span><span class="s font-papergirls">'W'</span><span class="p">,</span> <span class="mi">13</span><span class="p">),(</span><span class="s font-papergirls">'C'</span><span class="p">,</span> <span class="mi">11</span><span class="p">),(</span><span class="s font-papergirls">'M'</span><span class="p">,</span> <span class="mi">10</span><span class="p">),(</span><span class="s font-papergirls">'B'</span><span class="p">,</span> <span class="mi">10</span><span class="p">),(</span><span class="s font-papergirls">'P'</span><span class="p">,</span> <span class="mi">9</span><span class="p">),(</span><span class="s font-papergirls">'V'</span><span class="p">,</span> <span class="mi">8</span><span class="p">),(</span><span class="s font-papergirls">'F'</span><span class="p">,</span> <span class="mi">6</span><span class="p">),(</span><span class="s font-papergirls">'!'</span><span class="p">,</span> <span class="mi">6</span><span class="p">),(</span><span class="s font-papergirls">'K'</span><span class="p">,</span> <span class="mi">6</span><span class="p">),(</span><span class="s font-papergirls">'o'</span><span class="p">,</span> <span class="mi">4</span><span class="p">),(</span><span class="s font-papergirls">'e'</span><span class="p">,</span> <span class="mi">2</span><span class="p">),(</span><span class="s font-papergirls">'J'</span><span class="p">,</span> <span class="mi">2</span><span class="p">),(</span><span class="s font-papergirls">'?'</span><span class="p">,</span> <span class="mi">2</span><span class="p">),(</span><span class="s font-papergirls">'X'</span><span class="p">,</span> <span class="mi">2</span><span class="p">),(</span><span class="s font-papergirls">'d'</span><span class="p">,</span> <span class="mi">1</span><span class="p">),(</span><span class="s font-papergirls">'s'</span><span class="p">,</span> <span class="mi">1</span><span class="p">),(</span><span class="s font-papergirls">'Q'</span><span class="p">,</span> <span class="mi">1</span><span class="p">)]</span></code></pre>

So if we wanna stick to our guess that this is really just English, but written with silly squiggles, then it's looking pretty likely that '<span class="font-papergirls">E</span>' is the letter 'e', since off the top of my head that's the most commonly used letter in English. But what's the rest? I always forget, though I know that the rest of the vowels and the 'n' are pretty high up there. We could Google, but, like, why would we? Let's just grab some English text and count!

(Note: I strongly recommend not doing the next part by hand.)

Cool. Let's pick some random English text, say *[Emma][emma]* by Jane Austen---why not?---and do the exact same thing.
We remove spaces, apostrophes, and commas, and while we're at it, let's convert the text to lowercase---our eldritch glyphs don't really *look* like they have a case distinction.

```python
letters_emma = " ".join(nltk.corpus.gutenberg.words('austen-emma.txt'))
letters_emma = re.sub("[\s',]","",emma).lower()
letters_counts_emma = collections.Counter(letters_emma)
letters_counts_emma = 
    sorted(letters_counts_emma.items(), key=lambda x: x[1], reverse=True)
```

```python
[('e', 86021), ('t', 59201), ('a', 54379), ('o', 53199), ('n', 47288), … ]
```

Let's plot those side by side, see if anything looks off about this.

![Frequency analysis graphs for single characters in the speech in Paper Girls compared to the speech in Jane Austen's Emma. The graphs look similar.](/public/images/paper-girls-frequency-analysis.png)\

Great news! 
The distribution of the letters looks like *language*.
For comparison, if we encrypt *Emma* and then do frequency analysis, it looks something like this:

![Frequency analysis graphs for single characters in the speech in Jane Austen's Emma encrypted compared to the speech in Jane Austen's Emma unencrypted. The frequency counts for the encrypted speech are virtually uniform.](/public/images/paper-girls-frequency-analysis-encrypted.png)\

Ok, so hypothesis: 

> The frequencies for *Paper Girls* overlap *exactly* with those of *Emma*.

Let's test it.
First, we build a translation table, and then we apply it to our data.

```python
trans = {
    l1: l2 for (l1, c1), (l2, c2) in zip(letters_counts, letters_counts_emma)}

data_transed = [
    ''.join(trans.get(c,c) for c in line) for line in data.splitlines()]
```

```python
[
    'yauqogdnti',
    "hpsnt wpi ce'ss ge;t ot tre hefadu vasundyi",
    'htom ocom vlag ge-',
    'htap-',
    'pseohe-',
    'mawle yandy ta yet mawlhes.eh knsseu-',
    'maw peapse ole awt av mawl',
    'da',
    …
]
```

Ok, so, maybe we're not so lucky?
Eugh, why do these things always have to be hard.

Well, our data is pretty sparse, so it makes sense that these wouldn't line up precisely, even if our teenagers *were* speaking English…
And I'm not particularily keen on going in and transcribing more, so I guess we'll just have to do more thinking.

Let's just assume that we got one or two characters right.
Those first peaks look pretty convincing, so let's assume '<span class="font-papergirls">E</span>' is 'e' and '<span class="font-papergirls">T</span>' is 't'.
What else can we do?


### Common words

We can have a look at the single-letter words!
Let's pretend we don't know English.
What are common single-letter words, according to *Emma*?

```python
single_letter_words_counts = collections.Counter(
    word for word in emma.split() if len(word) == 1 and word.isalpha())
single_letter_words_counts = sorted(
    single_letter_words_counts.items(), key=lambda x: x[1], reverse=True)
```

```python
[('I', 3178), ('a', 3004), ('s', 933), ('A', 125), ('t', 19), ('d', 12),
 ('E', 11),   ('o', 8),    ('F', 4),   ('W', 4),   ('V', 3),  … ]
```

Ah, yes, the well-known word 'W'. 
But at least we can be fairly sure that the only single-letter words which matter in English are 'I' and 'a', and we didn't even have to think!
Let's see what single-letter words our data has on offer…

```python
single_letter_words = set(word for word in data.split() if len(word) == 1)
```

<pre><code class="highlight"><span class="p">[</span><span class="s font-papergirls">'I'</span><span class="p">]</span></code></pre>

Well, that's disappointing, now we have to pick.
At least we're fairly sure now that '<span class="s font-papergirls">I</span>' will turn out to be either 'i' or 'a'.

We can repeat this trick for two letter words, three letter words, four letter words, etc.
However, we'll probably need quite a lot of data for word counts to start looking like your average word counts for English---like those found in *Emma* and only *Emma*.
Let's count up the two-letter words, and plot their frequencies:

![Frequency analysis graphs for two-letter words in the speech in Paper Girls compared to the speech in Jane Austen's Emma. The graphs look similar.](/public/images/paper-girls-frequency-analysis-two-letter.png)\

It's tempting to assume that those plots align exactly, but we're dealing with *really* sparse data at this point.
Instead, let's use the knowledge that we've uncovered so far to see if we can find any of those common words.
The words '<span class="font-papergirls">TO</span>', '<span class="font-papergirls">IT</span>', and '<span class="font-papergirls">AT</span>' all feature a '<span class="font-papergirls">T</span>'. 
They seem like pretty likely candidates for 'to', 'it', and 'at', so it seems quite likely that '<span class="font-papergirls">O</span>' is 'o', and that '<span class="font-papergirls">I</span>' is 'i' and '<span class="font-papergirls">A</span>' is 'a', or vice versa. Note that '<span class="font-papergirls">A</span>' is the fourth most character, like we'd expect from an 'i' or an 'a'. Based on the frequencies---'a' is a more common character than 'i', but 'I' is a more common word than 'a'---we can guess that '<span class="font-papergirls">I</span>' is 'i' and '<span class="font-papergirls">a</span>' is 'a'.

We could continue with this trick for quite a while, using three-letter words, four-letter words, bi-grams, tri-grams, etc. However, we could quite quickly run into the problem that the data we have is really quite sparse, and we can only really solve that by reading more *Paper Girls* and adding more phrases to our database… which… well, the first part of that sounds like fun. However, there is one trick which we can still quite easily exploit…


### Double letters

Let's do some counting of *repeated letters*, such as the 'tt' in letters, and see how those patterns compare to those found in English. We do this by zipping the data with the data offset by one--e.g. from `"hello"` we would get `[('h','e'), ('e','l'), ('l','l'), ('l','o')]`---and then picking only the pairs where both letters are the same---in our example, `('l','l')`.

```python
doubles = [ l1 + l2 for l1, l2 in zip(data,data[1:]) if l1 == l2 ]
doubles_counts = collections.Counter(doubles)
doubles_counts = sorted(doubles_counts.items(), key=lambda x: x[1], reverse=True)
```

You've got the hang of this by now---we count doubles in *Emma* as well, and plot both distributions.

![Frequency analysis graphs for bigrams in the speech in Paper Girls compared to the speech in Jane Austen's Emma. The graph for paper girls has only two entries.](/public/images/paper-girls-frequency-analysis-doubles-1.png)\

Whoa, so something kinda weird is going on here. First off, really? There's only *two* characters that are ever repeated? Like, our sample size is small, but not *that* small. Also, neither of these characters are anything we have a guess for yet… so if our guesses are correct, we don't have *any* occurances of 'ee' or 'tt'. Look at the chart for *Emma*, they're pretty common.

It may be a good idea to have a closer look at the data.
If you scroll up to where we defined `data`, you'll find '<span class="font-papergirls">Dd</span>' and '<span class="font-papergirls">Ee</span>', and actually, if we look at all the data, we'll find '<span class="font-papergirls">Oo</span>' and '<span class="font-papergirls">Ss</span>' as well.
These mirrored versions of the characters only seem to appear directly after a neutral versions of the same character, and they look *suspiciously* like repeated characters.
Actually, we should've asked this *a long time ago*. 
How many characters do we even have?

```python
num_characters = len(letters_counts)
```

```python
32
```

*WHAT?!* 32 CHARACTERS?! 
Right, that doesn't mash with our whole "Latin alphabet" thing.
Eugh, let's finish this "double characters" thing first, and then we'll look into our surplus of characters.

Let's count up the double characters *and* the characters followed by their mirrored image, and plot those against the double characters in *Emma*.

```python
doubles_or_mirrored = [
    l1 + l2 for l1, l2 in zip(data,data[1:]) if l1 == l2 or l1 == l2.mirrored() ]
doubles_or_mirrored_counts = collections.Counter(doubles_or_mirrored)
doubles_or_mirrored_counts = sorted(
    doubles_or_mirrored_counts.items(), key=lambda x: x[1], reverse=True)
```

![Frequency analysis graphs for bigrams in the speech in Paper Girls compared to the speech in Jane Austen's Emma. The graphs look similar.](/public/images/paper-girls-frequency-analysis-doubles-2.png)\

Ah, that makes *way* more sense. 
We can still probably assume that '<span class="font-papergirls">LL</span>' is 'll', and it probably makes sense to stick to our guns and say that '<span class="font-papergirls">Oo</span>' is 'oo' and '<span class="font-papergirls">Ee</span>' is 'ee'.

### Too many characters

Eek! So let's get to that whole "too many characters thing".
See, if our hypothesis is true, and this is really English, then we'd kinda expect to see at most 26 characters.
Realistically, we'd expect more like 20 to 23 characters.
For instance, if we check the frequency analysis for letters a while back, we see that 'v', 'k', 'x', 'j', 'q', and 'z' are rarer in English than the full stop.

So what do we do with our bonus characters?
Well, if we go with the idea that doubles are mirrored, then that at least takes care of four characters: '<span class="font-papergirls">o</span>', '<span class="font-papergirls">e</span>', '<span class="font-papergirls">d</span>', and '<span class="font-papergirls">s</span>'. That takes us down to only 28 characters.

So… four down, at least two to go?
The most common non-letter is the full stop, but it'd be a bit weird if the full stop were included in the alphabet, given that the comma and the apostrophe were just written as usual.
However, if we look at the data, we find three characters---<span class="font-papergirls">.</span>, <span class="font-papergirls">!</span>, and <span class="font-papergirls">?</span>---which *exclusively* appear at the end of words, and *almost exclusively* at the end of the entire speech bubble.
Moreover, almost every speech bubble ends with one of these characters, the only exceptions being chapter titles and *two* other utterances.
We can assume that these are '.', '!', and '?', in order of frequency.
Honestly, it doesn't hurt too much if we get these wrong, the only important thing is that---if these truly are punctuation---we don't try to assign letters to them.

### Write down everything we've got!

Right, so, we've made quite a few guesses by now, let's see if that gets us anywhere.
First, let's write down everything we've got…

<pre><code class="highlight"><span class="n">trans</span> <span class="o">=</span> <span class="p">{</span>
    <span class="s">" "</span><span class="p">:</span> <span class="s">" "</span><span class="p">,</span> <span class="s">"'"</span><span class="p">:</span> <span class="s">"'"</span><span class="p">,</span> <span class="s">","</span><span class="p">:</span> <span class="s">","</span><span class="p">,</span> 
    <span class="s font-papergirls">'A'</span><span class="p">:</span> <span class="s">'a'</span><span class="p">,</span> <span class="s font-papergirls">'E'</span><span class="p">:</span> <span class="s">'e'</span><span class="p">,</span> <span class="s font-papergirls">'e'</span><span class="p">:</span> <span class="s">'e'</span><span class="p">,</span> <span class="s font-papergirls">'T'</span><span class="p">:</span> <span class="s">'t'</span><span class="p">,</span> <span class="s font-papergirls">'I'</span><span class="p">:</span> <span class="s">'i'</span><span class="p">,</span> <span class="s font-papergirls">'o'</span><span class="p">:</span> <span class="s">'o'</span><span class="p">,</span> <span class="s font-papergirls">'O'</span><span class="p">:</span> <span class="s">'o'</span><span class="p">,</span> <span class="s font-papergirls">'L'</span><span class="p">:</span> <span class="s">'l'</span><span class="p">,</span> <span class="s font-papergirls">'.'</span><span class="p">:</span> <span class="s">'.'</span><span class="p">,</span> <span class="s font-papergirls">'!'</span><span class="p">:</span> <span class="s">'!'</span><span class="p">,</span> <span class="s font-papergirls">'?'</span><span class="p">:</span> <span class="s">'?'</span><span class="p">}</span></code></pre>

…and then use that to try and translate our data, replacing anything for which we haven't made a guess yet with an underscore.

```python
data_transed = [
    ''.join(trans.get(c,'_') for c in line) for line in data.splitlines()]
```

```python
['_o__a__it.',
 "__lit __. _e'll _eet at t_e _e_o__ _ol_i__.",
 '_ta_ a_a_ __o_ _e!',
 '_to_!',
 '_lea_e!',
 '_o__e _oi__ to _et _o___el_e_ _ille_!',
 '_o_ _eo_le a_e o_t o_ _o__',
 '_o',
 …]
```

From here, it's a bit of a word puzzle. 
For instance, "_e'll _eet at t_e" looks like it's saying "we'll meet at the".
Let's add those guesses, and translate again, see if they make sense?

```python
['_o__am_it.',
 "__lit __. we'll meet at the _e_o__ _ol_i__.",
 '_ta_ awa_ __om me!',
 '_to_!',
 '_lea_e!',
 '_o__e _oi__ to _et _o___el_e_ _ille_!',
 '_o_ _eo_le a_e o_t o_ _o__',
 '_o',
 …]
```

Does "awa_ __om me" say "away from me"?
Let's see if that makes sense!

```python
['_o__am_it.',
 "__lit __. we'll meet at the _e_o__ fol_i__.",
 '_tay away from me!',
 '_to_!',
 '_lea_e!',
 'yo_re _oi__ to _et yo_r_el_e_ _ille_!',
 'yo_ _eo_le are o_t of yo_r',
 '_o',
 …]
```

Right, *stay*, as in "stay away from me!"
If we'd kept the source text in mind, we would've probably seen that.
Though I often find it easier to stare only at the underscored text, to not be distracted by these otherworldly glyphs.

```python
['_o__am_it.',
 "s_lit __. we'll meet at the se_o__ fol_i__.",
 'stay away from me!',
 'sto_!',
 '_lease!',
 'yo_re _oi__ to _et yo_rsel_es _ille_!',
 'yo_ _eo_le are o_t of yo_r',
 '_o',
 …]
```

Things are starting to get pretty clear.
"Stop!", "Please!", "youre going to get yourselves killed!", "you people are out of your"…
We can keep doing this, and eventually we'll manage to find guesses for most of the letters.
The authors have even included an 'x' and a 'q' in such natural words such as 'vertex' and 'tuplequad'.
Honestly, that last one took me a while. The 'ua' and the fact that it was either gonna be a 'q' or a 'z' helped.

### …and then you make a nice table

Woo! 
We've done it! 
We can read the bizzare glyphs in *Paper Girls* now!
And the only letter we're missing is the 'z'.
Hope I'll find one of those in one of the later books.

<table style="font-size:xx-large;">
<tbody>
<tr>
<td><span class="font-papergirls">A</span></td>
<td><span class="font-papergirls">B</span></td>
<td><span class="font-papergirls">C</span></td>
<td><span class="font-papergirls">D</span></td>
<td><span class="font-papergirls">E</span></td>
<td><span class="font-papergirls">F</span></td>
<td><span class="font-papergirls">G</span></td>
<td><span class="font-papergirls">H</span></td>
<td><span class="font-papergirls">I</span></td>
<td><span class="font-papergirls">J</span></td>
</tr>
<tr>
<td>a</td>
<td>b</td>
<td>c</td>
<td>d</td>
<td>e</td>
<td>f</td>
<td>g</td>
<td>h</td>
<td>i</td>
<td>j</td>
</tr>
<tr>
<td><span class="font-papergirls">K</span></td>
<td><span class="font-papergirls">L</span></td>
<td><span class="font-papergirls">M</span></td>
<td><span class="font-papergirls">N</span></td>
<td><span class="font-papergirls">O</span></td>
<td><span class="font-papergirls">P</span></td>
<td><span class="font-papergirls">Q</span></td>
<td><span class="font-papergirls">R</span></td>
<td><span class="font-papergirls">S</span></td>
<td><span class="font-papergirls">T</span></td>
</tr>
<tr>
<td>k</td>
<td>l</td>
<td>m</td>
<td>n</td>
<td>o</td>
<td>p</td>
<td>q</td>
<td>r</td>
<td>s</td>
<td>t</td>
</tr>
<tr>
<td><span class="font-papergirls">U</span></td>
<td><span class="font-papergirls">V</span></td>
<td><span class="font-papergirls">W</span></td>
<td><span class="font-papergirls">X</span></td>
<td><span class="font-papergirls">Y</span></td>
<td><span class="font-papergirls">_</span></td>
<td><span class="font-papergirls"> </span></td>
<td><span class="font-papergirls">.</span></td>
<td><span class="font-papergirls">!</span></td>
<td><span class="font-papergirls">?</span></td>
</tr>
<tr>
<td>u</td>
<td>v</td>
<td>w</td>
<td>x</td>
<td>y</td>
<td>z</td>
<td> </td>
<td>.</td>
<td>!</td>
<td>?</td>
</tr>
</tbody>
</table>

Wrapping up, what have we learned?
I guess the important things are that you can pretty easily crack any substitution cipher by boldly making some assumptions about what the language is, and then exploiting letter frequencies from that language… and that you can exploit way more than just *plain* letter freqencies---short words, n-grams, repeated characters, they're all good candidates for frequency analysis.

Also… *Paper Girls* is fucking rad, go read that shit.

[papergirls]: https://imagecomics.com/comics/releases/paper-girls-1
[johnny23]: https://www.goodreads.com/book/show/11161195-johnny-23
[xedout]: https://www.goodreads.com/book/show/7814774-x-ed-out
[rsa]: https://en.wikipedia.org/wiki/RSA_(cryptosystem)
[quenya]: https://en.wikipedia.org/wiki/Quenya
[klingon]: https://en.wikipedia.org/wiki/Klingon_language
[dothraki]: https://en.wikipedia.org/wiki/Dothraki_language
[valyrian]: https://en.wikipedia.org/wiki/Valyrian_languages
[volapuk]: https://en.wikipedia.org/wiki/Volap%C3%BCk
[solresol]: https://en.wikipedia.org/wiki/Solresol
[emma]: https://en.wikipedia.org/wiki/Emma_(novel)
