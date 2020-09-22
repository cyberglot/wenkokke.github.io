---
title        : "Breadboxes, Plenty Questions and Distributional Semantics"
date         : 2016-03-21 12:00:00
tags         : [games, linguistics, python]
---

Quite a while ago, [UnicornPower][UnicornPower] introduced me to a game called *Breadbox*. It's an experimental cousin of *20 Questions*, also known as *Plenty Questions*, which is played by two players---or more, really---who we'll name Allie and Blake:

 1. Allie thinks of something.
 2. As their first question, Blake asks *"Is it a breadbox?"*
 3. Allie---who, seeing the mandatory first question, obviously
    wouldn't choose a breadbox---answers *"No, it's not!"*

From there on out, all Blake's questions have to be of the form…

> Is it more like a *breadbox*, or more like…?

…where *breadbox* is replaced by whatever the current guess is, and the dots are filled in with whatever Blake wants. 

<!--more-->

For instance, a quick game might look like this:

Allie
: I'm thinking of something…

Blake
: Is it a breadbox?

Allie
: No, it's not.

Blake
: Is it more like a breadbox or more like a dog?

Allie
: It's more like a dog…

Blake
: Is it more like a dog or more like a cat?

Allie
: It's more like a cat…

Blake
: Is it more like a cat or more like a unicorn?

Allie
: It's more like a cat…

Blake
: Is it more like a cat or more like a garden?

Allie
: It's more like a garden…

Blake
: Is it more like a garden or more like a house?

Allie
: It's more like a house…

Blake
: Is it more like a house or more like a friend?

Allie
: It's more like a friend…

Blake
: Is it more like a friend or more like a lover?

Allie
: It's more like a friend…

Blake
: Is it more like a friend or more like a relative?

Allie
: It's more like a friend…

Blake
: Is it more like a friend or more like a neighbour?

Allie
: That's exactly what I was thinking of!

Since this game tends to bring out the… best in people, Blake might
just have to give up. After this, Allie may be called on to explain
what the word she chose even means:

Blake
: I give up… what was it?

Allie
: Reverberation!

Allie
: It's the repetition of a sound resulting from reflection of the sound waves!

The game does a *fantastic* job of revealing how hierarchical people tend to think. For instance, I would say that a dolphin is more like an orca than it is like a mammal, *even though a dolphin isn't an orca*. But while playing the game, I often find myself slipping into hierarchical thinking: "Oh, it's not like an animal… so we can exclude all animals."

It is exactly this---the fact that the game forces you to consider a distance metric for your internal ontology, insofar as one exists---which makes it so fascinating to play. I heartily recommend you try it!

If you're upset because Breadbox is hard, or because you think that the choices it makes are weird or wrong… try playing it with a person!

---

> *Hiya! It’s Wen from the future, here to tell you that I used to have a copy of Breadbox running on OpenShift, where you could try it out for yourself. Unfortunately, that died with the deprecation of OpenShift 2, and I haven’t been able to find anywhere else that would let me host a 2GB web app for free…*[^source]

---

In the summer of 2014, I took a course on distributional semantics (by Marco Baroni and Georgiana Dinu) and the first thing I thought to do was to use their [dataset][semantic-vectors] to implement an AI for Breadbox. ~~Try it here!~~

So how does it work? The core hypothesis of distributional semantics is

> You can know the meaning of a word by the company it keeps

Have a look at the two sentences below:[^wampimuk]

 1. He filled the *wampimuk*, passed it around and we all drunk some.
 2. We found a little, hairy *wampimuk* sleeping behind the tree.

While you've probably never read the word wampimuk before, it's likely that either sentence will give you a pretty clear idea of what a wampurnik is *in that context*.

So if you want to know what a word (e.g. "bear") means, you take a *huge* corpus, and you look for all the occurances of that word…[^webcorp]

<pre style="margin-left: 3em;">
       over the mountains. A <a style="color:dark-orange;">bear</a> also features prominentl
    rnejakt" (An Unfortunate <a style="color:dark-orange;">Bear</a> Hunt) by Theodor Kittels
       to his hagiography, a <a style="color:dark-orange;">bear</a> killed Saint Corbinian's
         however, he let the <a style="color:dark-orange;">bear</a> go. The saddled "bear
       bear go. The saddled "<a style="color:dark-orange;">bear</a> of St. Corbinian" the
    tails on this topic, see <a style="color:dark-orange;">Bear</a> in heraldry. The British
         Cat and the Russian <a style="color:dark-orange;">Bear</a> (see The Great Game)
     Great Game) The Russian <a style="color:dark-orange;">bear</a> is a common national
    Soviet Union). The brown <a style="color:dark-orange;">bear</a> is also Finland's nation
    animals and had the same <a style="color:dark-orange;">bear</a> carry him from his hermi
         thus Christianised, <a style="color:dark-orange;">bear</a> clasping each gable
     evidence of prehistoric <a style="color:dark-orange;">bear</a> worship. Anthropologists
     peoples, considered the <a style="color:dark-orange;">bear</a> as the spirit of one's
    fathers. This is why the <a style="color:dark-orange;">bear</a> (karhu) was a greatly
    ikämmen and kontio). The <a style="color:dark-orange;">bear</a> is the national animal
      tries to kill a mother <a style="color:dark-orange;">bear</a> and her cubs—and is
         society. "The Brown <a style="color:dark-orange;">Bear</a> of Norway" is a Scottish
     magically turned into a <a style="color:dark-orange;">bear</a>, and who managed to get
     television. Evidence of <a style="color:dark-orange;">bear</a> worship has been found
      mythology identify the <a style="color:dark-orange;">bear</a> as their ancestor and
    shopric of Freising, the <a style="color:dark-orange;">bear</a> is the dangerous totem
</pre>

…and then you count, for every other word, how often it occurs together with your word. The above text, for instance, gives us a number of obvious co-occurances for bear: mountain, kill, hunt, brown, animal and cub. The idea is that, given a large enough corpus, these co-occurances will drown out the noisier ones.

By doing this for *every* word, you build up a co-occurance matrix, which lists how often every word occurs with every other word. For instance,[^wampimuk]

         leash    walk     run      owner    pet      bark
-------  -------  -------  -------  -------  -------  -------
dog      3        5        2        5        3        2
cat      0        3        3        2        3        0
lion     0        3        2        0        1        0
light    0        0        0        0        0        0
bark     1        0        0        2        1        0
car      0        0        1        3        0        0

Now the meaning for a word is determined by its co-occurances with some select group of words. For instance, if we look at 'dog' we see that it strongly co-occurs with things such as 'leash', 'walk', 'owner', 'pet' and 'bark'. For 'cat', we lose 'leash' and 'bark'---since cats don't bark, and are rarely leashed. And for 'lion', we also lose 'owner' and 'pet'---while a lion could conceivably be a pet, it'd be a lot rarer than having a cat or dog… and we could never really feel like we *owned* the lion.

You can think of the rows in this matrix as points, in a six dimensional space---one dimension for every column. And because it's a space, you can measure the distance between words. And this is where we get back to Breadbox: in order to play this bizarre game, we needed a distance metric for meanings, to be able to compare and order *any two objects*. And this is *exactly* what a co-occurance matrix gives us!

Obviously, there's a lot more to distributional semantics than just this. For instance, the matrices that you derive this way tend to be huge---one axis per word, one point per word---so there's a whole bunch of work which goes into selecting exactly which set of words should be the axes. Then there's the difficultly of *composing* word meanings. You may have noticed that my breadbox implementation doesn't work all too well for compound nouns: that's because I'm not taking the effort to compose meaning vectors.[^abitofashame]

If you wish to read more about distributional semantics, there's a pretty good overview of introductions and surveys [here][slides]. Additionally, there's a whole branch of work which uses neural networks to learn the word meanings: for instance, have a look at [Word2Vec][word2vec].

---

[^source]: You can still try to run it [from source][breadbox], but be warned… I did not future-proof it.
[DontCountPredict]: http://clic.cimec.unitn.it/marco/publications/acl2014/baroni-etal-countpredict-acl2014.pdf
[UnicornPower]: https://github.com/UnicornPower
[word2vec]: http://deeplearning4j.org/word2vec
[slides]: https://www.cs.utexas.edu/~mooney/cs388/slides/dist-sem-intro-NLP-class-UT.pdf
[semantic-vectors]: http://clic.cimec.unitn.it/composes/semantic-vectors.html
[breadbox]: https://github.com/wenkokke/Breadbox
[^wampimuk]: Taken from <https://www.cs.utexas.edu/~mooney/cs388/slides/dist-sem-intro-NLP-class-UT.pdf>.
[^webcorp]: Taken from <http://www.webcorp.org.uk>.
[^abitofashame]: A bit of a shame, really, since the course I took was *about* composing meaning vectors. Also, full disclosure, the vectors that I used were created using [Word2Vec][word2vec], using neural networks. Such vectors generally outperform counting vectors in tasks of relatedness (see *[Don't Count, Predict!][DontCountPredict]* by Baroni et al.).
