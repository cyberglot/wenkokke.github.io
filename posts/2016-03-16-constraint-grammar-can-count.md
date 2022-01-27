---
title: "Constraint Grammar can count!"
---

Constraint grammar---it is a natural language processing formalism with great two distinctions: it routinely scores amongst the highest in tasks such as part-of-speech tagging and word-sense disambiguation, with F-scores at around 99%; and it has made some of the most dubious choices in programming language syntax in history. Though its specification has changed tremendously since CG1, it is nontheless a grammar formalism which sees a lot of usage. One natural question to ask of any grammar formalism is "how expressive is it?"

Over the weekend, [inariksit](https://github.com/inariksit) visited me, and we decided to find out!

<!--more-->

It's not immediately obvious how to even approach this question, as constraint grammar doesn't *generate* strings per se. It simply *constrains* existing, ambiguous strings. We took the following approach: we view a constraint grammar as a formal language $\mathcal{L}$, generated over an alphabet $\Sigma$. We generate the strings in our language by passing maximally ambiguous strings of *every* length to the grammar. With maximally ambiguous, I mean those strings where each position contains the entire alphabet, so $\langle \Sigma \rangle_n$. A constraint grammar is said to *accept* a string $w$ of length $n$ if, when we pass $\langle \Sigma \rangle_n$ as an input to the CG, $w$ is one of the possible interpretations of its output.[^downside]

[^downside]: An obvious downside to this approach is that for finite languages (as well as infinite ones, but duh) the CG will never stop generating the language, as we have to feed it $\langle \Sigma \rangle_n$ *for every $n$*. But we're playing fast and loose here, so what gives.

The specification of CG3 mentions tags such as `EXTERNAL`, which passes information to an external command. So constraint grammar is obviously Turing complete. However, that's a little bit boring, so let's see what we can say about the expressiveness of the absolute core of constraint grammar: `REMOVE` with sections. If we leave out sections, there is no recursion, and therefore the language will be strictly finite and boring. If we leave out `REMOVE` then there is no way to restrict strings, so we'd only have the languages $\Sigma^*$ for any $\Sigma$.

There are a few concessions we will allow ourselves. If we had `MAP`, `ADD`, or any such other command, we would have a way to store information. In this strict fragment, all we have is the current set of symbol assignments. Therefore, we will allow ourselves a second alphabet $\Sigma\prime$ of *hidden* symbols---i.e. symbols that we are not allowed to pass to the output. In addition, we update our definition of $\mathcal{L}$ to state that we pass in $\langle \Sigma \cup \Sigma\prime \rangle_n$. This is not *strictly* necessary in CG3, as we could use `APPEND` to add these hidden characters, but we would like to stay as faithful to our fragment as possible.

One last hurdle is that constraint grammar has no notion of *failure*. The worst that can happen is that a grammar changes nothing. Worse so, if there is only one reading left, the `REMOVE` command will have no effect. So one more concession we make is that we allow ourselves to use the `REMCOHORT` command---which removes an entire "cohort", or "position" in our terminology---for the *sole purpose* of deleting the entire string if it is not accepted.

From here on out, when we say 'CG3', we are referring to this fragment of constraint grammar.


# CG3 is not regular; the language $a^nb^n$

In this section we show that CG3, restricted to sections and `REMOVE` is not regular. We show this by implementing a grammar for the counting language $a^nb^n$.

The first thing we do is to try and detect the edges of the string. CG3 has "magical" constants for this, called `>>>` and `<<<` for the left and right edge, respectively. However, we cannot use those. Instead, we define them ourselves using two hidden variables, which we also call `>>>` and `<<<`. We do this as follows:

```python
SET ANY = A OR B;

BEFORE-SECTIONS
REMOVE >>> (-1 ANY);
REMOVE <<< ( 1 ANY);
```

Initially, all positions will be labeled with both `>>>` and `<<<`. These above rules check whether there is *any* position preceding or succeeding the current position, and if so, delete `>>>` or `<<<`. As a result, the first position will be the only one tagged `>>>`, and the last the only one tagged `<<<`.[^magic][^beforeafter]

[^magic]: CG3's magic constants are just outside of the string, whereas ours are right at the edge of the string. Therefore, all indices using magic constants are moved by one.

[^beforeafter]: While we use `BEFORE-SECTIONS` and `AFTER-SECTIONS` throughout this post, their usage is not strictly necessary. The grammar also works if everything is executed under a single `SECTION`.

Next, we note that *all* strings in the language $a^nb^n$ are of even length, and that every even length corresponds to *exactly* one string. Therefore, we must reject all strings of uneven length. We assume two more hidden symbols, `EVEN` and `ODD`. We can use these to label whether a position is even or odd: we know the first position is odd, so we delete `EVEN`; we know that positions following odd positions must be even, so we delete `ODD`; and we know that positions following even positions are `ODD`, so we delete `EVEN`...[^link]

[^link]: Note that `LINK` is a conjunction, but one in which indices in the *second* argument are interpreted from the perspective of the position matched in the first.

```python
BEFORE-SECTIONS
REMOVE EVEN (0 >>>);

SECTION
REMOVE ODD  (NOT  0 >>> LINK NOT -1 EVEN);
REMOVE EVEN (NOT -1 ODD);
```

It's exactly this "marking as even by deleting odd" that makes it a bit of a confusing read, so if you'd like to play around with an example, [my full code with examples is available here](https://gist.github.com/wenkokke/e5f76d82939ecc9d3a4c), and [vislcg3 is available here](http://beta.visl.sdu.dk/cg3/chunked/installation.html).

Anyway, after performing this labelling, we can check if the last position is even, and if so, delete all positions:[^emptystring]

[^emptystring]: We have chosen to describe faillure by outputting the empty string. If we would have been more careful, we could have added a dedicated symbol for failure. However, under our current definitions we compare languages *minus* the empty string.

```python
AFTER-SECTIONS
REMCOHORT ANY (1* <<< LINK NOT 0 EVEN);
REMCOHORT <<< (NOT 0 EVEN);
```

Now that we are certain that we only accept even-length strings, it is safe to say that the first symbol must be an $a$, and the last must be a $b$:[^select]

[^select]: We can use `SELECT`, since it is equivalent to calling
    `REMOVE` with the complement---i.e. remove everything *but* its
    argument.

```python
BEFORE-SECTIONS
SELECT A (0 >>>);
SELECT B (0 <<<);
```

And now it's only a matter of slowly growing these $a$s and $b$s until they meet. We do this as follows: in each pass, we mark the position *after* the last definite $a$ as a candidate for $a$ (written `OPT_A`), and do likewise for the last position *before* the first definite $b$. Then we mark each candidate $a$ and $b$ as *definite*, and we continue:[^noeffect]

[^noeffect]: Note that `SELECT A` has no effect if $a$ is not a valid option, and that `REMOVE A` has no effect if $a$ is the *only* remaining option.

```python
SECTION
REMOVE OPT_B (-1C A);
REMOVE OPT_A ( 1C B);
SELECT A (NOT 0 OPT_B);
SELECT B (NOT 0 OPT_A);
```

The grammar described so far exactly expresses the language $a^nb^n$.[^emptystring] Since this language is not regular, we can conclude that constraint grammar is not regular.


# CG3 is not context-free; the language $a^nb^nc^n$

In this section we show that CG3, restricted to sections and `REMOVE` is not context-free. We show this by implementing a grammar for the counting language $a^nb^nc^n$.

The language $a^nb^nc^n$ has us divide strings whose length is a multiple of three into three even chunks. The first part of this is obviously to find the bounds of the input string, as before, and make sure that it has a length divisible by three. We can trivially extend our previous approach---now abandoning "even" and "odd" in favour of `X1`, `X2` and `X3`:

```python
SET X1_OR_X2 = X1 OR X2;
SET X2_OR_X3 = X2 OR X3;
SET X3_OR_X1 = X3 OR X1;

BEFORE-SECTIONS
REMOVE X2_OR_X3 (0 >>>)

SECTION
REMOVE X3_OR_X1 (NOT 0 >>> LINK NOT -1 X2_OR_X3)
REMOVE X1_OR_X2 (NOT 0 >>> LINK NOT -1 X3_OR_X1)
REMOVE X2_OR_X3 (NOT 0 >>> LINK NOT -1 X1_OR_X2)

AFTER-SECTIONS
REMCOHORT ANY (1* <<< LINK NOT 0 X3)
REMCOHORT <<< (NOT 0 X3)
```

Note that, somewhat counterintuitively, `REMOVE X1_OR_X2`[^x_or_y] removes *both* `X1` and `X2`, but `0 X1_OR_X2` matches if the current position still has either option.

[^x_or_y]: When we write `X_OR_Y`, this means that we have defined a
    "set" as `SET X1_OR_X2 = X1 OR X2;`. The reason for this is that
    CG3 does not allow the *inline* use of set primitives.

Now that we can be sure that our string is of some length $3n$, we can proceed to divide it into three equal chunks. One good way to do this, is to start by finding the middle. This is *exactly* what we did in our grammar for $a^nb^n$. Below we implement the same, but now *without* using `SELECT`, as using this would erase all other tags. For this, we assume four new hidden symbols `FST`, `SND`---for first and second half---and `OPT_*` varieties:

```python
SET NOT_FST = OPT_FST OR SND OR OPT_SND ;
SET NOT_SND = FST OR OPT_FST OR OPT_SND ;

BEFORE-SECTIONS
REMOVE NOT_FST (0 >>>)
REMOVE NOT_SND (0 <<<)

SECTION
REMOVE OPT_SND (-1 FST LINK (NOT 0 NOT_FST))
REMOVE OPT_FST ( 1 SND LINK (NOT 0 NOT_SND))
REMOVE NOT_FST (0 FST LINK 0 SND LINK 0 OPT_FST LINK NOT 0 OPT_SND)
REMOVE NOT_SND (0 FST LINK 0 SND LINK 0 OPT_SND LINK NOT 0 OPT_FST)
```

Once we've divided the word in half, it becomes fairly easy to point out the middle. Below, we mark the first position as $a$, the last position as $c$ and the middle position as $b$:[^middletwo]

[^middletwo]: If the string has an even-numbered length, we in fact mark the middle *two* positions as $b$.

```python
SET OPT_A_OR_B = (OPT_A OR OPT_B);
SET OPT_B_OR_C = (OPT_A OR OPT_B);
SET OPT_C_OR_D = (OPT_A OR OPT_B);

BEFORE-SECTIONS
REMOVE OPT_B_OR_C (0 >>>)
REMOVE OPT_A_OR_B (0 <<<)

SECTION
REMOVE OPT_C_OR_A (0 FST LINK  1 SND LINK NOT 0 FST)
REMOVE OPT_C_OR_A (0 SND LINK -1 FST LINK NOT 0 SND)
```

And finally, we grow $a$ and $b$, and $b$ and $c$ towards one another as we did before. Note that we have to let $a$ and $c$ grow twice every time we grow $b$, because $b$ is growing in *two* directions at the same time:

```python
SECTION
REMOVE OPT_B_OR_C (-1C A)
REMOVE OPT_A_OR_B ( 1C C)
SELECT A (0 OPT_A LINK NOT 0 OPT_B_OR_C)
SELECT C (0 OPT_C LINK NOT 0 OPT_A_OR_B)

REMOVE OPT_C_OR_A ( 1C B)
REMOVE OPT_C_OR_A (-1C B)
SELECT B (0 OPT_B LINK NOT 0 OPT_C_OR_A)

REMOVE OPT_B_OR_C (-1C A)
REMOVE OPT_A_OR_B ( 1C C)
SELECT A (0 OPT_A LINK NOT 0 OPT_B_OR_C)
SELECT C (0 OPT_C LINK NOT 0 OPT_A_OR_B)
```

The grammar described so far exactly expresses the language $a^nb^nc^n$. Since this language is not context-free, we can conclude that constraint grammar is not context-free.


# Beyond Context-Free

It seems pretty obvious that a language formalism whose only construct has the power to observe *all* of its surrounding context ends up being at least context-sensitive. I could continue. It is still fairly straightforward to generate the language $a^nb^nc^nd^n$---divide into half, and divide halves into half---and using similar strategies, you can keep on constructing CGs which compute the counting language $\sigma_1^n\cdots \sigma_k^n$ for any $k$ as long as you can come up with new strategies for prime numbers.. but this won't do us a whole lot of good---at least, it won't help us escape the class of context-sensitive languages.

So for now, let's leave it at this. I'm a little bored of programming CG at any rate. If you want to have a go, [my full code and examples are available here](https://gist.github.com/wenkokke/e5f76d82939ecc9d3a4c), and [vislcg3 is available here](http://beta.visl.sdu.dk/cg3/chunked/installation.html).

---
