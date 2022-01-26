<p>Back when I wrote this, I had just discovered <a href="http://okmij.org/ftp/Haskell/extensible/">“Extensible Effects: an alternative to Monad Transformers”</a> by Oleg Kiselyov, Amr Sabry, Cameron Swords, and Hiromi Ishii, and I’ve always had a penchant for mucking about with linguistics and Haskell… so… let’s have a little fun with this library and some basic AB grammars in Haskell, see how far we can get within the universally well-defined maximum length of a blog post!</p>
<!--more-->
<p>Before we start, let’s get a clear idea of what we’re going to try and accomplish. It’s more or less a well known fact that natural language has tons of side-effects—sometimes also referred to as “non-compositional phenomena”. Let’s look at some examples:</p>
<ol type="1">
<li>“I cooked up a delicious dinner!”</li>
<li>“There! I walked the damn dog!”</li>
<li>“As Mary left, she whistled a cheery tune.”</li>
</ol>
<p>In (1), the word “I” is non-compositional: it’s a word which you can always use, but which changes its meaning depending on the context—on who uses it. In (2) we have the word “damn”, an expressive. There’s pretty extensive literature on expressives—see, for instance, Daniel Gutzmann’s <a href="http://www.danielgutzmann.com/work/use-conditional-meaning">“Use-conditional meaning”</a>—but the gist of it is as follows: “damn” doesn’t affect the <em>truth</em> of a sentence. If I come back from walking the dog, even though I do not like dogs, and say “There! I walked the damn dog!”, you can’t reply by saying “No, you didn’t! The dog is nice!” Instead, “damn” conveys it’s meaning on some sort of side-channel. Finally, in (3) we have “she”, which again has a context-dependent meaning. However, in this situation, “she” doesn’t get its meaning from the context in which the sentence is uttered. Instead, reading this sentence in isolation, it seems pretty likely that “she” refers to Mary.</p>
<p>“Non-compositional phenomena” is a bit of a misnomer for the phenomena in (1-3). We can implement these phenomena as <em>side-effects</em>, and as we know from functional programming, side-effects are often perfectly compositional. In fact, the above phenomena correspond, in Haskell-lingo, to a <em>Reader</em>, a <em>Writer</em> and a <em>State</em> monad.<a href="#fn1" class="footnote-ref" id="fnref1" role="doc-noteref"><sup>1</sup></a> However, rolling together various different monads can be a tedious chore. In addition, when we’re writing what a word means, we might not <em>want</em> to specify its meaning for <em>all possible side-effects</em>. Since linguistics is continually changing, we might not even want to commit to what all possible side-effects <em>are</em>.</p>
<p>So this is why I got excited when I saw the latest library for extensible effects. If you don’t know what extensible effects are, I’d recommend <a href="http://okmij.org/ftp/Haskell/extensible/">the paper linked above</a>. But anyway, what I’m going to do in this post is: develop a parser, which parses Haskell strings, looks up the words in a dictionary of <em>effectful</em> Haskell functions, and composes these to get some meaning for the sentence. Here’s an example that you’ll see again at the end of the post, except then it’ll actually work!</p>
<div class="sourceCode" id="cb1"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb1-1"><a href="#cb1-1" aria-hidden="true" tabindex="-1"></a><span class="fu">lex</span><span class="ot"> ::</span> <span class="dt">String</span> <span class="ot">-&gt;</span> [<span class="dt">SomeEffectfulFunction</span>]</span>
<span id="cb1-2"><a href="#cb1-2" aria-hidden="true" tabindex="-1"></a><span class="fu">lex</span> <span class="st">&quot;tim&quot;</span>    <span class="ot">=</span> [ <span class="dt">NP</span> , <span class="dt">Tim</span>             ]</span>
<span id="cb1-3"><a href="#cb1-3" aria-hidden="true" tabindex="-1"></a><span class="fu">lex</span> <span class="st">&quot;bob&quot;</span>    <span class="ot">=</span> [ <span class="dt">NP</span> , <span class="dt">Bob</span>             ]</span>
<span id="cb1-4"><a href="#cb1-4" aria-hidden="true" tabindex="-1"></a><span class="fu">lex</span> <span class="st">&quot;likes&quot;</span>  <span class="ot">=</span> [ <span class="dt">TV</span> , <span class="dt">Like</span>            ]</span>
<span id="cb1-5"><a href="#cb1-5" aria-hidden="true" tabindex="-1"></a><span class="fu">lex</span> <span class="st">&quot;stupid&quot;</span> <span class="ot">=</span> [ <span class="dt">AP</span> , <span class="op">&lt;</span> <span class="fu">id</span> , <span class="dt">Stupid</span> <span class="op">&gt;</span> ]</span>
<span id="cb1-6"><a href="#cb1-6" aria-hidden="true" tabindex="-1"></a>  <span class="co">-- ^ Has an identity (i.e. no) meaning, but</span></span>
<span id="cb1-7"><a href="#cb1-7" aria-hidden="true" tabindex="-1"></a>  <span class="co">--   but conveys `Stupid` as a side-effect.</span></span>
<span id="cb1-8"><a href="#cb1-8" aria-hidden="true" tabindex="-1"></a><span class="fu">lex</span> <span class="st">&quot;him&quot;</span>    <span class="ot">=</span> [ <span class="dt">NP</span> , magic           ]</span>
<span id="cb1-9"><a href="#cb1-9" aria-hidden="true" tabindex="-1"></a>  <span class="co">-- ^ Has some magic way of obtaining the</span></span>
<span id="cb1-10"><a href="#cb1-10" aria-hidden="true" tabindex="-1"></a>  <span class="co">--   thing that&#39;s referenced.</span></span>
<span id="cb1-11"><a href="#cb1-11" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb1-12"><a href="#cb1-12" aria-hidden="true" tabindex="-1"></a><span class="ot">example ::</span> [(<span class="dt">Pred</span>, [<span class="dt">Pred</span>])]</span>
<span id="cb1-13"><a href="#cb1-13" aria-hidden="true" tabindex="-1"></a>example <span class="ot">=</span> parseWith <span class="dt">Tim</span> <span class="st">&quot;(stupid bob) likes him&quot;</span></span>
<span id="cb1-14"><a href="#cb1-14" aria-hidden="true" tabindex="-1"></a>  <span class="co">-- &gt; [(Like Bob Tim, [Stupid Bob])]</span></span></code></pre></div>
<h3 id="ab-grammars-in-haskell">AB Grammars in Haskell</h3>
<p>Well, first off, don’t let this scare you off… but we are going to do this in Haskell, and we’re going to need a LOT of language extensions. This is because we’re basically going to parse strings to Haskell functions:</p>
<div class="sourceCode" id="cb2"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb2-1"><a href="#cb2-1" aria-hidden="true" tabindex="-1"></a><span class="ot">{-# LANGUAGE</span></span>
<span id="cb2-2"><a href="#cb2-2" aria-hidden="true" tabindex="-1"></a><span class="ot">    TemplateHaskell, QuasiQuotes, FlexibleInstances, </span></span>
<span id="cb2-3"><a href="#cb2-3" aria-hidden="true" tabindex="-1"></a><span class="ot">    FlexibleContexts, TypeFamilies, GADTs, TypeOperators, </span></span>
<span id="cb2-4"><a href="#cb2-4" aria-hidden="true" tabindex="-1"></a><span class="ot">    DataKinds, PolyKinds, RankNTypes, KindSignatures, </span></span>
<span id="cb2-5"><a href="#cb2-5" aria-hidden="true" tabindex="-1"></a><span class="ot">    UndecidableInstances, StandaloneDeriving, RecordWildCards, </span></span>
<span id="cb2-6"><a href="#cb2-6" aria-hidden="true" tabindex="-1"></a><span class="ot">    DeriveFunctor, DeriveFoldable, DeriveTraversable #-}</span></span></code></pre></div>
<!--
```haskell
import Prelude hiding (lookup,lex)
import Control.Applicative ((<|>),empty,liftA2)
import Data.Maybe (maybeToList)
import Data.Singletons.Decide (Decision(..),(:~:)(..),(%~))
import Data.Singletons.Prelude
import Data.Singletons.TH (singletons)
import Eff1 (Eff,run,Reader,runReader,ask,Writer,tell,runWriter)
import Text.Parsec (char,letter,spaces,many1,chainr1,parse)
```
-->
<p>In addition, we’re going to use the following packages:</p>
<ul>
<li><a href="https://hackage.haskell.org/package/singletons">singletons</a>;</li>
<li><a href="http://okmij.org/ftp/Haskell/extensible/">extensible effects</a>;</li>
<li><a href="https://hackage.haskell.org/package/parsec">parsec</a>;</li>
<li><a href="https://hackage.haskell.org/package/markdown-unlit">markdown-unlit</a>.</li>
</ul>
<p>I’ve included a copy the extensible effects code in <a href="https://github.com/wenkokke/side-effects-in-english/">the repository</a>.</p>
<p>Before we start off, let’s review some basic AB-grammar knowledge. In general, a categorial grammar—of which AB-grammars are an instance—consist of three things:</p>
<ol type="1">
<li>a typed language $_1$;</li>
<li>a typed language $_2$; and</li>
<li>a translation $Tr$ from $_1$ to $_2$.</li>
</ol>
<p>The language $_1$ describes the <em>grammar</em> of our language, whereas $_2$ will describe its <em>meaning</em>. And one more important requirement: if we have a type in $_1$, then we should have some efficient way of getting all the programs of that type—this will be our parsing algorithm.</p>
<p>In the case of AB-grammars, $_1$ has the following types:</p>
<p>$$A, B S N NP A B B/A$$</p>
<p>The programs in this language consist of a bunch of constants, which represent words. It also has two rules for building programs, of them variants of function application:</p>
<p>$$  $$</p>
<p>The language $_2$ is the simply-typed lambda calculus, typed with only the primitive types $e$ and $t$, for entities and truth-values:</p>
<p>$$, e t $$</p>
<p>It also has a set of typed constants, which we use to represent the abstract meanings of words. This means it contains familiar logical operators, like ${} : t t t$ or $: (e t) t$, but also things like $ : e t$, the predicate which tests whether or not something is a cat.</p>
<p>The translation function then maps the types for $_1$ to types for $_2$, and the words in $_1$ to expressions in $_2$. For the types, the translation is as follows:</p>
$$
<p>$$</p>
<p>The translation on the level of programs is simple: programs in $_1$ consist <em>solely</em> of function applications and some constants. As long as we don’t make promises in the types of those constants that we cannot keep, we should be fine!</p>
<p>So, let’s start off by creating some Haskell data types to represent the syntactic and semantic types described above:</p>
<div class="sourceCode" id="cb3"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb3-1"><a href="#cb3-1" aria-hidden="true" tabindex="-1"></a>singletons [d|</span>
<span id="cb3-2"><a href="#cb3-2" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb3-3"><a href="#cb3-3" aria-hidden="true" tabindex="-1"></a>    data SynT = S | N | NP | SynT :\ SynT | SynT :/ SynT</span>
<span id="cb3-4"><a href="#cb3-4" aria-hidden="true" tabindex="-1"></a>              deriving (Show,Eq)</span>
<span id="cb3-5"><a href="#cb3-5" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb3-6"><a href="#cb3-6" aria-hidden="true" tabindex="-1"></a>    data SemT = E | T | SemT :-&gt; SemT</span>
<span id="cb3-7"><a href="#cb3-7" aria-hidden="true" tabindex="-1"></a>              deriving (Show,Eq)</span>
<span id="cb3-8"><a href="#cb3-8" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb3-9"><a href="#cb3-9" aria-hidden="true" tabindex="-1"></a>  |]</span></code></pre></div>
<p>The <code>singletons</code> function that we’re using here is important. It’s a template Haskell function which, given some datatype, defines its “singleton”. A “singleton” is a Haskell data type which has the same structure on the value level and on the type level. For the type <code>SynT</code> above, that means that the <code>singletons</code> function generates a second data type:</p>
<div class="sourceCode" id="cb4"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb4-1"><a href="#cb4-1" aria-hidden="true" tabindex="-1"></a><span class="kw">data</span> <span class="dt">SSynT</span> (<span class="ot">ty ::</span> <span class="dt">SynT</span>) <span class="kw">where</span></span>
<span id="cb4-2"><a href="#cb4-2" aria-hidden="true" tabindex="-1"></a>  <span class="dt">SS</span><span class="ot">    ::</span> <span class="dt">SSynT</span> <span class="dt">S</span></span>
<span id="cb4-3"><a href="#cb4-3" aria-hidden="true" tabindex="-1"></a>  <span class="dt">SN</span><span class="ot">    ::</span> <span class="dt">SSynT</span> <span class="dt">N</span></span>
<span id="cb4-4"><a href="#cb4-4" aria-hidden="true" tabindex="-1"></a>  <span class="dt">SNP</span><span class="ot">   ::</span> <span class="dt">SSynT</span> <span class="dt">NP</span></span>
<span id="cb4-5"><a href="#cb4-5" aria-hidden="true" tabindex="-1"></a>  (<span class="op">:%</span>\)<span class="ot"> ::</span> <span class="dt">SSynT</span> a <span class="ot">-&gt;</span> <span class="dt">SSynT</span> b <span class="ot">-&gt;</span> <span class="dt">SSynT</span> (a <span class="op">:</span>\ b)</span>
<span id="cb4-6"><a href="#cb4-6" aria-hidden="true" tabindex="-1"></a><span class="ot">  (:%/) ::</span> <span class="dt">SSynT</span> b <span class="ot">-&gt;</span> <span class="dt">SSynT</span> a <span class="ot">-&gt;</span> <span class="dt">SSynT</span> (b <span class="op">:/</span> a)</span></code></pre></div>
<p>By using the singleton of some value, we can get that value <em>on the type level</em>—and by pattern matching on a singleton, we can pattern match on types! For now, just be aware that those data types are generated. They will become relevant soon enough.</p>
<p>First off, though—we probably should’ve done this right away—let’s just set some fixities for our type-level operators:</p>
<div class="sourceCode" id="cb5"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb5-1"><a href="#cb5-1" aria-hidden="true" tabindex="-1"></a><span class="kw">infixr</span> <span class="dv">5</span> <span class="op">:</span>\</span>
<span id="cb5-2"><a href="#cb5-2" aria-hidden="true" tabindex="-1"></a><span class="kw">infixl</span> <span class="dv">5</span> <span class="op">:/</span></span>
<span id="cb5-3"><a href="#cb5-3" aria-hidden="true" tabindex="-1"></a><span class="kw">infixr</span> <span class="dv">5</span> <span class="op">:-&gt;</span></span></code></pre></div>
<p>And while we’re at it, let’s create some type-level aliases for common parts of speech—though I cannot say that this treatment of appositive modifiers is entirely common:<a href="#fn2" class="footnote-ref" id="fnref2" role="doc-noteref"><sup>2</sup></a></p>
<div class="sourceCode" id="cb6"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb6-1"><a href="#cb6-1" aria-hidden="true" tabindex="-1"></a><span class="kw">type</span> <span class="dt">IV</span> <span class="ot">=</span> <span class="dt">NP</span> <span class="op">:</span>\ <span class="dt">S</span>  <span class="co">-- intransitive verbs</span></span>
<span id="cb6-2"><a href="#cb6-2" aria-hidden="true" tabindex="-1"></a><span class="kw">type</span> <span class="dt">TV</span> <span class="ot">=</span> <span class="dt">IV</span> <span class="op">:/</span> <span class="dt">NP</span> <span class="co">-- transitive verbs</span></span>
<span id="cb6-3"><a href="#cb6-3" aria-hidden="true" tabindex="-1"></a><span class="kw">type</span> <span class="dt">AP</span> <span class="ot">=</span> <span class="dt">NP</span> <span class="op">:/</span> <span class="dt">NP</span> <span class="co">-- appositive modifier</span></span>
<span id="cb6-4"><a href="#cb6-4" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb6-5"><a href="#cb6-5" aria-hidden="true" tabindex="-1"></a>sIV <span class="ot">=</span> <span class="dt">SNP</span> <span class="op">:%</span>\ <span class="dt">SS</span></span>
<span id="cb6-6"><a href="#cb6-6" aria-hidden="true" tabindex="-1"></a>sTV <span class="ot">=</span> sIV <span class="op">:%/</span> <span class="dt">SNP</span></span>
<span id="cb6-7"><a href="#cb6-7" aria-hidden="true" tabindex="-1"></a>sAP <span class="ot">=</span> <span class="dt">SNP</span> <span class="op">:%/</span> <span class="dt">SNP</span></span></code></pre></div>
<p>So now that we’ve defined the types of the languages $_1$ and $_2$, we can define our translation <em>on types</em>. Note that our previous definition of our translation function was already more-or-less valid Haskell:</p>
<div class="sourceCode" id="cb7"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb7-1"><a href="#cb7-1" aria-hidden="true" tabindex="-1"></a><span class="kw">type</span> <span class="kw">family</span> <span class="dt">Tr</span> (<span class="ot">ty ::</span> <span class="dt">SynT</span>)<span class="ot"> ::</span> <span class="dt">SemT</span> <span class="kw">where</span></span>
<span id="cb7-2"><a href="#cb7-2" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Tr</span> <span class="dt">S</span>        <span class="ot">=</span> <span class="dt">T</span></span>
<span id="cb7-3"><a href="#cb7-3" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Tr</span> <span class="dt">N</span>        <span class="ot">=</span> <span class="dt">E</span> <span class="op">:-&gt;</span> <span class="dt">T</span></span>
<span id="cb7-4"><a href="#cb7-4" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Tr</span> <span class="dt">NP</span>       <span class="ot">=</span> <span class="dt">E</span></span>
<span id="cb7-5"><a href="#cb7-5" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Tr</span> (a <span class="op">:</span>\ b) <span class="ot">=</span> <span class="dt">Tr</span> a <span class="op">:-&gt;</span> <span class="dt">Tr</span> b</span>
<span id="cb7-6"><a href="#cb7-6" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Tr</span> (b <span class="op">:/</span> a) <span class="ot">=</span> <span class="dt">Tr</span> a <span class="op">:-&gt;</span> <span class="dt">Tr</span> b</span></code></pre></div>
<p>Let’s assume for now that we have some sort of data type that we wish to use to represent our semantic terms, for instance:</p>
<div class="sourceCode" id="cb8"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb8-1"><a href="#cb8-1" aria-hidden="true" tabindex="-1"></a><span class="kw">data</span> <span class="dt">Expr</span> (<span class="ot">ty ::</span> <span class="dt">SemT</span>) <span class="kw">where</span></span>
<span id="cb8-2"><a href="#cb8-2" aria-hidden="true" tabindex="-1"></a>  <span class="dt">John</span><span class="ot"> ::</span> <span class="dt">Expr</span> <span class="dt">E</span></span>
<span id="cb8-3"><a href="#cb8-3" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Mary</span><span class="ot"> ::</span> <span class="dt">Expr</span> <span class="dt">E</span></span>
<span id="cb8-4"><a href="#cb8-4" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Like</span><span class="ot"> ::</span> <span class="dt">Expr</span> (<span class="dt">E</span> <span class="op">:-&gt;</span> <span class="dt">E</span> <span class="op">:-&gt;</span> <span class="dt">T</span>)</span>
<span id="cb8-5"><a href="#cb8-5" aria-hidden="true" tabindex="-1"></a><span class="ot">  (:$) ::</span> <span class="dt">Expr</span> (a <span class="op">:-&gt;</span> b) <span class="ot">-&gt;</span> <span class="dt">Expr</span> a <span class="ot">-&gt;</span> <span class="dt">Expr</span> b</span></code></pre></div>
<p>While we have a way of talking about terms of a certain type—e.g. by saying <code>Expr E</code> we can talk about all entities—we cannot really leave the type open and talk about <em>all</em> well-typed terms, regardless of type. For this we need to introduce a new data type:</p>
<div class="sourceCode" id="cb9"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb9-1"><a href="#cb9-1" aria-hidden="true" tabindex="-1"></a><span class="kw">data</span> <span class="dt">Typed</span> (<span class="ot">expr ::</span> <span class="dt">SemT</span> <span class="ot">-&gt;</span> <span class="op">*</span>) <span class="ot">=</span> <span class="kw">forall</span> a<span class="op">.</span> <span class="dt">Typed</span> (<span class="dt">SSynT</span> a, expr (<span class="dt">Tr</span> a))</span></code></pre></div>
<p>The <code>Typed</code> data-type contains a tuple of a singleton for a semantic type, and an expression. Notice that the type-level variable <code>a</code> is shared between the singleton and the expression, which means that the expression in the second position is forced to be of the type given in the first.</p>
<p>Our definition of <code>Typed</code> has one type-level parameter, <code>expr</code>, which represents the type of expressions. One possible value for this is the <code>Expr</code> type we sketched earlier—for instance, some values of the type <code>Typed Expr</code> would be <code>(SE, John)</code>, <code>(SE, Mary)</code>, <code>(ST, Like :$ John :$ Mary)</code> and <code>(SE %:-&gt; ST, Like :$ Mary)</code>.</p>
<p>We are abstracting over the expressions used, but we’re going to need them to support <em>at least</em> function application—as this is what AB grammars are built around. Therefore, we’re going to make a tiny type class which encodes function application of functions using the semantic types:</p>
<div class="sourceCode" id="cb10"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb10-1"><a href="#cb10-1" aria-hidden="true" tabindex="-1"></a><span class="kw">class</span> <span class="dt">SemE</span> (<span class="ot">expr ::</span> <span class="dt">SemT</span> <span class="ot">-&gt;</span> <span class="op">*</span>) <span class="kw">where</span></span>
<span id="cb10-2"><a href="#cb10-2" aria-hidden="true" tabindex="-1"></a><span class="ot">    apply ::</span> <span class="kw">forall</span> a b<span class="op">.</span> expr (a <span class="op">:-&gt;</span> b) <span class="ot">-&gt;</span> expr a <span class="ot">-&gt;</span> expr b</span></code></pre></div>
<p>Using this <code>apply</code> function, we can define application on <code>Typed</code> expression as well.<a href="#fn3" class="footnote-ref" id="fnref3" role="doc-noteref"><sup>3</sup></a> Since these expressions hide their type, we cannot enforce on the type-level that this application necessarily succeeds. What we’re doing in the function is the following:</p>
<ol type="1">
<li>we pattern match to check if either the left or the right type is an appropriate function type;</li>
<li>we use the type-level equality function <code>%~</code> to check if the argument type is the same in both cases; and</li>
<li>if so, we apply <code>apply</code>.</li>
</ol>
<p>In all other cases, we’re forced to return <code>Nothing</code>:</p>
<div class="sourceCode" id="cb11"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb11-1"><a href="#cb11-1" aria-hidden="true" tabindex="-1"></a><span class="ot">maybeApply ::</span> <span class="dt">SemE</span> expr <span class="ot">=&gt;</span> <span class="dt">Typed</span> expr <span class="ot">-&gt;</span> <span class="dt">Typed</span> expr <span class="ot">-&gt;</span> <span class="dt">Maybe</span> (<span class="dt">Typed</span> expr)</span>
<span id="cb11-2"><a href="#cb11-2" aria-hidden="true" tabindex="-1"></a>maybeApply (<span class="dt">Typed</span> (a1,x)) (<span class="dt">Typed</span> (a2 <span class="op">:%</span>\ b,f)) <span class="ot">=</span></span>
<span id="cb11-3"><a href="#cb11-3" aria-hidden="true" tabindex="-1"></a>  <span class="kw">case</span> a1 <span class="op">%~</span> a2 <span class="kw">of</span></span>
<span id="cb11-4"><a href="#cb11-4" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Proved</span> <span class="dt">Refl</span> <span class="ot">-&gt;</span> <span class="fu">pure</span> (<span class="dt">Typed</span> (b, apply f x))</span>
<span id="cb11-5"><a href="#cb11-5" aria-hidden="true" tabindex="-1"></a>    _           <span class="ot">-&gt;</span> empty</span>
<span id="cb11-6"><a href="#cb11-6" aria-hidden="true" tabindex="-1"></a>maybeApply (<span class="dt">Typed</span> (b <span class="op">:%/</span> a1,f)) (<span class="dt">Typed</span> (a2,x)) <span class="ot">=</span></span>
<span id="cb11-7"><a href="#cb11-7" aria-hidden="true" tabindex="-1"></a>  <span class="kw">case</span> a1 <span class="op">%~</span> a2 <span class="kw">of</span></span>
<span id="cb11-8"><a href="#cb11-8" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Proved</span> <span class="dt">Refl</span> <span class="ot">-&gt;</span> <span class="fu">pure</span> (<span class="dt">Typed</span> (b, apply f x))</span>
<span id="cb11-9"><a href="#cb11-9" aria-hidden="true" tabindex="-1"></a>    _           <span class="ot">-&gt;</span> empty</span>
<span id="cb11-10"><a href="#cb11-10" aria-hidden="true" tabindex="-1"></a>maybeApply _ _ <span class="ot">=</span> empty</span></code></pre></div>
<p>What we’ve implemented above is just a <em>check</em> to see if some given pair of expressions can be applied as function and argument. Applied repeatedly, this corresponds to checking if some given syntax tree has a well-typed function-argument structure. If we want to do this, we’re going to need some sort of trees:</p>
<div class="sourceCode" id="cb12"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb12-1"><a href="#cb12-1" aria-hidden="true" tabindex="-1"></a><span class="kw">data</span> <span class="dt">Tree</span> a <span class="ot">=</span> <span class="dt">Leaf</span> a <span class="op">|</span> <span class="dt">Node</span> (<span class="dt">Tree</span> a) (<span class="dt">Tree</span> a)</span>
<span id="cb12-2"><a href="#cb12-2" aria-hidden="true" tabindex="-1"></a>            <span class="kw">deriving</span> (<span class="dt">Show</span>, <span class="dt">Functor</span>, <span class="dt">Foldable</span>, <span class="dt">Traversable</span>)</span></code></pre></div>
<p>However, since we don’t actually want to write these horribly verbose things, we’re going to use parser combinators to implement a tiny parser which parses sentences of the form “(the unicorn) (found jack) first”:</p>
<div class="sourceCode" id="cb13"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb13-1"><a href="#cb13-1" aria-hidden="true" tabindex="-1"></a><span class="ot">parseTree ::</span> <span class="dt">String</span> <span class="ot">-&gt;</span> <span class="dt">Maybe</span> (<span class="dt">Tree</span> <span class="dt">String</span>)</span>
<span id="cb13-2"><a href="#cb13-2" aria-hidden="true" tabindex="-1"></a>parseTree str <span class="ot">=</span> <span class="kw">case</span> parse sent <span class="st">&quot;&quot;</span> str <span class="kw">of</span></span>
<span id="cb13-3"><a href="#cb13-3" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Left</span>  _ <span class="ot">-&gt;</span> empty</span>
<span id="cb13-4"><a href="#cb13-4" aria-hidden="true" tabindex="-1"></a>  <span class="dt">Right</span> t <span class="ot">-&gt;</span> <span class="fu">pure</span> t</span>
<span id="cb13-5"><a href="#cb13-5" aria-hidden="true" tabindex="-1"></a>  <span class="kw">where</span></span>
<span id="cb13-6"><a href="#cb13-6" aria-hidden="true" tabindex="-1"></a>    sent <span class="ot">=</span> chainr1 atom node</span>
<span id="cb13-7"><a href="#cb13-7" aria-hidden="true" tabindex="-1"></a>      <span class="kw">where</span></span>
<span id="cb13-8"><a href="#cb13-8" aria-hidden="true" tabindex="-1"></a>        word <span class="ot">=</span> <span class="dt">Leaf</span> <span class="op">&lt;$&gt;</span> many1 letter</span>
<span id="cb13-9"><a href="#cb13-9" aria-hidden="true" tabindex="-1"></a>        atom <span class="ot">=</span> word <span class="op">&lt;|&gt;</span> (char <span class="ch">&#39;(&#39;</span> <span class="op">*&gt;</span> (sent <span class="op">&lt;*</span> char <span class="ch">&#39;)&#39;</span>))</span>
<span id="cb13-10"><a href="#cb13-10" aria-hidden="true" tabindex="-1"></a>        node <span class="ot">=</span> <span class="fu">pure</span> <span class="dt">Node</span> <span class="op">&lt;*</span> spaces</span></code></pre></div>
<p>That is to say, for our parser, spaces form nodes in the tree, and are taken to be right associative. So, the example above represents the following tree:</p>
<pre><code>        -----------
       /           \
      /           ----
     /           /    \
   ----        ----    \
  /    \      /    \    \
the unicorn found jack first</code></pre>
<p>Last, before we can write out full implementation of “parsing” with AB grammars, we’re going to need the concept of a lexicon. In our case, a lexicon will be a function from string to lists of typed expressions (because a word can have multiple interpretations):</p>
<div class="sourceCode" id="cb15"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb15-1"><a href="#cb15-1" aria-hidden="true" tabindex="-1"></a><span class="kw">type</span> <span class="dt">Lexicon</span> expr <span class="ot">=</span> <span class="dt">String</span> <span class="ot">-&gt;</span> [<span class="dt">Typed</span> expr]</span></code></pre></div>
<p>Parsing consists of four stages:</p>
<ol type="1">
<li>we parse the given string into a tree;</li>
<li>we look up the words in the tree in the lexicon;</li>
<li>we combine the words using <code>maybeApply</code> as defined above; and</li>
<li>we return those resulting terms that are of the correct type.</li>
</ol>
<p>Below, you see the function written out in full. Note that the <code>checkType</code> function once again makes use of the type-level equality function <code>%~</code>:</p>
<div class="sourceCode" id="cb16"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb16-1"><a href="#cb16-1" aria-hidden="true" tabindex="-1"></a><span class="ot">parseWith ::</span> <span class="dt">SemE</span> expr </span>
<span id="cb16-2"><a href="#cb16-2" aria-hidden="true" tabindex="-1"></a>          <span class="ot">=&gt;</span> <span class="dt">Lexicon</span> expr <span class="ot">-&gt;</span> <span class="dt">String</span> <span class="ot">-&gt;</span> <span class="dt">SSynT</span> a <span class="ot">-&gt;</span> [expr (<span class="dt">Tr</span> a)]</span>
<span id="cb16-3"><a href="#cb16-3" aria-hidden="true" tabindex="-1"></a>parseWith <span class="fu">lex</span> str a1 <span class="ot">=</span> <span class="kw">do</span></span>
<span id="cb16-4"><a href="#cb16-4" aria-hidden="true" tabindex="-1"></a>    wordTree <span class="ot">&lt;-</span> maybeToList (parseTree str)</span>
<span id="cb16-5"><a href="#cb16-5" aria-hidden="true" tabindex="-1"></a>    exprTree <span class="ot">&lt;-</span> <span class="fu">traverse</span> <span class="fu">lex</span> wordTree</span>
<span id="cb16-6"><a href="#cb16-6" aria-hidden="true" tabindex="-1"></a>    expr     <span class="ot">&lt;-</span> combine exprTree</span>
<span id="cb16-7"><a href="#cb16-7" aria-hidden="true" tabindex="-1"></a>    checkType expr</span>
<span id="cb16-8"><a href="#cb16-8" aria-hidden="true" tabindex="-1"></a>    <span class="kw">where</span></span>
<span id="cb16-9"><a href="#cb16-9" aria-hidden="true" tabindex="-1"></a>      <span class="co">-- Check if type a1 == a2, and if so return the</span></span>
<span id="cb16-10"><a href="#cb16-10" aria-hidden="true" tabindex="-1"></a>      <span class="co">-- expression. Otherwise return Nothing.</span></span>
<span id="cb16-11"><a href="#cb16-11" aria-hidden="true" tabindex="-1"></a>      checkType (<span class="dt">Typed</span> (a2,x)) <span class="ot">=</span></span>
<span id="cb16-12"><a href="#cb16-12" aria-hidden="true" tabindex="-1"></a>        <span class="kw">case</span> a1 <span class="op">%~</span> a2 <span class="kw">of</span></span>
<span id="cb16-13"><a href="#cb16-13" aria-hidden="true" tabindex="-1"></a>          <span class="dt">Proved</span> <span class="dt">Refl</span> <span class="ot">-&gt;</span> <span class="fu">pure</span> x</span>
<span id="cb16-14"><a href="#cb16-14" aria-hidden="true" tabindex="-1"></a>          _           <span class="ot">-&gt;</span> empty</span>
<span id="cb16-15"><a href="#cb16-15" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb16-16"><a href="#cb16-16" aria-hidden="true" tabindex="-1"></a>      <span class="co">-- Combine the expressions in the tree using the maybeApply</span></span>
<span id="cb16-17"><a href="#cb16-17" aria-hidden="true" tabindex="-1"></a>      <span class="co">-- function, defined above.</span></span>
<span id="cb16-18"><a href="#cb16-18" aria-hidden="true" tabindex="-1"></a>      combine (<span class="dt">Leaf</span> e)     <span class="ot">=</span> <span class="fu">pure</span> e</span>
<span id="cb16-19"><a href="#cb16-19" aria-hidden="true" tabindex="-1"></a>      combine (<span class="dt">Node</span> t1 t2) <span class="ot">=</span></span>
<span id="cb16-20"><a href="#cb16-20" aria-hidden="true" tabindex="-1"></a>        <span class="kw">do</span> e1 <span class="ot">&lt;-</span> combine t1</span>
<span id="cb16-21"><a href="#cb16-21" aria-hidden="true" tabindex="-1"></a>           e2 <span class="ot">&lt;-</span> combine t2</span>
<span id="cb16-22"><a href="#cb16-22" aria-hidden="true" tabindex="-1"></a>           maybeToList (maybeApply e1 e2)</span></code></pre></div>
<h3 id="interpretations-in-haskell">Interpretations in Haskell</h3>
<p>Now comes the part where all this mucking about with singleton types really pays off. Because our expressions are typed, and sound with respect to Haskell’s type system, we can choose Haskell to be our semantic language. That means that we now have the ability to parse strings to valid Haskell functions.</p>
<p>First, let’s set up a small language to represent our world, which in this case is mostly made up of Bob and Tim:</p>
<div class="sourceCode" id="cb17"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb17-1"><a href="#cb17-1" aria-hidden="true" tabindex="-1"></a><span class="kw">data</span> <span class="dt">Entity</span> <span class="ot">=</span> <span class="dt">Tim</span> <span class="co">-- ^ Tim is a carpenter and an introvert, likes</span></span>
<span id="cb17-2"><a href="#cb17-2" aria-hidden="true" tabindex="-1"></a>                  <span class="co">--   holding hands and long walks on the beach.</span></span>
<span id="cb17-3"><a href="#cb17-3" aria-hidden="true" tabindex="-1"></a>            <span class="op">|</span> <span class="dt">Bob</span> <span class="co">-- ^ Bob is an aspiring actor, and a social media</span></span>
<span id="cb17-4"><a href="#cb17-4" aria-hidden="true" tabindex="-1"></a>                  <span class="co">--   junkie. Likes travelling, beer, and Tim.</span></span>
<span id="cb17-5"><a href="#cb17-5" aria-hidden="true" tabindex="-1"></a>            <span class="kw">deriving</span> (<span class="dt">Show</span>)</span>
<span id="cb17-6"><a href="#cb17-6" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb17-7"><a href="#cb17-7" aria-hidden="true" tabindex="-1"></a><span class="kw">data</span> <span class="dt">Pred</span> <span class="ot">=</span> <span class="dt">Like</span> <span class="dt">Entity</span> <span class="dt">Entity</span> <span class="co">-- ^ Is it &#39;like&#39; or &#39;like like&#39;?</span></span>
<span id="cb17-8"><a href="#cb17-8" aria-hidden="true" tabindex="-1"></a>          <span class="op">|</span> <span class="dt">Stupid</span> <span class="dt">Entity</span>      <span class="co">-- ^ This is definitely not &#39;like like&#39;.</span></span>
<span id="cb17-9"><a href="#cb17-9" aria-hidden="true" tabindex="-1"></a>          <span class="kw">deriving</span> (<span class="dt">Show</span>)</span></code></pre></div>
<p>Secondly, we could turn our expressions into plain Haskell expressions, but that would be dull. Language isn’t side-effect free—there’s all kinds of stuff going on! So, we’re going to use a library for <a href="http://okmij.org/ftp/Haskell/extensible/">extensible effects</a> written by Oleg Kiselyov, Amr Sabry, Cameron Swords, and Hiromi Ishii.</p>
<p>Let’s translate our semantic types into effectful Haskell types! And, most importantly, let’s keep the set of effects <code>r</code> unspecified!</p>
<div class="sourceCode" id="cb18"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb18-1"><a href="#cb18-1" aria-hidden="true" tabindex="-1"></a><span class="kw">type</span> <span class="kw">family</span> <span class="dt">ToEff</span> r<span class="ot"> t ::</span> <span class="op">*</span> <span class="kw">where</span></span>
<span id="cb18-2"><a href="#cb18-2" aria-hidden="true" tabindex="-1"></a>  <span class="dt">ToEff</span> r <span class="dt">E</span>         <span class="ot">=</span> <span class="dt">Eff</span> r <span class="dt">Entity</span></span>
<span id="cb18-3"><a href="#cb18-3" aria-hidden="true" tabindex="-1"></a>  <span class="dt">ToEff</span> r <span class="dt">T</span>         <span class="ot">=</span> <span class="dt">Eff</span> r <span class="dt">Pred</span></span>
<span id="cb18-4"><a href="#cb18-4" aria-hidden="true" tabindex="-1"></a>  <span class="dt">ToEff</span> r (a <span class="op">:-&gt;</span> b) <span class="ot">=</span> <span class="dt">ToEff</span> r a <span class="ot">-&gt;</span> <span class="dt">ToEff</span> r b</span></code></pre></div>
<p>Now, because Haskell is being a buzzkill about using un-saturated type families, we have to wrap our translation in a newtype to be able to use it with the <code>Typed</code> definition and the <code>SemE</code> type class. And because of this, we also have to convince Haskell that these wrapped Haskell functions can be applied:</p>
<div class="sourceCode" id="cb19"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb19-1"><a href="#cb19-1" aria-hidden="true" tabindex="-1"></a><span class="kw">newtype</span> <span class="dt">Ext</span> r a <span class="ot">=</span> <span class="dt">Ext</span> (<span class="dt">ToEff</span> r a)</span>
<span id="cb19-2"><a href="#cb19-2" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb19-3"><a href="#cb19-3" aria-hidden="true" tabindex="-1"></a><span class="kw">instance</span> <span class="dt">SemE</span> (<span class="dt">Ext</span> r) <span class="kw">where</span></span>
<span id="cb19-4"><a href="#cb19-4" aria-hidden="true" tabindex="-1"></a>  apply (<span class="dt">Ext</span> f) (<span class="dt">Ext</span> x) <span class="ot">=</span> <span class="dt">Ext</span> (f x)</span></code></pre></div>
<p>But now we’re all ready to go! First, let’s determine the effects we want to use in our library. We could still leave this under specified, and only mention which effects we expect to be supported… but that would be much more verbose:</p>
<div class="sourceCode" id="cb20"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb20-1"><a href="#cb20-1" aria-hidden="true" tabindex="-1"></a><span class="kw">type</span> <span class="dt">RW</span> <span class="ot">=</span> (<span class="dt">Reader</span> <span class="dt">Entity</span> &#39;<span class="op">:</span> <span class="dt">Writer</span> <span class="dt">Pred</span> &#39;<span class="op">:</span> &#39;[])</span></code></pre></div>
<p>Hooray! We can have a lexicon now! And it’s reasonably simple, too!</p>
<div class="sourceCode" id="cb21"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb21-1"><a href="#cb21-1" aria-hidden="true" tabindex="-1"></a><span class="fu">lex</span><span class="ot"> ::</span> <span class="dt">String</span> <span class="ot">-&gt;</span> [<span class="dt">Typed</span> (<span class="dt">Ext</span> <span class="dt">RW</span>)]</span>
<span id="cb21-2"><a href="#cb21-2" aria-hidden="true" tabindex="-1"></a><span class="fu">lex</span> <span class="st">&quot;tim&quot;</span>    <span class="ot">=</span> [ <span class="dt">Typed</span> (<span class="dt">SNP</span> , <span class="dt">Ext</span> (<span class="fu">pure</span> <span class="dt">Tim</span>))                            ]</span>
<span id="cb21-3"><a href="#cb21-3" aria-hidden="true" tabindex="-1"></a><span class="fu">lex</span> <span class="st">&quot;bob&quot;</span>    <span class="ot">=</span> [ <span class="dt">Typed</span> (<span class="dt">SNP</span> , <span class="dt">Ext</span> (<span class="fu">pure</span> <span class="dt">Bob</span>))                            ]</span>
<span id="cb21-4"><a href="#cb21-4" aria-hidden="true" tabindex="-1"></a><span class="fu">lex</span> <span class="st">&quot;likes&quot;</span>  <span class="ot">=</span> [ <span class="dt">Typed</span> (sTV , <span class="dt">Ext</span> (liftA2 (<span class="fu">flip</span> <span class="dt">Like</span>)))                  ]</span>
<span id="cb21-5"><a href="#cb21-5" aria-hidden="true" tabindex="-1"></a><span class="fu">lex</span> <span class="st">&quot;stupid&quot;</span> <span class="ot">=</span> [ <span class="dt">Typed</span> (sAP , <span class="dt">Ext</span> (<span class="op">&gt;&gt;=</span> \x <span class="ot">-&gt;</span> tell (<span class="dt">Stupid</span> x) <span class="op">*&gt;</span> <span class="fu">pure</span> x)) ]</span>
<span id="cb21-6"><a href="#cb21-6" aria-hidden="true" tabindex="-1"></a><span class="fu">lex</span> <span class="st">&quot;him&quot;</span>    <span class="ot">=</span> [ <span class="dt">Typed</span> (<span class="dt">SNP</span> , <span class="dt">Ext</span> ask)                                   ]</span></code></pre></div>
<p>The first two definitions simply return Tim and Bob as effect-free constants—hence the application of <code>pure</code>. Tim and Bob are both of type <code>Entity</code>, and through our translation, <code>NP</code> gets translated to <code>Eff r Entity</code>, so this works out.</p>
<p>Then, the predicate <code>Like</code> is simply lifted by <code>liftA2</code>, which is similar to <code>pure</code>, but for binary functions. The <code>flip</code> is present because according to… <em>egh</em>… <em>grammar</em>, <code>Like</code> will take its object first and its subject second… but for readability, we’d like that to be the other way around.</p>
<p>The definition for “stupid” acts as an identity function on entities, but inserts a predicate into the “appositive dimension”. This corresponds to the linguistic analysis of expressives: they don’t contribute to the sentence meaning, but store their meanings in some other meaning dimension—in this case, a <code>Writer</code> monad!</p>
<p>And last, the definition for “him” simply asks a <code>Reader</code> monad what it’s interpretation should be! A more complex example of anaphora resolution would be to also include a <code>Writer</code> monad, and have entities submit themselves as potential referents, then have this <code>Writer</code> monad periodically empty itself into the <code>Reader</code> monad, e.g. at sentence or clause boundaries, and have anaphora consume the first appropriate referent… But we digress!</p>
<p>We’re still stuck with these unresolved effects coming from our lexicon. So we’re going to define a function <code>runExt</code>, which handles all effects in order, and then escapes the <code>Eff</code> monad:</p>
<div class="sourceCode" id="cb22"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb22-1"><a href="#cb22-1" aria-hidden="true" tabindex="-1"></a><span class="ot">runExt ::</span> <span class="dt">Entity</span> <span class="ot">-&gt;</span> <span class="dt">Ext</span> <span class="dt">RW</span> <span class="dt">T</span> <span class="ot">-&gt;</span> (<span class="dt">Pred</span>, [<span class="dt">Pred</span>])</span>
<span id="cb22-2"><a href="#cb22-2" aria-hidden="true" tabindex="-1"></a>runExt x (<span class="dt">Ext</span> e) <span class="ot">=</span> run (runWriter (runReader e x))</span></code></pre></div>
<p>And with all this in place, we can handle an example sentence:</p>
<div class="sourceCode" id="cb23"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb23-1"><a href="#cb23-1" aria-hidden="true" tabindex="-1"></a><span class="ot">example ::</span> [(<span class="dt">Pred</span>, [<span class="dt">Pred</span>])]</span>
<span id="cb23-2"><a href="#cb23-2" aria-hidden="true" tabindex="-1"></a>example <span class="ot">=</span> runExt <span class="dt">Tim</span> <span class="op">&lt;$&gt;</span> parseWith <span class="fu">lex</span> <span class="st">&quot;(stupid bob) likes him&quot;</span> <span class="dt">SS</span></span></code></pre></div>
<p>Which evaluates to: <code>[(Like Bob Tim,[Stupid Bob])]</code></p>
<hr />
<section class="footnotes footnotes-end-of-document" role="doc-endnotes">
<hr />
<ol>
<li id="fn1" role="doc-endnote"><p>For a more hands-on implementation of side-effects in natural language using <em>monads</em>, see <a href="https://github.com/dylnb/esslli2015-monads" class="uri">https://github.com/dylnb/esslli2015-monads</a>.<a href="#fnref1" class="footnote-back" role="doc-backlink">↩︎</a></p></li>
<li id="fn2" role="doc-endnote"><p>The convention in the singletons library is to define the singleton version of a constructor by prefixing it with an <code>S</code>. Obviously, since the above definitions aren’t constructors, we can’t do that. However, we stick as close to the convention as possible in naming these “derived” singletons <code>sIV</code>, <code>sTV</code> and <code>sAP</code>.<a href="#fnref2" class="footnote-back" role="doc-backlink">↩︎</a></p></li>
<li id="fn3" role="doc-endnote"><p>It is the repeated application of this function which corresponds to backward-chaining proof search in the more general framework of categorial grammar. However, AB grammars <em>only</em> support function application, and therefore our “proof search” (1) can return at most one result, and (2) is more-or-less just a cursory check to see if the types match.<a href="#fnref3" class="footnote-back" role="doc-backlink">↩︎</a></p></li>
</ol>
</section>