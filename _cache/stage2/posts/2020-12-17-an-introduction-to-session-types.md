<p>Session types. Ostensibly, I’ve studied them for the past few years, so I should know something about them, right? I am gonna try and explain the <em>foundations</em> of session types, and along the way, there will be programs which crash, Victorian ladies having milk puddings, and tin can telephones.</p>
<!--more-->
<p>Let’s start out with a <em>dramatis personæ</em>.</p>
<h3 id="dramatis-personæ">Dramatis Personæ</h3>
<p>Session types are about <em>channels</em>, which are like <em>tin can telephones</em>, in that you can use your tin to whisper sweet little nothings to every friend who has a tin connected to yours. I know, my love for tin can telephones is betraying my age a little—I’m an old Victorian lady.</p>
<figure>
<a href="https://books.google.com/books?id=IB8_AAAAYAAJ&amp;dq=Ebenezer+Cobham+Brewer,+Fran%C3%A7ois+Napol%C3%A9on+Marie+Moigno,+Henri+de+Parville&amp;pg=PA227&amp;redir_esc=y#v=onepage&amp;q&amp;f=false"><img src="/assets/images/tin-can-telephone.png" alt="Two victorian ladies hold a tin can telephone pulled taut between them." /></a>
<figcaption>
“You simply must try my milk puddings, Ada”, Briar whispers into the telephone.
</figcaption>
</figure>
<dl>
<dt><em>Ada</em></dt>
<dd>A Victorian Lady.
</dd>
<dt><em>Briar</em></dt>
<dd>Ada’s Lady Friend.
</dd>
<dt><em>The Tin Labelled A</em></dt>
<dd>A Tin Held by Ada.
</dd>
<dt><em>The Tin Labelled B</em></dt>
<dd>A Tin Held by Briar.
</dd>
<dt><em>The Piece of Twine</em></dt>
<dd>A Piece of Twine Connecting Tin A and B.
</dd>
</dl>
<p>In the vernacular of session types, the tin cans are referred to as <em>channel endpoints</em> or simply <em>endpoints</em>, and the collection of all tin cans held together by the twine is referred to as a <em>channel</em>. A series of messages whispered back and forth over a single channel is referred to as a <em>session</em>.</p>
<p>Most of the literature on session types considers only the classic scenario, in which we connect exactly two tin cans to form a channel—this is referred to as <em>binary session types</em>. Yet if we wanted to, we could make a telephone with any number of tin cans—this is referred to as <em>multiparty session types</em>.</p>
<p>In this blog post, we I’ll focus on <em>binary session types</em></p>
<h3 id="session-types-at-a-glance">Session Types <em>at a Glance</em></h3>
<p>Let’s imagine for a moment that Ada were to take Briar up on her offer, and ask her to sample her famous milk puddings. Briar, a proper lady, only offers her milk puddings to those who make a <em>sufficiently polite</em> request—Ada must be polite and say “please”, but she must not overuse it, lest she comes off as begging!</p>
<p>We <a name="Ada-and-Briar"></a>encode the interaction between Ada and Briar using <em>session types</em> in Haskell:</p>
<ul>
<li>Ada’s requests are represented using the <code>Request</code> datatype, which allows us to prefix a request for pudding with any number of uses of <code>Please</code>.</li>
<li>Briar’s response is represented using the <code>Response</code> datatype, in which she can either grant permission, in which case Briar sends an <code>Allow</code> with a sample of pudding attached, or refuse Ada’s request, in which case she sends a <code>Deny</code> with a reason.</li>
</ul>
<p>The functions <code>ada</code> and <code>briar</code> represent Ada and Briar—these functions each receive an endpoint for the shared channel, and communicate along the lines of our story—Ada sends a request, Briar evaluates her politeness and responds with either pudding or a refusal, and finally Ada evaluates Briars response, and expresses her emotions accordingly:</p>
<div class="sourceCode" id="cb1"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb1-1"><a href="#cb1-1" aria-hidden="true" tabindex="-1"></a><span class="kw">data</span> <span class="dt">Request</span></span>
<span id="cb1-2"><a href="#cb1-2" aria-hidden="true" tabindex="-1"></a>  <span class="ot">=</span> <span class="dt">Please</span> <span class="dt">Request</span></span>
<span id="cb1-3"><a href="#cb1-3" aria-hidden="true" tabindex="-1"></a>  <span class="op">|</span> <span class="dt">MayIHaveSomePudding</span></span>
<span id="cb1-4"><a href="#cb1-4" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb1-5"><a href="#cb1-5" aria-hidden="true" tabindex="-1"></a><span class="kw">data</span> <span class="dt">Response</span></span>
<span id="cb1-6"><a href="#cb1-6" aria-hidden="true" tabindex="-1"></a>  <span class="ot">=</span> <span class="dt">Allow</span> <span class="dt">Pudding</span></span>
<span id="cb1-7"><a href="#cb1-7" aria-hidden="true" tabindex="-1"></a>  <span class="op">|</span> <span class="dt">Deny</span> <span class="dt">String</span></span>
<span id="cb1-8"><a href="#cb1-8" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb1-9"><a href="#cb1-9" aria-hidden="true" tabindex="-1"></a><span class="ot">ada ::</span> <span class="dt">Send</span> <span class="dt">Request</span> (<span class="dt">Recv</span> <span class="dt">Response</span> <span class="dt">End</span>) <span class="ot">-&gt;</span> <span class="dt">IO</span> ()</span>
<span id="cb1-10"><a href="#cb1-10" aria-hidden="true" tabindex="-1"></a>ada chan <span class="ot">=</span> <span class="kw">do</span></span>
<span id="cb1-11"><a href="#cb1-11" aria-hidden="true" tabindex="-1"></a>  chan&#39; <span class="ot">&lt;-</span> send (<span class="dt">Please</span> <span class="dt">MayIHaveSomePudding</span>) chan</span>
<span id="cb1-12"><a href="#cb1-12" aria-hidden="true" tabindex="-1"></a>  (resp, chan&#39;&#39;) <span class="ot">&lt;-</span> recv chan&#39;</span>
<span id="cb1-13"><a href="#cb1-13" aria-hidden="true" tabindex="-1"></a>  <span class="kw">case</span> resp <span class="kw">of</span></span>
<span id="cb1-14"><a href="#cb1-14" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Allow</span> pudding <span class="ot">-&gt;</span> <span class="fu">putStrLn</span> <span class="st">&quot;I’m so happy!&quot;</span></span>
<span id="cb1-15"><a href="#cb1-15" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Deny</span> reason <span class="ot">-&gt;</span> <span class="fu">putStrLn</span> <span class="st">&quot;Woe is me!&quot;</span></span>
<span id="cb1-16"><a href="#cb1-16" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb1-17"><a href="#cb1-17" aria-hidden="true" tabindex="-1"></a><span class="ot">briar ::</span> <span class="dt">Recv</span> <span class="dt">Request</span> (<span class="dt">Send</span> <span class="dt">Response</span> <span class="dt">End</span>) <span class="ot">-&gt;</span> <span class="dt">IO</span> ()</span>
<span id="cb1-18"><a href="#cb1-18" aria-hidden="true" tabindex="-1"></a>briar chan <span class="ot">=</span> <span class="kw">do</span></span>
<span id="cb1-19"><a href="#cb1-19" aria-hidden="true" tabindex="-1"></a>  (req, chan&#39;) <span class="ot">&lt;-</span> recv chan</span>
<span id="cb1-20"><a href="#cb1-20" aria-hidden="true" tabindex="-1"></a>  <span class="kw">let</span> resp <span class="ot">=</span> <span class="kw">case</span> req <span class="kw">of</span></span>
<span id="cb1-21"><a href="#cb1-21" aria-hidden="true" tabindex="-1"></a>    <span class="dt">MayIHaveSomePudding</span> <span class="ot">-&gt;</span> <span class="dt">Deny</span> <span class="st">&quot;Such rudeness!&quot;</span></span>
<span id="cb1-22"><a href="#cb1-22" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Please</span> <span class="dt">MayIHaveSomePudding</span> <span class="ot">-&gt;</span> <span class="dt">Allow</span> myPudding</span>
<span id="cb1-23"><a href="#cb1-23" aria-hidden="true" tabindex="-1"></a>    <span class="dt">Please</span> (<span class="dt">Please</span> _) <span class="ot">-&gt;</span> <span class="dt">Deny</span> <span class="st">&quot;Such beggary!&quot;</span></span>
<span id="cb1-24"><a href="#cb1-24" aria-hidden="true" tabindex="-1"></a>  chan&#39;&#39; <span class="ot">&lt;-</span> send resp chan&#39;</span></code></pre></div>
<p>The example illustrates a crucial notions for session types:</p>
<p>Firstly, session types are <em>communication protocols.</em> If you glance at the types of the endpoints, you see that they represent the communication protocol from each participants perspective. For instance, Ada’s endpoint says she must send a request, receive a response, and then end the session.</p>
<p>Secondly, the types of the endpoints of a binary channel must be <em>dual</em>. When Ada’s endpoint says she must send, Briar’s endpoint says she must receive. For classical multiparty session types, the equivalent notion is called <em>coherence</em>, but the principle remains the same.</p>
<p>Finally, each endpoint must be used <em>exactly once</em> if we want to be sure to stick to the protocol. For instance, in the code above, each channel endpoint is only used once, and each send or receive returns a new channel on which to continue the communication. If we didn’t, we would be able to write a <a name="cheeky-Ada"></a>cheeky variant of Ada, who simply tries any number of pleases until she gets that sweet, sweet pudding:</p>
<div class="sourceCode" id="cb2"><pre class="sourceCode haskell"><code class="sourceCode haskell"><span id="cb2-1"><a href="#cb2-1" aria-hidden="true" tabindex="-1"></a><span class="ot">ada ::</span> <span class="dt">Send</span> <span class="dt">Request</span> (<span class="dt">Recv</span> <span class="dt">Response</span> <span class="dt">End</span>) <span class="ot">-&gt;</span> <span class="dt">IO</span> ()</span>
<span id="cb2-2"><a href="#cb2-2" aria-hidden="true" tabindex="-1"></a>ada chan <span class="ot">=</span> tryAll <span class="dt">MayIHaveSomePudding</span> chan</span>
<span id="cb2-3"><a href="#cb2-3" aria-hidden="true" tabindex="-1"></a>  <span class="kw">where</span></span>
<span id="cb2-4"><a href="#cb2-4" aria-hidden="true" tabindex="-1"></a>    tryAll req chan <span class="ot">=</span> <span class="kw">do</span></span>
<span id="cb2-5"><a href="#cb2-5" aria-hidden="true" tabindex="-1"></a>      chan&#39; <span class="ot">&lt;-</span> send req chan</span>
<span id="cb2-6"><a href="#cb2-6" aria-hidden="true" tabindex="-1"></a>      (resp, chan&#39;&#39;) <span class="ot">&lt;-</span> recv chan&#39;</span>
<span id="cb2-7"><a href="#cb2-7" aria-hidden="true" tabindex="-1"></a>      <span class="kw">case</span> resp <span class="kw">of</span></span>
<span id="cb2-8"><a href="#cb2-8" aria-hidden="true" tabindex="-1"></a>        <span class="dt">Allow</span> pudding <span class="ot">-&gt;</span> <span class="fu">putStrLn</span> <span class="st">&quot;I’m so happy!&quot;</span></span>
<span id="cb2-9"><a href="#cb2-9" aria-hidden="true" tabindex="-1"></a>        <span class="dt">Deny</span> reason <span class="ot">-&gt;</span> tryAll (<span class="dt">Please</span> req) chan</span></code></pre></div>
<p>But that’s not what the protocol says! Briar doesn’t have time for more than one request, so after the first one has run its course, Ada whispers her second request into the tin can, then waits forever, pining for a response from Briar which will never come!</p>
<h3 id="a-bit-of-a-roadmap">A Bit of a Roadmap</h3>
<p>Only a few short years after Ada and Briar enjoyed sweet milk puddings, a man by the name of <em>Alonzo Church</em> was born in Washington, D.C., in the United States. Three decades later, in the 1930s, Alonzo developed <a href="https://www.jstor.org/stable/1968337">the λ-calculus</a>, a foundational calculus which studies computation using <em>functions</em>. To this day, the λ-calculus underpins most theories of functional programming languages. Talk about influential!</p>
<p>Only a few short years after Alonzo developed the λ-calculus, a man by the name of <em>Robin Milner</em> was born near Yealmpton, in England. Alonzo lived a long life, over nine decades! A few years before Alonzo’s death in the mid 1990s, Robin, together with Joachim Parrow and David Walker, developed the <a href="http://www.lfcs.inf.ed.ac.uk/reports/89/ECS-LFCS-89-85/">π-calculus</a>, a foundational calculus which studies concurrent computation by processes using <em>message-passing communication</em>. It wasn’t the first process calculus—it itself was heavily influenced by ideas dating back to the early 1980s—but it’s certainly one of the most influential!</p>
<p>We’ll start out by discussing the untyped λ-calculus. It’s a wonderful little language, and it’s <em>really</em> powerful. Unfortunately, it has all sorts of programs that do all sorts of bad things, like loop forever, so with all that power, it’s <em>really scary</em> too! We’ll then discuss the idea of taming all that scary power using types, to try and get only well-behaved programs, and the challenges of taming it without taking all the <em>oomph</em> out.</p>
<p>Then, we’ll switch to discussing the π-calculus. It’s a wonderful little language, even if it’s twice as big as the λ-calculus—with <em>six</em> constructs instead of <em>three</em>! It’s even more powerful than the λ-calculus—it can express all sorts of concurrent behaviours that the λ-calculus has no hope of expressing. Unfortunately, it’s scarier as well—there’s way more things that can go wrong! Again, we’ll turn our attention to taming all that scary power using types, and the problems of <em>oomph</em>’lessness that comes with it.</p>
<p>Finally, we’ll talk about having the best of both worlds, in a concurrent λ-calculus, which is sorta what you get when you smash the λ-calculus and the π-calculus together at high speeds! The concurrent λ-calculus has the best of both worlds: higher-order functions and concurrency with message-passing communication!</p>
<h3 id="the-λ-calculus-so-powerful-so-scary">The λ-calculus! <em>(So powerful, so scary…)</em></h3>
<p>The untyped λ-calculus celebrated its 89th birthday last November, so to say that it’s been around for a while undersells it a bit. It’s a pretty small system—it has only three things—there’s variables, λ-abstractions to make functions, and function applications to get rid of ’em:</p>
$$
<p>$$</p>
<p>There’s only one computation rule—if a function $x.M$ meets its argument $N$ we replace all occurrences of $x$ in the function body $M$ with the argument $N$.</p>
<div class="mathpar">
$
<p>$</p>
</div>
<p>That’s not all, though, since we also need to let our calculus know that it’s okay to reduce under a function application. For this, we <em>could</em> just write out the following rules:</p>
<div class="mathpar">
$
$ $
<p>$</p>
</div>
<p>However, things tends to compose a little better if you use a little trick called <em>evaluation contexts</em>. We’ll see an example of <em>how</em> evaluation contexts compose better later. Anyway, you write down all the partial terms under which it’s okay to normalise, and then write a single rule… Read $E[M]$ as “replace the single $$ in $E$ with $M$”:</p>
<div class="mathpar">
$
$ $
<p>$</p>
</div>
<p>When we choose what to put in our evaluation contexts, we determined where to allow and disallow reduction. For instance, here we’re saying “You’re allowed to reduce function calls and their arguments… but don’t you dare touch the function body before we’re actually calling it!” This is called an <em>evaluation strategy</em>, and the one we’re using here is called <em>call-by-name</em>. Usually, call-by-name is pretty terrible in terms of efficiency. Imagine you have a pretty expensive computation, and the result of that computation is used twenty times… call-by-name is likely to do the whole computation twenty times! In practice, you’ll want to use call-by-value or call-by-need, but those complicate things, so we’re not using them here!</p>
<p>Where were we? Oh, yes, λ-calculus, powerful, scary… Right! The λ-calculus is <em>very</em> powerful—<em>some stuff about it being a “universal model of computation”</em>—but that power comes at the cost of also being able to express quite a lot of scary programs that do bad stuff.</p>
<p>For instance, the λ-calculus comes with general recursion out of the box, via the $Y$ combinator! We’ll see an example of using the $Y$ combinator below, but essentially, $Y;f$ represents an infinite series of applications of $f$:</p>
$$
<p>$$</p>
<p>That’s good—as programmers, we like recursion! Really simplifies your programs, not having to write out the case for every possible input!</p>
<p>However, if you pass $Y$ the identity function, you’ll get $Ω$—the program which runs forever, but never gets anything done! Watch it reduce to right back to itself in a single step:</p>
$$
<p>$$</p>
<p>That’s scary, I’d prefer not to have that! Programs which run forever, but never do a single thing—or worse, programs which are doing things the whole time, but never produce any outputs!</p>
<p>Most functional languages don’t just implement the core λ-calculus, but rather extend the λ-calculus with various constructs—numbers, addition, multiplication, pairs, sums, <em>etc.</em> <em>Technically speaking</em>, these can all be encoded using just functions—see, <em>e.g.</em>, Church encodings—but it tends to be <em>a lot</em> more practical and faster to use, <em>e.g.</em>, machine numbers.</p>
<p>For example, we can extend the untyped λ-calculus with <a name="peano"></a>Peano numbers. First, we extend the term language with the number <em>zero</em>, written $$, the successor, written $$, and a pattern matching construct for numbers, written $;L;;{;;{x}}$:</p>
$$
<p>$$</p>
<p>Then, we extend the reduction rules with two reduction rules for pattern matches on numbers—depending on whether the number is zero or a successor:</p>
<div class="mathpar">
<p>$;;;{;;{x}}  M$</p>
<p>$;;{L};;{;;{x}}  N{L/x}$</p>
</div>
<p>And we shouldn’t forget to extend our evaluation contexts:</p>
$$
<p>$$</p>
<p>We can now define addition on Peano numbers in our calculus! Ideally, we’d write something like the following, familiar definition for addition:</p>
$$
<p>$$</p>
<p>Our core language doesn’t support recursive or pattern matching definitions, so we’ll have to elaborate the above definition into something less familiar, which uses the $Y$ combinator and the pattern matching construct. It’s a bit more verbose, but it’s addition nonetheless:</p>
$$
<p>$$</p>
<p>Woe is us! We have <em>another</em> kind of problem! We now have to worry about programs like $;(x.x);$. What does that even mean?! According to our semantics, it means exactly that, since it doesn’t reduce any further… It’s stuck on the pattern match on $x.x$, since there’s no case for functions.</p>
<p>Problems like these are less obvious when using, <em>e.g.</em>, Church encodings, since everything is just functions. For instance, if we use Church-encoded Peano numbers to compute $<em>{ch} ; (x.x) ; </em>{ch}$, and convert the result to our builtin Peano numbers, we find that adding the identity function to the number <em>zero</em> gives us <em>one</em>:</p>
$$
<p>$$</p>
$$
<p>$$</p>
<p>All in all, we’ve identified <em>two</em> main problems with using the untyped λ-calculus as a foundation for programming languages:</p>
<ol type="1">
<li>it has programs which loop forever but produce nothing; and</li>
<li>it has no way of making sure that data is used as intended.</li>
</ol>
<p>Specifically, for the second problem, we have the choice between programs which get stuck and programs which compute nonsense. If we want programs that misuse data to get stuck, we tag our data—by using, <em>e.g.</em>, a syntactically distinct $$ constructor—and check the types of our data at runtime. If we’re fine with the misuse of data, we encode everything as functions, and accept whatever results fall out of our encoding.</p>
<h3 id="taming-the-λ-calculus-with-types">Taming the λ-calculus with types…</h3>
<p>So, we’ve got a lot of scary stuff going on, stuff we really rather wouldn’t have, like programs which uselessly loop forever, and programs which try to add numbers to functions. What can we do?</p>
<p>One of the <em>simplest</em> solutions—horrible pun <em>absolutely</em> intended—is to use simple types. You see, in 1940, in an attempt to get rid of these unhelpful loops, Alonzo developed <a href="https://www.jstor.org/stable/2266170">the simply-typed λ-calculus</a>.</p>
<p>We start by formalising what we mean by <em>type</em>. Since all we’ve got is functions, all we need is a function type $A B$ and <em>some</em> base type—it doesn’t really matter what, so we’re gonna call it $$:</p>
$$
<p>$$</p>
<p>With types in hand, we write down some <em>typing rules</em>. The goal is that <em>if</em> we can construct a typing derivation for a term, that term will be well-behaved.</p>
<p>Terms are checked in the context of some typing environment, which we’ll refer to with the variable $$ or $$. Typing environments are just a bunch of typing assignments $x : A$. Why do we need typing environments? A variable in a program might stand for some base value, like an int, or it might stand for a function, and it’s kinda important to know ahead of time which one it is—you can’t apply an int, and you can’t add one to a function! We write $x : A $ to mean that “somewhere in $$ there’s the typing assignment $x : A$”.</p>
<p>We write $M : A$ to mean that “using the variables from the typing environment $$, the term $M$ has type $A$”—statements like these are called <em>typing judgements</em>. But we can’t just <em>claim</em> a some term has some time, we need to back it up with some actual proof! For that, we use <em>inference rules</em>, which is how most type systems are presented.</p>
<p>Inference rules are like a puzzle. They’re written as…</p>
$$
<p>$$</p>
<p>The puzzle you’re trying to solve is:</p>
<ul>
<li>Find a piece whose conclusion matches the thing you’re trying to prove;</li>
<li>Oh no! All those things had premises! You gotta find puzzle pieces whose conclusions match those as well now!</li>
<li>Keep going until there’s no more open premises! You got this!</li>
</ul>
<p>For the simply-typed λ-calculus, there are three inference rules, one for each term construct:</p>
<div class="mathpar">
$
$ $
$ $
<p>$</p>
</div>
<p>Oh no! All of these rules have some premises! Does that mean we’re gonna have to puzzle forever? Nope, all it means is that we are <em>immediately</em> complicating the puzzle analogy.</p>
<p>If you look at the first rule, the premise isn’t actually a typing judgement… It’s one of those thingies which checks whether or not $x$ has type $A$ in $$! There’s a whole separate kind of puzzle for those! One that’s usually left implied, because it’s relatively simple:</p>
<div class="mathpar">
$
$ $
<p>$</p>
</div>
<p>All those puzzle pieces say is “if you wanna know if $x : A$ is in $$… go through all the thing in $$ and check if one of ’em is $x : A$.” With those pieces made explicit, our puzzle will have a satisfying “no premise left unproven!” kind of feel to it… Though, mostly, folks just leave the proofs for $x : A $ implicit, since once you write out $$, they’re pretty obvious.</p>
<p>Anyway, back to our typing rules for the λ-calculus! In order of appearance:</p>
<ul>
<li>A variable $x$ has type $A$ if there’s an assignment in $$ that says so.</li>
<li>If we’ve got something of type $B$ which uses something of type $A$ from the typing environment, we can abstract over that something to create a function of type $A B$.</li>
<li>If we’ve got something of type $A B$ and something of type $A$, then we can apply the former to the latter to get something of type $B$.</li>
</ul>
<p>Guess what?! It works! All the programs you can type with these rules are super well-behaved and nice! Buuuut… there’s kinda a lot of programs that are really nice and good, that you can’t type with these rules… Very, <em>very</em>, notably, you can’t type the $Y$ combinator. Oh no! We lost recursion!</p>
<p>Queue the history of type theory, trying to wrangle with this, trying to make this system more permissive while still keeping lots of the scary stuff out!</p>
<p>It’s, <em>uh</em>, pretty hard to get <em>extactly</em> the bad looping stuff out, so some folks are like “eh, we’ll keep the looping stuff, but still use types to get rid of all that ‘adding functions to numbers’ nonsense”, whereas other folks are all hardcore and decide that “no it has to be terminating all the way even if it becomes pretty hard to use!”</p>
<h3 id="a-detour-into-linearity">A detour into linearity!</h3>
<p>Let’s briefly talk about another type system for the λ-calculus—but only because it’ll turn out to be highly relevant to session types, I haven’t forgotten what I promised to write about! Let’s talk about <a href="https://homepages.inf.ed.ac.uk/wadler/papers/lineartaste/lineartaste-revised.pdf">the linear λ-calculus</a>.</p>
<p>In its most minimal form, the linear λ-calculus demands that every variable is used <em>exactly once</em>. This’ll end up being a <em>very important</em> restriction for our session-typed calculus. Remember that cheeky implementation for Ada, which kept sending new and new requests for milk pudding, even though the protocol <em>clearly</em> stated she could only send one request? That’s where the <em>used exactly once</em> restriction comes in.</p>
<p>Okay, so how are we going to enforce this in the type system? When you check a function application, you have to decide which parts of the typing environment are gonna be used in the function, and which parts in the argument. By the time you’ve made it all the way down to a variable, the typing environment is supposed to be empty save for the variable you’re checking. Everything else must’ve already been split off for usage elsewhere.</p>
<p>Also, we now use this cute little lollipop instead of the function arrow:</p>
$$
<p>$$</p>
<div class="mathpar">
$
$ $
<p>$</p>
$
<p>$</p>
</div>
<p>In order of appearance:</p>
<ul>
<li>A variable $x$ has type $A$ if the typing environment <em>only</em> contains $x : A$.</li>
<li>If we’ve got something of type $B$ which uses something of type $A$ from the typing environment, we can abstract over that something to create a function of type $A B$. <em>(It’s the same as before!)</em></li>
<li>If we’ve got something of type $A B$ which uses some chunk of the typing environment called $$, and something of type $A$ which uses the rest of the typing environment called $$, then we can apply the former to the latter to get something of type $B$ which uses both $$ and $$.</li>
</ul>
<p>Notice that $A B$ being a linear function isn’t something that follows from the rules for abstraction and application… it’s something to do with the structure of <em>all rules</em>.</p>
<p>As a type system, this is <em>highly restrictive</em>. Essentially, what we’re left with is a calculus of permutations. Think of lists… if you’re writing a function from lists to lists, but you <em>have to</em> use every element in the list exactly once, what kinds of programs can you write? Permutations. That’s it.</p>
<h3 id="the-π-calculus-is-even-scarier">The π-calculus! <em>(Is even scarier…)</em></h3>
<p>Oof, that was a bit of a detour, wasn’t it? Wanna talk about <em>session types</em>, the thing that I promised I’d talk about? Okay, let’s do it! The π-calculus is pretty young—it didn’t show up until 1992, though it’s heavily influenced by ideas dating back to the 1980s. Unlike with the λ-calculus, there’s not really a <em>canonical</em> π-calculus that everyone agrees on, so the one I’m presenting here is just kinda the version that I felt like presenting.</p>
<p>It’s also pretty big! It’s got twice as many <em>things</em> in it as the λ-calculus. Instead of functions, we’re talking about processes, which are built using <em>six</em> different constructors:</p>
$$
<p>$$</p>
<p>In order of appearance:</p>
<ul>
<li>We’ve got ν-binders, written $(x)P$, which creates a new channel $x$, which can be used in $P$. (That ν is the Greek letter nu, which sure sounds a lot like “new”. It’s, <em>like</em>, the only well-chosen Greek letter we use in programming language theory.)</li>
<li>We’ve got parallel composition, written $$, to let you know that two processes are running in parallel.</li>
<li>We’ve got nil, written $0$, the process which is done.</li>
<li>We’ve got <em>send</em>, written $x y .P$, which sends some $y$ on $x$, and then continues as $P$.</li>
<li>We’ve got <em>receive</em>, written $x ( y ).P$, which receives some value on $x$, names it $y$, and then continues as $P$.</li>
<li>We’ve got replication, written $! P$, which represents a process $P$ which is replicated an arbitrary number of times.</li>
</ul>
<p>Replication isn’t truly <em>essential</em> to the π-calculus, it’s just that we can’t do any sort of <em>infinite</em> behaviour with just sending and receiving, so we have to add it explicitly. Other solutions, like adding recursive definitions, work as well.</p>
<p>There’s only one computation rule—if we’ve got a send and a receive in parallel, we perform the communication, and replace all instances of the name bound by the receive instruction by the actual value sent:</p>
<div class="mathpar">
$
<p>$</p>
</div>
<p>Plus our usual trick to let us reduce under parallel compositions and ν-binders:</p>
<div class="mathpar">
$
$ $
<p>$</p>
</div>
<p>However, these rules in and of themselves are not enough. You see, a parallel composition $P Q$ isn’t intended to be <em>ordered</em>—I mean, if you’ve got two processes in parallel, does it make sense to say that one of them is “to the left of” the other?—but we haven’t told the reduction semantics about that. That means that with the rules we’ve given so far, we cannot reduce the following:</p>
<p>$$ (x)(x(z).{Q}x.{P}) $$</p>
<p>Why not? The send and the receive are in the wrong order—our computation rule requires that the send is <em>to the left of</em> the receive, so we can’t apply it:</p>
<p>$$ x.{P}x(z).{Q}  {P}{y/z} $$</p>
<p>One solution is to tell the reduction semantics that the order of processes doesn’t matter—along with a few other things:</p>
$$
<p>$$</p>
<p>In order of appearance:</p>
<ul>
<li>Parallel composition is <em>commutative</em> and <em>associative</em>, <em>i.e.</em> the order of parallel processes doesn’t matter.</li>
<li>We can remove (and add) processes which are done.</li>
<li>We can remove (and add) ν-binders which aren’t used.</li>
<li>The order of ν-binders doesn’t matter.</li>
<li>We can swap ν-binders and parallel compositions as long as we don’t accidentally move an endpoint out of the scope of its binder.</li>
<li>Replicated processes can be, <em>well</em>, replicated. We kinda forgot to add this at first, so we didn’t have any infinite behaviour… but now we do!</li>
</ul>
<p>Now that we have this equivalence of processes—usually called <em>structural congruence</em>—we can embed it in the reduction relation:</p>
$$
<p>$$</p>
<p>The reason we’re embedding it this way, with a reduction step sandwiched between two equivalence, is because the equivalence relation isn’t super well-behaved—there’s plenty of infinite chains of rewrite rules, <em>e.g.</em>, imagine swapping $P Q$ back and forth forever, or duplicating $! P$ forever—and we’d prefer not to have any infinite chains of reductions. Embedding it this way forces there to be at least one <em>real</em> computation step in each reduction step, because the only way to construct a reduction is to start with a computation.</p>
<p>If you thought the λ-calculus had problems, have I got news for you. There’s all the old problems we had with the lambda calculus. We’ve got processes that reduce forever without doing anything:</p>
$$
<p>$$</p>
<p>Ah! One process which just <em>keeps</em> sending the value $y$ to another process, which throws it away immediately, just sending and throwing away, <em>forever</em>. Isn’t that the kind of programs we all like to write?</p>
<p>Plus, if we add numbers, we could try to send over the number 5, foolishly assuming it’s a channel.</p>
<p>But <em>what fun</em>! There’s new problems as well! If we’ve got two <em>pairs</em> of processes, both of which are trying to communicate over $x$ at the same time, then reduction is not longer <em>deterministic</em>—it’s anyone’s guess which message will end up where! Yaaay, it’s <em>race conditions</em>:</p>
$$
<p>$$</p>
<p>Another <em>fun</em> thing we can do is write two processes which echo a message on $x$—they receive something and send it back… but the twist is, they’re both wanting to <em>receive</em> first! Ack, it’s a <em>deadlock</em>:</p>
$$
<p>$$</p>
<p>To be fair, it’s not surprising that race conditions and deadlocks show up in a foundational calculus for <em>concurrency</em>—it’d be weird if they didn’t. But that does mean that as programming languages people, we now have two new problems to worry about! To summarise, we’ve identifier <em>four</em> main problems with the untyped π-calculus as a foundation for programming languages:</p>
<ol type="1">
<li>it has programs which loop forever but produce nothing;</li>
<li>it has no way of making sure that data is used as intended;</li>
<li>it has <em>race conditions</em>; and</li>
<li>it has <em>deadlocks</em>.</li>
</ol>
<h3 id="taming-the-π-calculus-with-types">Taming the π-calculus with types…</h3>
<p>Oh dear, so many problems to solve. Where do we begin?</p>
<p>It may help to think a little deeper about the latter three problems. In a sense, we could see a deadlock as the consequence of us not using a channel as intended. After all, we probably intended for one party to be sending while the other was receiving. We could see a race condition in a similar light. We intended for the two pairs of processes to communicate in a <em>predictable</em> pattern, in pairs of two.</p>
<p>These are overly simplistic descriptions—sometimes we truly don’t care about the order of messages, and a bit of a race is fine. However, much like with the λ-calculus, we’re gonna try and cut all the bad stuff out first, no matter what cost, and then get to the business recovering what we lost.</p>
<p>Let’s have a look at <em>session types</em>, invented in the early 1990s by <a href="http://www.kurims.kyoto-u.ac.jp/~kyodo/kokyuroku/contents/pdf/0851-05.pdf">Kohei Honda</a>, and let’s focus first and foremost on one important property—<em>session fidelity</em>. Essentially, it means that we communicate over a channel as intended by its protocol—or <em>session type</em>.</p>
<p>Let’s start with the simplest system, where there’s only two things we can do with a channel—send and receive. That’ll be our language of session types—either we send on a channel, we receive from a channel, or we’re done:</p>
$$
<p>$$</p>
<p>A crucial notion in session types—as mentioned in the introduction—is <em>duality</em>, the idea that while I’m sending a message, you should be expecting to receive one, and vice versa. Duality is a function on session types. We write duality using an <em>overline</em>, so the dual of $S$ is $$:</p>
$$
<p>$$</p>
<p>Finally, before we get to the typing rules, we’re gonna make one tiny tweak to the syntax for processes. Before, our ν-binders introduced a <em>channel name</em>, which any process could then use to communicate on. Now, ν-binders introduce a channel by its two <em>channel endpoint names</em>. It’s gonna make the typing rules a <em>bunch</em> easier to write down:</p>
$$
<p>$$</p>
<p>We’ve also gotta propagate this changes through the structural congruence and the reduction rules. It’s pretty straightforward for most of the changes—just replace $(x)$ with $(x x’)$—but there’s one extra change we have to make to the computation rule, which is a bit of a shift in perspective. See, in our first iteration of the π-calculus, processes were connected by virtue of having access to the same <em>channel name</em>, and ν-binders were merely a convenience to let us hide channel names from the outside world. However, in our current version, processes use <em>unrelated</em> endpoint names, and it’s the ν-binder who connects them to form a channel. Or—if you will—each participant is just holding a tin can, and they can’t be used to communicate until they’re bound together with twine. That means that we’ll have to <em>require</em> that reduction takes place under a ν-binder:</p>
<p>$$ (x x’)(x.{P}x’(z).{Q})  (x x’)({P}{y/z}) $$</p>
<p>Okay, typing rules! One thing that’s very different from the λ-calculus is that the typing rules only check whether processes use channels correctly—the processes themselves don’t have types. There are six rules, corresponding to the six process constructs:</p>
<div class="mathpar">
$
$ $
$ $
$ $
<p>$</p>
$
$ $
<p>$</p>
</div>
<p>In order of appearance:</p>
<ul>
<li>If we’ve got a process $P$ which uses two endpoints $x$ and $x’$ at dual session types, then we can connect them to form a channel, written $(x x’)P$.</li>
<li>If we’ve got two processes $P$ and $Q$, which use channels according to the session types in $$ and $$, then we can put those processes in parallel, written $P Q$, and the resulting process will use channels according to the session types in $, $, where $$ and $$ <em>must be disjoint</em>.</li>
<li>The terminated process is done—it doesn’t use any channels.</li>
<li>If we’ve got a channel $x$ of type $!A.B$ and some $y$ of type $A$, then we can send $y$ over $x$. We lose access to $y$—we’ve sent it away, after all—and the channel $x$ continues as a channel of type $B$.</li>
<li>If we’ve got a channel $x$ of type $?A.B$, then we can receive something of type $A$ over $x$—call it $y$—after which the channel $x$ continues as a channel of type $B$.</li>
<li>Finally, if we’ve got a channel which is done, we can forget about it.</li>
</ul>
<p>This type system is <em>linear</em>, much like the linear λ-calculus we saw earlier. Told you it’d be relevant! Anyway, it’s not linear in quite the same way, since channels can be used <em>multiple times</em>. However, each step in the protocol has to be executed <em>exactly once</em>!</p>
<p>So, did it work? Are we safe from the bad programs? Yes and no. Which, <em>uh</em>, kinda just means no? But there’s <em>some</em> bad programs we got rid of! There are no longer programs which misuse data, since everything strictly follows protocols. Since every channel has <em>exactly two</em> processes communicating over it, we no longer have any race conditions. Furthermore, because those two processes must act <em>dually</em> on the channel, we no longer have any deadlocks <em>within a single session</em>—that is to say, as long as each two processes only share <em>one</em> means of communication, we don’t have any deadlocks. Unfortunately, it’s quite easy to write a program which interleaves <em>two</em> sessions and deadlocks:</p>
$$
<p>$$</p>
<p>We also don’t have looping programs anymore, but, <em>uh</em>, that’s mostly because we removed replication, so… win?</p>
<p>How do we get rid of those last few deadlocks? The ones caused by having multiple open lines of communication with another process? There’s several different ways to do this, and they all have their advantages and disadvantages:</p>
<p>The first option, used in, <em>e.g.</em>, the logic-inspired session type systems by <a href="https://widgets.figshare.com/articles/6606974/embed">Luís Caires and Frank Pfenning</a> and <a href="https://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-sessions/propositions-as-sessions-jfp.pdf">Philip Wadler</a>, is to just say “The problem happens when you’ve got multiple open lines of communication with another process? Well, have you considered just not doing that?” Essentially, these type systems require that the <em>communication graph is acyclic</em>—what that means is that, if you drew all the participants, and then drew lines between each two participants who share a channel, you wouldn’t draw any cycles. This works! Can’t share multiple channels if you don’t share multiple channels, am I right? But it’s a <em>wee bit</em> restrictive… Got some homework about <a href="https://en.wikipedia.org/wiki/Dining_philosophers_problem">forks and hungry, hungry philosophers</a> you need to do? Nope. Wanna write a neat cyclic scheduler? Not for you. However, it has some <em>nice</em> aspects too—it composes! Got two deadlock-free programs? Well, you can put ’em together, and it’s gonna be a deadlock-free program! If we updated our typing rules, they’d look a little like this:</p>
<div class="mathpar">
$
$ $
$ $
<p>$</p>
$
$ $
<p>$</p>
</div>
<p>We’ve glued the ν-binder and the parallel composition together in a single operation which makes sure that one endpoint goes one way and the other the other.</p>
<p>The second option, developed by <a href="https://doi.org/10.1016/S0890-5401(02)93171-8">Naoki Kobayashi</a>, is to just do a whole-program check for deadlocks. Take out a piece of paper, and draw a blob for every dual pair of send and receive actions in your program. For every action, if it <em>has to</em> happen before another action, draw an arrow between their blobs. Finally, check to see if there’s any <em>directed</em> cycles—for each blob, see if you can follow the arrows and end up back at the same blob. We can do this <em>formally</em> by adding some little blobs to our session types—since each session type connective corresponds to an action—and requiring a particular order on the little blobs. For reference, little blobs are more commonly known as <em>priorities</em>. If we updated our typing rules, they’d look a little like this… first, we add little blobs to our session types. Duality <em>preserves</em> priorities:</p>
<div class="mathpar">
$
$ $
<p>$</p>
</div>
<p>We also define a function, $$, to nab the topmost priority from a session type. If we apply $$ to a $$ we mean to get the <em>smallest</em> priority of all types in $$—actions with smaller priorities happen earlier, so we essentially wanna know when the <em>first</em> action in $$ is gonna happen.</p>
$$
<p>$$</p>
<p>Then we change the typing rules:</p>
<div class="mathpar">
$
$ $
$ $
$ $
<p>$</p>
$
<p>$</p>
$
<p>$</p>
</div>
<p>We’re enforcing two things here:</p>
<ol type="1">
<li>If we write $x(y).P$, then the action $x(y)$ must happen before <em>everything else in</em> $P$. Similarly for $x.P$.</li>
<li>If we connect two endpoints, $(x x’)$, then the dual actions on those endpoints must each happen <em>at the same time</em>.</li>
</ol>
<p>There’s a really nice recent example of a session type system which uses this technique by <a href="http://www.dcs.gla.ac.uk/~ornela/assetsations/DG18-Extended.pdf">Ornela Dardha and Simon Gay</a>. The upside of this technique is that you can have all sorts of neat cyclic communication graphs, and still rest assured knowing that they don’t do anything scary. The downside is that it’s a <em>whole-program</em> check, meaning that if you’ve got two deadlock-free programs, and you put ’em together, you have to check again, to see that you didn’t introduce any deadlocks.</p>
<p>Anyway, we’ve now got a <em>mostly</em> safe foundation for session types! You don’t see type systems <em>this</em> simple touted in papers much—or <em>at all</em>, as far as I’m aware. The reason is probably that they’re kinda <em>too simple</em>. Just like with the <em>purely</em> linear λ-calculus, there’s not much you can actually compute with these programs, and you have to put in a little bit of work before you can actually get to a point where you get back enough expressivity to be taken seriously as a programming language. However, I thought it would be illustrative to discuss the <em>simplest possible</em> type systems.</p>
<p>Once we’ve got this foundation, we can get to work extending it! With a little bit of effort we could add branching, replication, recursion, several forms of shared state, polymorphism, higher-order processes, <em>etc.</em> However, figuring out how all this stuff works in the context of the π-calculus seems like a bit of a waste, especially when we already know how they work within the context of the λ-calculus. Plus, doesn’t that “adding higher-order processes” thing sound suspiciously like adding higher-order functions?</p>
<h3 id="concurrent-λ-calculus-λ-and-π-together-forever">Concurrent λ-calculus <em>(λ and π, together forever)</em></h3>
<p>Okay, this will be the <em>final</em> computational model I’m introducing in this by now rather long blog post, I promise! The point of concurrent λ-calculus, in short, is this: the π-calculus is great for modelling concurrency, but it’s a rather, <em>uh</em>, unpleasant language to actually write programs in, and instead of figuring out how to make it a nice language, why not just smash it together with a thing that we already know and love!</p>
<p>So, here’s the plan: we’re gonna start with the λ-calculus as a model of <em>sequential</em> computation, and then we’re gonna add a smattering of π-calculus processes on top as a model of sequential computations running <em>concurrently</em>.</p>
<p>First off, we’re going to take our λ-calculus terms, and extend then with some constants $K$, which will be our concurrency primitives. We’re also going to add <em>units</em> and <em>pairs</em>—we’ll need them in a bit! We write $()$ for the unit value and $ ; () = M ;  ; N$ for pattern matching on units. We write $(M, N)$ for the pair of $M$ and $N$ and $ ; (x, y) = M ;  ; N$ for pattern matching on pairs.</p>
<p>Second, we’re going to wrap those terms up in π-calculus processes—we’ll have ν-binders and parallel compositions, and threads which run terms. There’s two kinds of threads—<em>main</em> threads, written $M$, and <em>child</em> threads, written $M$. Why? Well, we expect functional programs to produce a value, but processes just send and receive things, and then stop. Programs in our concurrent λ-calculus are going to have <em>exactly one</em> main thread, which is going to compute a value. Child threads will send and receive, but eventually compute the unit value:</p>
<div class="mathpar">
$
$ $
<p>$</p>
$
<p>$ $ ; ::= $</p>
</div>
<p>So what are the semantics of our calculus going to be?</p>
<p>First off, we just keep the reduction rules for our λ-calculus—and we’re just gonna sneak those rules for units and pairs on in there while you’re distracted. We’re gonna call this reduction arrow $_M$, to distinguish it from the reduction on processes:</p>
<div class="mathpar">
$
<p>$</p>
$
<p>$</p>
$
<p>$</p>
</div>
<p>Let’s not forget our usual “evaluation context” shenanigans:</p>
<div class="mathpar">
$
<p>$</p>
$
<p>$</p>
</div>
<p>We’re also going to just copy over the structural congruence from the π-calculus, best as we can—the, <em>uh</em>, terminated process has disappeared, and in it’s place we’ve now got child threads which are done, <em>i.e.</em>, $()$, so it’s a <em>little</em> different, but otherwise, pretty much the same:</p>
$$
<p>$$</p>
<p>Then we copy over our rules from the π-calculus… but what’s this! We don’t have send and receive in the process language anymore? They’re all awkwardly wedged into the term language now… So terms reduce, and at some point, they get stuck on a $$ or $$ that they don’t know how to deal with, and then the π-calculus rules have to take over. Our first instinct might be to write something like…</p>
<div class="mathpar">
$
<p>$</p>
</div>
<p>…which sure looks a lot like our π-calculus rule. Unfortunately, it only captures top-level $$ or $$ operations… and usually, we’ll want to bind and actually use the value we receive! Plus, we actually lose access to the channel this way… our intention is that each endpoint can only be used once, and that $$ and $$ return the endpoint for the next step in the session along with any potential result… Now, we <em>could</em> discard the old channel, create a new one, and return it… and in an implementation, this may well be what you do… but here? Why would we? We’ve got a perfectly good channel, with two endpoints we know aren’t going to be used anyway… so why not just return the same channel?</p>
<div class="mathpar">
$
<p>$</p>
</div>
<p>This is a great example of why evaluation contexts compose better! Try and write this rule without them! Oh, yeah, almost forgot! We’ve still gotta add evaluation contexts for the process language, plus that thing where we tell reduction that it’s okay to rewrite using our structural congruence, and a new rule where we tell π-calculus reduction that it’s okay for them to use the λ-calculus rules as well:</p>
<div class="mathpar">
$
<p>$</p>
$
$ $
$ $
<p>$</p>
</div>
<p>We’re <em>almost done!</em> You might’ve noticed that we’ve got $$ and $$ in our term language <em>as well as</em> ν-binders and parallel compositions in our process language. That’s seems kinda redundant, doesn’t it? We’d like to think of the <em>terms</em> as the programs that we actually write, and of the <em>processes</em> as modelling the configuration of threads and shared channels created while <em>running those programs</em>. So the $$ and $$ functions really just mean “create a ν-binder” and “create a parallel composition” on the process level! So we’ve got to add two more rules:</p>
<div class="mathpar">
$
$ $
<p>$</p>
</div>
<p>Oof, we’ve done it! We’ve got the whole reduction system!</p>
<h3 id="two-victorian-ladies-more-formal-somehow">Two Victorian Ladies <em>(More Formal, Somehow?)</em></h3>
<p>Our formal concurrent λ-calculus is getting pretty close to being able to encode the interaction between <a href="#Ada-and-Briar">Ada and Briar</a>! Remember that, like a billion words ago? There’s two problems left, if we want to encode our example:</p>
<ol type="1">
<li>Ada prints a string, but we don’t really have strings or, <em>uh</em>, the ability to print strings.</li>
<li>Ada and Briar send values of data types back and forth, but we don’t have <em>data types</em>.</li>
</ol>
<p>For the first one, we’re just gonna take $$ and strings as primitives in our calculus, with no associated behaviour, and if our programs reduce to, <em>e.g.</em>, $;$ we’ll say that’s fine.</p>
<p>For the second one, <em>well</em>, we can make this work without having to add full-fledged data types to our language. See, the request data type… it essentially encodes the number of <em>pleases</em>, right? It’s kinda like <a href="#peano">Peano numbers</a>, where <code>MayIHaveSomePudding</code> is $$ and <code>Please</code> is the successor $$. Remember that from the first section? And the <code>Response</code>? Well, Ada doesn’t actually use the pudding <em>or</em> the reason, so that’s pretty much just a Boolean… and those should be relatively easy to add! First, we extend our terms:</p>
$$
<p>$$</p>
<p>Then, we extend the reduction rules with two reduction rules for if-statements—one for when it’s true, and one for when it’s false:</p>
<div class="mathpar">
<p>$;;;M;;N M$</p>
<p>$;;;M;;N N$</p>
</div>
<p>And we extend our evaluation contexts:</p>
$$
<p>$$</p>
<p>Great! <a name="formal-Ada-and-Briar"></a> Now we can encode our example!</p>
$$
<p>$$</p>
$$
<p>$$</p>
<p>And let’s put it all together in a single $$ process. We’ve not done this so far, since it was <em>hopefully</em> pretty clear how Ada and Briar were meant to share a channel, but if we want to actually evaluate our processes, we’ll have to create a channel to connect Ada and Briar:</p>
$$
<p>$$</p>
<p>Right, let’s see if our encoding does what we think it should do! I’m gonna spare no detail, so, <em>uh</em>, very long series of evaluation steps coming up.</p>
$$
<p>$$</p>
<p>Whew, so far so good! The $$ and $$ have done their jobs, and created a channel and a parallel composition! We’re right down to Ada and Briar now! Things are about to get messy! (We’ll sometimes write $_n$ when we’re doing multiple steps at a time, either because Ada and Briar are computing things in parallel, or because I didn’t feel like writing the whole thing out.)</p>
$$
<p>$$</p>
<p><em>Yes</em>, we’ve shown that our program is correct! It makes Ada happy! What more could you want?</p>
<h3 id="taming-the-concurrent-λ-calculus-with-types">Taming the concurrent λ-calculus with types…</h3>
<p>Types? Is it types? It <em>should be!</em> Just because our happy example works out, doesn’t mean the calculus as a whole is well-behaved. See, we can still encode <a name="formal-cheeky-Ada"></a> <a href="#cheeky-Ada">cheeky Ada</a>, who’ll do anything for that sweet, sweet pudding:</p>
$$
<p>$$</p>
<p>I’m not gonna write out the whole evaluation, like I did with the previous example, but you can verify for yourself that evaluation gets stuck after a single back-and-forth, with Briar being done with Ada’s cheek and reducing to $;()$, while Ada still wants to talk.</p>
<p>We’d like to rule out this sort of failing interaction <em>a priori</em>. Briar was <em>very clear</em> about her boundaries of only taking a <em>single</em> request for cake, so we should’ve never set her up with <em>cheeky</em> Ada. How are we gonna do this? <em>With types!</em></p>
<p>Developing the type system for the concurrent λ-calculus will be a very similar experience to developing its reduction semantics… we’re mostly just smashing stuff from the λ-calculus and the π-calculus together, and seeing what falls out.</p>
<p>To start off with, we’re gonna copy over the whole type system for the linear λ-calculus, adding the rules for units and pairs as needed. Similar to how we write $A B$ in place of $A B$ for <em>linear</em> functions, we write $A B$ in place of $A B$ for <em>linear</em> pairs:</p>
$$
<p>$$</p>
<div class="mathpar">
$
<p>$</p>
$
$ $
<p>$</p>
$
$ $
<p>$</p>
$
$ $
<p>$</p>
</div>
<p>First come the rules for variables and functions, which we’ve seen before:</p>
<ul>
<li>A variable $x$ has type $A$ if the typing environment <em>only</em> contains $x : A$. <em>(If we’d allow other variables to appear, those would be discarded, since the variable $x$ doesn’t use them, and hence wouldn’t be linear!)</em></li>
<li>If we’ve got something of type $B$ which uses something of type $A$ from the typing environment, we can abstract over that something to create a function of type $A B$.</li>
<li>If we’ve got something of type $A B$ which uses some chunk of the typing environment called $$, and something of type $A$ which uses the rest of the typing environment called $$, then we can apply the former to the latter to get something of type $B$ which uses both $$ and $$.</li>
</ul>
<p>Then, the rules for units:</p>
<ul>
<li>We can always construct the unit value, and doing so uses no resources.</li>
<li>If we’ve got something of the unit type $$ which uses some chunk of the typing environment called $$, and something of type $A$ which uses the rest of the typing environment called $$, then we can put them together to get something of type $A$ which uses both $$ and $$.</li>
</ul>
<p>And finally, the rules for pairs:</p>
<ul>
<li>If we’ve got something of type $A$ which uses some chunk of the typing environment called $$, and something of type $B$ which uses the rest of the typing environment $$, then we can put them together as a pair of type $A B$ which uses both $$ and $$.</li>
<li>If we’ve got something of type $A B$ which uses some chunk of the typing environment called $$, and something of type $C$ which uses the rest of the typing environment $$ <em>plus</em> something of type $A$ and something of type $B$, we can put them together to get something of type $C$ which uses both $$ and $$.</li>
</ul>
<p>Great, that settles it for our term language, doesn’t it? Oh, right! We’re gonna have to give types to $$, $$, $$ and $$ as well! And for that, we’ll need session types! We’re gonna import those from the π-calculus, but we’re making one change—instead of only sending and receiving values of <em>session types</em> we’re gonna allow ourselves to send and receive values of <em>any type</em>:</p>
<div class="mathpar">
$
$ $
<p>$</p>
</div>
<p>Remember <em>duality</em> on session types? Yeah, we’re also gonna need that:</p>
$$
<p>$$</p>
<p>Okay, and we’re finally ready to give types to our concurrency primitives:</p>
<div class="mathpar">
$
$ $
<p>$</p>
$
$ $
<p>$</p>
</div>
<p>In order of appearance:</p>
<ul>
<li>$$ creates a new session-typed channel, and returns a pair of two endpoints with dual types.</li>
<li>$$ takes a <em>thunk</em>, a function of type $$, and spawns it off as a thread. This has to be a function, since otherwise we’d be able to start evaluating the thunk before it’s spawned off.</li>
<li>$$ takes a value, and an endpoint to a session-typed channel over which such a value can be send, and returns the continuation of the session.</li>
<li>$$ takes an endpoint to a session-typed channel over which a value can be received, and returns a pair of the received value and the continuation of the session.</li>
</ul>
<p>Right, so that’s terms properly taken care of. What about processes? To the surprise of, I hope, absolutely nobody, we’re pretty much gonna copy over the typing rules from the π-calculus, best we can. There’s one small difference. Remember how we were marking threads as either <em>main</em> or <em>child</em> threads, and only the main thread could return a value? That’s gonna show in our typing rules. First off, we’ll have two ways of embedding terms as processes—either as a main thread or as a child thread—which will show up in the typing judgement:</p>
<div class="mathpar">
$
$ $
<p>$</p>
</div>
<p>The <em>premises</em> here refer back to our term typing rules, but the conclusions uses our process typing rules. In our process typing judgements, we’re marking which kind of thread we’re dealing with on top of the $$, <em>e.g.</em>, as $^$ or $^$. When we compose multiple processes in parallel, we’re going to want to keep track of whether the composition as a whole contains the main thread or not—we’ll do this by combining the markings of the two processes, written $+^$:</p>
<div class="mathpar">
$
<p>$</p>
</div>
<p>We’re not listing what $+$ equals—it’s not allowed! If you try to add two main threads, that’s a big no-no, as it violates our <em>only one main thread</em> restriction! Okay, so we’re now in a place where we can copy over the remaining rules—the ones for ν-binders and parallel composition:</p>
<div class="mathpar">
$
$ $
<p>$</p>
</div>
<p>Phew, I think that’s it! We’ve got typing rules! Specifically, we’ve now got typing rules which ensure that the session protocol is followed… so we should be able to show that <a href="#formal-Ada-and-Briar">the interaction between Ada and Briar</a> is well-typed. I’ll leave it up to you to verify this, as the proof is quite big and I’m pretty tired of typesetting things by now. And, great news, <a href="#formal-cheeky-Ada">cheeky Ada</a> is <em>not</em> well-typed! There’s two reasons—one is a bit cheeky, but the other one is a bit more satisfactory:</p>
<ol type="1">
<li>We cannot type cheeky Ada because we have no recursion <em>(cheeky)</em>.</li>
<li>We cannot type cheeky Ada because she uses the communication channel repeatedly, which violates linearity <em>(satisfactory)</em>.</li>
</ol>
<p>Unfortunately, this type system for the concurrent λ-calculus has similar problems to the type system we showed for the π-calculus… it’s really very restrictive, and yet it still has deadlocks if you start mixing multiple sessions. Fortunately, the same solutions we gave for the π-calculus can be used here. <a href="https://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-sessions/propositions-as-sessions-jfp.pdf">Philip Wadler</a> has an example of the first solution, where you glue together ν-binders and parallel composition, in a calculus he calls <em>Good Variation</em>. <a href="http://dx.doi.org/10.1007/978-3-319-19195-9_1">Luca Padovani and Luca Novara</a> have an example of the second solution, where you do a global check to see if you have any cyclic dependencies.</p>
<h3 id="session-end">Session End</h3>
<p>Whew, that sure was quite a number of words! Let’s look back on what we learned:</p>
<h3 id="the-λ-calculus">The λ-calculus</h3>
<ul>
<li>The untyped λ-calculus is a really neat model of computation, but it’s got some problems, namely programs which do nothing forever, and programs which do silly things like adding numbers to functions.</li>
<li>There’s several approaches to mitigate these problems via type systems, but it’s always a struggle between how many bad programs you rule out versus how many good programs you rule out with them—and how unwieldy your type system gets.</li>
</ul>
<h3 id="the-π-calculus">The π-calculus</h3>
<ul>
<li>The untyped π-calculus is, like the λ-calculus, a really neat model of computation, and it’s even more expressive, in that it can model concurrency. However, this comes with all the problems of concurrency. Suddenly, we find ourselves facing deadlocks and race conditions!</li>
<li>There’s several approaches to mitigate these problems via type systems, but again, it’s always a struggle between how many bad programs you rule out versus how many good programs you rule out with them—and how unwieldy your type system gets.</li>
</ul>
<h3 id="the-concurrent-λ-calculus">The concurrent λ-calculus</h3>
<ul>
<li>We can smash together the λ-calculus and the π-calculus to get the concurrent λ-calculus, with the best of both worlds—it has higher-order functions <em>and</em> can model concurrency—and the worst of both worlds—now you’ve got to reason about higher-order functions <em>and</em> concurrency!</li>
<li>Unsurprisingly, the semantics and type systems for the concurrent λ-calculus look a lot like what you’d get if you smashed the semantics and type systems for the λ-calculus and the π-calculus together, but there’s some tiny tweaks we need to make to get them to behave like we want to.</li>
</ul>
<p>If you made it this far, thanks for reading altogether too many words! If you didn’t—<em>how are you reading this?!</em>—thanks for reading whatever number of words you thought was appropriate.</p>
<hr />
<p><strong>Disclaimer</strong>: I haven’t proven any safety properties for any of the calculi presented here. The simply-typed λ-calculus is pretty well established, so you can trust that’s good, but the other systems are potentially destructive simplifications of existent systems, so all bets are off! I guess you could do the proofs yourself—or if you wanna be really safe, refer to the papers I’ve linked. However, I’ve opted to make these simplifications because the smallest typed π-calculi which are <em>actually</em> expressive tend to be pretty big already</p>