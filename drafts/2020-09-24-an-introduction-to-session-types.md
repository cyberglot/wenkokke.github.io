---
title: An Introduction to Session Types
katex: true
---

Session types. Ostensibly, I’ve studied them for the past few years, so I should know something about them, right? I am going to try and explain the *foundations* of session types, and along the way, there will be programs which crash, Victorian ladies having milk puddings, and tin can telephones.

<!--more-->

Let’s start out with a *dramatis personæ*. 

## Dramatis Personæ

Session types are about *channels*, which are like *tin can telephones*, in that you can use your tin to whisper sweet little nothings to every friend who has a tin connected to yours. I know, my love for tin can telephones is betraying my age a little—I’m an old Victorian lady.

<figure>
    [![Two victorian ladies hold a tin can telephone pulled taut between them.](/public/images/tin-can-telephone.png)](https://books.google.com/books?id=IB8_AAAAYAAJ&dq=Ebenezer+Cobham+Brewer,+Fran%C3%A7ois+Napol%C3%A9on+Marie+Moigno,+Henri+de+Parville&pg=PA227&redir_esc=y#v=onepage&q&f=false)
    <figcaption>“You simply must try my milk puddings, Ada”, Briar whispers into the telephone.</figcaption>
</figure>

*Ada*
: A Victorian Lady.

*Briar*
: Ada’s Lady Friend.

*The Tin Labelled A*
: A Tin Held by Ada.

*The Tin Labelled B*
: A Tin Held by Briar.

*The Piece of Twine*
: A Piece of Twine Connecting Tin A and B.

In the vernacular of session types, the tin cans are referred to as *channel-endpoints* or simply *endpoints*, and the collection of all tin cans held together by the twine is referred to as a *channel*. A series of messages whispered back and forth over a single channel is referred to as a *session*. 

Most of the literature on session types considers only the classic scenario, in which we connect exactly two tin cans to form a channel—this is referred to as *binary session types*. Yet if we wanted to, we could make a telephone with any number of tin cans—this is referred to as *multiparty session types*.

In this blog post, we I’ll focus on *binary session types*


## Session Types *at a Glance*

Let’s imagine for a moment that Ada were to take Briar up on her offer, and ask her to sample her famous milk puddings. Briar, a proper lady, only offers her milk puddings to those who make a *sufficiently polite* request—Ada must be polite and say “please”, but she must not overuse it, lest she comes off as begging! 

We encode the interaction between Ada and Briar using *session types* in Haskell:

- Ada’s requests are represented using the `Request` datatype, which allows us to prefix a request for pudding with any number of uses of `Please`;
- Briar’s response is represented using the `Response` datatype, in which she can either grant permission, in which case Briar sends an `Allow` with a sample of pudding attached, or refuse Ada’s request, in which case she sends a `Deny` with a reason.

The functions `ada` and `briar` represent Ada and Briar—these functions each receive an endpoint for the shared channel, and communicate along the lines of our story—Ada sends a request, Briar evaluates her politeness and responds with either pudding or a refusal, and finally Ada evaluates Briars response, and expresses her emotions accordingly.

```haskell
data Request
  = Please Request
  | MayIHaveSomePudding
    
data Response
  = Allow Pudding
  | Deny String
    
ada :: Send Request (Recv Response End) -> IO ()
ada chan = do
  chan' <- send (Please MayIHaveSomePudding) chan
  (resp, chan'') <- recv chan'
  close chan''
  case resp of
    Allow pudding -> putStrLn "I’m so happy!"
    Deny reason -> putStrLn "Woe is me!"

briar :: Recv Request (Send Response End) -> IO ()
briar chan = do
  (req, chan') <- recv chan
  let resp = case req of
    MayIHaveSomePudding -> Deny "Such rudeness!"
    Please MayIHaveSomePudding -> Allow myPudding
    Please (Please _) -> Deny "Such beggary!"
  chan'' <- send resp chan'
  close chan''
```

The example illustrates a crucial notions for session types:

Firstly, session types are *communication protocols.* If you glance at the types of the endpoints, you see that they represent the communication protocol from each participants perspective. For instance, Ada’s endpoint says she must send a request, receive a response, and then end the session.

Secondly, the types of the endpoints of a binary channel must be *dual*. When Ada’s endpoint says she must send, Briar’s endpoint says she must receive. For multiparty session types, the equivalent notion is called *coherence*, but the principle remains the same.

Finally, each endpoint must be used *exactly once* if we want to be sure to stick to the protocol. For instance, in the code above, each channel-endpoint is only used once, and each send or receive returns a new channel on which to continue the communication. If we didn’t, we would be able to write a cheeky variant of Ada, who simply tries any number of pleases until she gets that sweet, sweet pudding.

```haskell
ada :: Send Request (Recv Response End) -> IO ()
ada chan = tryAll MayIHaveSomePudding chan
  where
    tryAll req chan = do 
      chan' <- send req chan
      (resp, chan'') <- recv chan'
      close chan''
      case resp of
        Allow pudding -> putStrLn "I’m so happy!"
        Deny reason -> tryAll (Please req) chan
```

But that’s not what the protocol says! Briar doesn’t have time for more than one request, so after the first one has run its course, Ada whispers her second request into the tin can, then waits forever, pining for a response from Briar which will never come!


## A Bit of a Roadmap

Only a few short years after Ada and Briar enjoyed sweet milk puddings, a man by the name of *Alonzo Church* was born in Washington, D.C., in the United States. Three decades later, in the 1930s, Alonzo would develop [the λ-calculus][church1932], a foundational calculus which studies computation using *functions*. To this day, the λ-calculus underpins most theories of functional programming languages. Talk about influential! 

Only a few short years after Alonzo developed the λ-calculus, a man by the name of *Robin Milner* was born near Yealmpton, in England. Alonzo lived a long live, over nine decades! A few years before Alonzo’s death in the mid 1990s, Robin, together with Joachim Parrow and David Walker, developed the [π-calculus][milner1992], a foundational calculus which studies concurrent computation by processes using *message-passing communication*. It wasn’t the first process calculus—it itself was heavily influenced by ideas dating back to the early 1980s—but it’s certainly one of the most influential!

We’ll start out by discussing the untyped λ-calculus. It’s a wonderful little language, and it’s *really* powerful. Unfortunately, it has all sorts of programs that do all sorts of bad things, like loop forever, so with all that power, it’s *really scary* too! We’ll then discuss the idea of taming all that scary power using types, to try and get only well-behaved programs, and the challenges of taming it without taking all the *oomph* out.

Then, we’ll switch to discussing the π-calculus. It’s a wonderful little language, even if it’s twice as big as the λ-calculus—with *six* constructs instead of *three*! It’s even more powerful than the λ-calculus—it can express all sorts of concurrent behaviours that the λ-calculus has no hope of expressing. Unfortunately, it’s scarier as well—there’s way more things that can go wrong! Again, we’ll turn our attention to taming all that scary power using types, and the problems of *oomph*’lessness that comes with it.

Finally, we’ll talk about having the best of both worlds, in a concurrent λ-calculus, which is sorta what you get when you smash the λ-calculus and the π-calculus together at high speeds! The concurrent λ-calculus has the best of both worlds: higher-order functions and concurrency with message-passing communication!


## The λ-calculus! *(So powerful, so scary…)*

The untyped λ-calculus celebrated its 89th birthday last November, so to say that it’s been around for a while undersells it a bit. It’s a pretty small system—it has only three things—there’s variables, λ-abstractions to make functions, and function applications to get rid of ’em.

$$
\begin{array}{l}
\text{Term} \; L, M, N
\\
\quad
  \begin{array}{rl}
  ::= & x
  \;\mid\;    \lambda x.M
  \;\mid\;    M \; N
  \end{array}
\end{array}
$$

There’s only one computation rule—if a function $\lambda x.M$ meets its argument $N$ we replace all occurrences of $x$ in the function body $M$ with the argument $N$. The other two reduction rules are really just there to let us reduce inside of function applications.

$$
\begin{array}{c}
  \begin{array}{c}
  (\lambda x.M)\;N
  \longrightarrow
  M\{N/x\}
  \end{array}
  \\
  \\
  \begin{array}{c}
  M
  \longrightarrow
  M^\prime
  \\ \hline
  M \; N
  \longrightarrow
  M^\prime \; N
  \end{array}
  \quad
  \begin{array}{c}
  N
  \longrightarrow
  N^\prime
  \\ \hline
  M \; N
  \longrightarrow
  M \; N^\prime
  \end{array}
\end{array}
$$

The λ-calculus is *very* powerful—*some stuff about it being a “universal model of computation”*—but that power comes at the cost of also being able to express quite a lot of scary programs that do bad stuff.

For instance, the λ-calculus comes with general recursion out of the box, via the $Y$ combinator! We’ll see an example of using the $Y$ combinator below, but essentially, $Y\;f$ represents an infinite series of applications of $f$.

$$
\begin{array}{lrl}
Y
& =               & \lambda f. (\lambda x. x \; x) (\lambda x. f \; (x \; x)) \\
\\
Y \; f
& \longrightarrow & f \; (Y \; f)
\\
& \longrightarrow & f \; (f \; (Y \; f))
\\
& \longrightarrow & f \; (f \; (f \; (Y \; f)))
\\
& \longrightarrow & \dots
\end{array}
$$

That’s good—as programmers, we like recursion! Really simplifies your programs, not having to write out the case for every possible input!

However, if you pass $Y$ the identity function, you’ll get $Ω$—the program which runs forever, but never gets anything done! Watch it reduce to right back to itself in a single step!

$$
\begin{array}{lrl}
\Omega
& =               & (\lambda x. x \; x) (\lambda x. x \; x)
\\
& \longrightarrow & (x \; x)\{\lambda x. x \; x/x\}
\\
& =               & (\lambda x. x \; x) (\lambda x. x \; x)
\end{array}
$$

That’s scary, I’d prefer not to have that! Programs which run forever, but never do a single thing—or worse, programs which are doing things the whole time, but never produce any outputs!

Most functional languages don’t just implement the core λ-calculus, but rather extend the λ-calculus with various constructs—numbers, addition, multiplication, pairs, sums, *etc.* *Technically speaking*, these can all be encoded using just function—see, *e.g.*, Church encodings—but it tends to be *a lot* more practical and faster to use, *e.g.*, machine numbers.

For example, we can extend the untyped λ-calculus with Peano numbers. First, we extend the term language with the number *zero*, written $\text{zero}$, the successor function, written $\text{suc}$, and a pattern matching construct for numbers, written $\text{case}\;L\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}$.

$$
\begin{array}{l}
\text{Term} \; L, M, N
\\
\quad
  \begin{array}{rl}
  ::= & \dots
  \\\mid &    \text{zero}
  \;\mid\;    \text{suc}
  \\\mid &    \text{case}\;L\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}
  \end{array}
\end{array}
$$

Then, we extend the reduction rules with two reduction rules for pattern matches on numbers—depending on whether the number is zero or a successor—and a rule to let us reduce inside of pattern matches.

$$
\begin{array}{c}
  \text{case}\;\text{zero}\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}
  \longrightarrow
  M
  \\
  \\
  \text{case}\;\text{suc}\;{L}\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}
  \longrightarrow
  N\{L/x\}
  \\
  \\
  \begin{array}{c}
  L
  \longrightarrow
  L^\prime
  \\ \hline
  \text{case}\;L\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}
  \\
  \downarrow
  \\
  \text{case}\;L^\prime\;\text{of}\;\{\text{zero}\mapsto{M};\text{suc}\;{x}\mapsto{N}\}
  \end{array}
\end{array}
$$

We can now define addition on Peano numbers in our calculus! Ideally, we’d write something like the following, familiar definition for addition.

$$
\begin{array}{lllcl}
\text{plus} & \text{zero} & {N} & = & \text{zero}
\\
\text{plus} & (\text{suc}\;{M}) & {N} & = & \text{suc}\;(\text{plus}\;{M}\;{N})
\end{array}
$$

Our core language doesn’t support recursive or pattern matching definitions, so we’ll have to elaborate the above definition into something less familiar, which uses the $Y$ combinator and the pattern matching construct. It’s a bit more verbose, but it’s addition nonetheless!

$$
\begin{array}{l}
\text{plus} \triangleq \lambda{x}.\lambda{y}.(Y\;(\text{plus}^\prime\;{y}))\;{x}
\\
\quad\textbf{where}
\\
\quad\quad
\text{plus}^\prime \triangleq \lambda{y}.\lambda{\text{rec}}.\lambda{x}.
\\
\quad\quad\quad\text{case}\;{x}\;\text{of}\;\left\{
\begin{array}{lcl}
\text{zero} & \mapsto & \text{zero};
\\
\text{suc}\;{x^\prime} & \mapsto & \text{suc}\;(\text{rec}\;{x^\prime}\;{y})
\end{array}
\right\}
\end{array}
$$

Woe is us! We have *another* kind of problem! We now have to worry about programs like $\text{plus}\;(\lambda x.x)\;\text{zero}$. What does that even mean?! According to our semantics, it means exactly that, since it doesn’t reduce any further… It’s stuck on the pattern match on $\lambda x.x$, since there’s no case for functions.

Problems like these are less obvious when using, *e.g.*, Church encodings, since everything is just functions. For instance, if we use Church-encoded Peano numbers to compute $\text{plus}_{ch} \; (\lambda x.x) \; \text{zero}_{ch}$, and convert the result to our builtin Peano numbers, we find that adding the identity function to the number *zero* gives us *one*.

$$
\begin{array}{lcl}
\text{zero}_{ch} & \triangleq & \lambda s. \lambda z. z
\\
\text{suc}_{ch}  & \triangleq & \lambda n. \lambda s. \lambda z. s \; (n \; s \; z)
\\
\text{plus}_{ch} & \triangleq & \lambda m. \lambda n. \lambda s. \lambda z. m \; s \; (n \; s \; z)
\end{array}
$$

$$
\begin{array}{l}
\text{plus}_{ch} \; (\lambda x.x) \; \text{zero}_{ch} \; \text{suc} \; \text{zero}
\\
\quad
  \begin{array}{rl}
  \longrightarrow & (\lambda x.x) \; \text{suc} \; (\text{zero}_{ch} \; \text{suc} \; \text{zero})
  \\
  \longrightarrow & (\lambda x.x) \; \text{suc} \; \text{zero}
  \\
  \longrightarrow & \text{suc} \; \text{zero}
  \end{array}
\end{array}
$$

All in all, we’ve identified *two* main problems with using the untyped λ-calculus as a foundation for programming languages:

1. it has programs which loop forever but produce nothing; and
2. it has no way of making sure that data is used as intended.

Specifically, for the second problem, we have the choice between programs which get stuck and programs which compute nonsense. For the first option, we tag our data—by using, *e.g.*, a syntactically distinct $\text{zero}$ constructor—and check the types of our data at runtime. For the second option, we encode everything as functions, and accept whatever results fall out of our encoding.


## Taming the λ-calculus with types…

$$
\begin{array}{c}
  \begin{array}{c}
  \Gamma \ni x : A
  \\ \hline
  \Gamma \vdash x : A
  \end{array}
  \quad
  \begin{array}{c}
  \Gamma, x : A \vdash M : B
  \\ \hline
  \Gamma \vdash \lambda x.M : A \to B
  \end{array}
  \\\\
  \begin{array}{c}
  \Gamma \vdash M : A \to B \quad \Gamma \vdash N : A
  \\ \hline
  \Gamma \vdash M \; N : B
  \end{array}
\end{array}
$$

## The π-calculus! *(Is even scarier…)*

$$
\begin{array}{l}
\text{Process}\;{P},{Q},{R}
\\
\quad
  \begin{array}{rll}
     ::=    & (\nu {x}){P}           &\text{— create new channel}
  \\\mid    & ({P}\parallel{Q})      &\text{— put }P\text{ and }Q\text{ in parallel}
  \\\mid    & 0                      &\text{— done}
  \\\mid    & x\langle{y}\rangle.{P} &\text{— send }y\text{ on }x
  \\\mid    & x(y).{P}               &\text{— receive }y\text{ on }x
  \\\mid    & !{P}                   &\text{— replicate }P
  \end{array}
\end{array}
$$

$$
\begin{array}{c}
  \begin{array}{c}
  (\nu x)(x\langle{y}\rangle.{P}\parallel x(z).{Q})
  \longrightarrow
  (\nu x)({P}\parallel{Q}\{y/z\})
  \end{array}
  \\
  \\
  \begin{array}{c}
    {P}
    \longrightarrow
    {P}^\prime
    \\ \hline
    (\nu x){P}
    \longrightarrow
    (\nu x){P}^\prime
  \end{array}
  \\
  \\
  \begin{array}{c}
    {P}
    \longrightarrow
    {P}^\prime
    \\ \hline
    {P}\parallel{Q}
    \longrightarrow
    {P}^\prime\parallel{Q}
  \end{array}
  \quad
  \begin{array}{c}
    {Q}
    \longrightarrow
    {Q}^\prime
    \\ \hline
    {P}\parallel{Q}
    \longrightarrow
    {P}\parallel{Q}^\prime
  \end{array}
\end{array}
$$

$$
(\nu x)(x(z).{Q}\parallel x\langle{y}\rangle.{P})
$$

$$
(\nu x)(x\langle{y}\rangle.{P}\parallel x(z).{Q})
\longrightarrow
(\nu x)({P}\parallel{Q}\{y/z\})
$$

$$
\begin{array}{lrll}
  P \parallel Q
  & \equiv
  & Q \parallel P
  \\
  P \parallel (Q \parallel R)
  & \equiv
  & (P \parallel Q) \parallel R
  \\
  P \parallel 0
  & \equiv
  & P
  \\
  (\nu x)(\nu y)P
  & \equiv
  & (\nu y)(\nu x)P
  \\
  (\nu x)(P \parallel Q)
  & \equiv
  & (\nu x)P \parallel Q,
  & \text{if}\;x\not\in{Q}
  \\
  !{P}
  & \equiv
  & !{P}\parallel{P}
\end{array}
$$

$$
\begin{array}{c}
  P \equiv P^\prime \quad P^\prime \longrightarrow Q^\prime \quad Q^\prime \equiv Q
  \\ \hline
  P \longrightarrow Q
\end{array}
$$

$$
\begin{array}{c}
  (\nu x)%
  \left(
  \begin{array}{l}
  x\langle{y_1}\rangle.{P_1}\parallel x(z_1).{Q_1}\parallel
  \\
  x\langle{y_2}\rangle.{P_2}\parallel x(z_2).{Q_2}
  \end{array}
  \right)
  \\
  \\
  \downarrow
  \\
  \\
  (\nu x)%
  \left(
  \begin{array}{l}
  {P_1}\parallel {Q_1}\{y_1/z_1\}\parallel
  \\
  {P_2}\parallel {Q_2}\{y_2/z_2\}
  \end{array}
  \right)
  \\
  \\
  \mathit{or}
  \\
  \\
  (\nu x)%
  \left(
  \begin{array}{l}
  {P_1}\parallel {Q_1}\{y_2/z_1\}\parallel
  \\
  {P_2}\parallel {Q_2}\{y_1/z_2\}
  \end{array}
  \right)
\end{array}
$$

$$
\begin{array}{c}
  (\nu x)%
  \left(
  x(z).{P}\parallel x(w).{Q}
  \right)
  \not\longrightarrow
\end{array}
$$


## Taming the π-calculus with types…

$$
\begin{array}{l}
\text{Process}\;{P},{Q},{R}
\\
\quad
  \begin{array}{rll}
     ::=    & (\nu xx'){P} &\text{— create new channel }{x}{\leftrightarrow}{x'}
  \\\mid    & \dots
  \end{array}
\end{array}
$$

$$
\begin{array}{l}
\text{Session type}\;{S}\quad
\\
\quad
  \begin{array}{rll}
     ::= & !S.S'        &\text{— send}
  \\\mid & ?S.S'        &\text{— receive}
  \\\mid & \mathbf{end} &\text{— done}
  \end{array}
\\
\end{array}
$$

$$
\begin{array}{l}
\text{Duality}
\\
\quad
    \begin{array}{lrl}
    \overline{!S.S'}        & = & ?S.\overline{S'}
    \\
    \overline{?S.S'}        & = & !S.\overline{S'}
    \\
    \overline{\mathbf{end}} & = & \mathbf{end}
    \end{array}
\end{array}
$$

$$
\begin{array}{c}
  \begin{array}{c}
  \Gamma, x : S, x' : \overline{S} \vdash P
  \\ \hline
  \Gamma \vdash (\nu xx') P
  \end{array}
  \quad
  \begin{array}{c}
  \Gamma \vdash P
  \quad
  \Delta \vdash Q
  \\ \hline
  \Gamma, \Delta \vdash P \parallel Q
  \end{array}
  \\
  \\
  \begin{array}{c}
  \\ \hline
  \varnothing \vdash 0
  \end{array}
  \quad
  \begin{array}{c}
  \Gamma, x : B \vdash P
  \\ \hline
  \Gamma, x : {!}A.B, y : A \vdash x\langle{y}\rangle.P
  \end{array}
  \\
  \\
  \quad
  \begin{array}{c}
  \Gamma, y : A, x : B \vdash P
  \\ \hline
  \Gamma, x : {?}A.B \vdash x(y).P
  \end{array}
  \quad
  \begin{array}{c}
  \Gamma \vdash P
  \\ \hline
  \Gamma, x : \mathbf{end} \vdash P
  \end{array}
\end{array}
$$



## λ and π, together at last!

$$
\begin{array}{c}
\begin{array}{l}
  \begin{array}{l}
  \text{Term} \; L, M, N
  \\
  \quad
    \begin{array}{rl}
    ::=    & x 
    \\\mid & \lambda x.M
    \\\mid & M \; N
    \\\mid & K
    \end{array}
  \end{array}
\end{array}
\quad
\begin{array}{l}
\text{Process}\;{P},{Q},{R}
\\
\quad
  \begin{array}{rll}
     ::=    & (\nu xx'){P}
  \\\mid    & ({P}\parallel{Q})
  \\\mid    & M
  \\
  \\
  \end{array}
\\
\end{array}
\\
\\
\text{Const} \; K ::= \mathbf{send} \mid \mathbf{recv} \mid \mathbf{new} \mid \mathbf{spawn}
\end{array}
$$


$$
\begin{array}{c}
  (\nu xx')(\nu yy')%
  \left(
  \begin{array}{l}
  \mathbf{let}\;(\_,z)=\mathbf{recv}\;{x}
  \\
  \mathbf{in}\;\mathbf{send}\;{z}\;{y}; M
  \parallel
  \\
  \mathbf{let}\;(\_,w)=\mathbf{recv}\;{x'}
  \\
  \mathbf{in}\;\mathbf{send}\;{w}\;{y'}; N
  \end{array}
  \right)
  \\
  \\
  \downarrow
  \\
  \\
  ✨\;\mathit{nothing}\;✨
\end{array}
$$

[church1932]: https://www.jstor.org/stable/1968337
[milner1992]: http://www.lfcs.inf.ed.ac.uk/reports/89/ECS-LFCS-89-85/
