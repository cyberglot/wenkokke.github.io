---
title: An Introduction to Session Types
katex: true
---

# Roadmap

+ The λ-calculus studies *functions*. 
+ The π-calculus studies *communication*.

## Example

TODO: create a running example

## Introduction to the λ-calculus

### Syntax and semantics

$$
\begin{array}{l}
\text{Term} \; L, M, N
\\
\quad
  \begin{array}{rl}
  \coloneqq & x 
  \;\mid\;    \lambda x.M 
  \;\mid\;    M \; N
  \end{array}
\end{array}
$$

<div class="mathpar">
$$
\begin{array}{l}
(\lambda x.M)\;V
\longrightarrow
M\{V/x\}
\end{array}
$$
$$
\begin{array}{c}
M
\longrightarrow
M^\prime
\\ \hline
M \; N
\longrightarrow
M^\prime \; N
\end{array}
$$
$$
\begin{array}{c}
N
\longrightarrow
N^\prime
\\ \hline
V \; N
\longrightarrow
V \; N^\prime
\end{array}
$$
</div>


### Linear types

<div class="mathpar">
$$
\begin{array}{c}
\\ \hline
x : A \vdash x : A
\end{array}
$$
$$
\begin{array}{c}
\Gamma, x : A \vdash M : B
\\ \hline
\Gamma \vdash (\lambda x.M) : A \multimap B
\end{array}
$$
$$
\begin{array}{c}
\Gamma \vdash M : A \multimap B \quad \Delta \vdash N : A
\\ \hline
\Gamma, \Delta \vdash M \; N : B
\end{array}
$$
</div>


## Introduction to the π-calculus

## Syntax

$$
\begin{array}{l}
\text{Process}\;{P},{Q},{R}
\\
\quad
  \begin{array}{rll}
  \coloneqq & (\nu {x}{x^\prime}){P} &\text{— create new channel }{x}{\leftrightarrow}{x^\prime}
  \\\mid    & ({P}\parallel{Q})      &\text{— put }P\text{ and }Q\text{ in parallel}
  \\\mid    & 0                      &\text{— done}
  \\\mid    & x\langle{y}\rangle.{P} &\text{— send }y\text{ on }x
  \\\mid    & x(y).{P}               &\text{— receive }y\text{ on }x
  \end{array}
\end{array}
$$

### Reduction semantics

<div class="mathpar">
$$
\begin{array}{c}
(\nu xx^\prime)(x\langle{y}\rangle.{P}\parallel x^\prime(z).{Q}) 
\\
\downarrow
\\
(\nu xx^\prime)({P}\parallel{Q}\{y/z\})
\end{array}
$$
</div>
<div class="mathpar">
$$
\begin{array}{c}
  \begin{array}{c}
    {P}
    \longrightarrow
    {P}^\prime
    \\ \hline
    (\nu xx^\prime){P}
    \longrightarrow
    (\nu xx^\prime){P}^\prime
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
</div>

### Structural congruence

<div class="mathpar">
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
  (\nu xx^\prime)(\nu yy^\prime)P
  & \equiv
  & (\nu yy^\prime)(\nu xx^\prime)P
  \\
  (\nu xx^\prime)(P \parallel Q)
  & \equiv
  & (\nu xx^\prime)P \parallel Q,
  \\
  &
  & \text{if}\;x,x^\prime\not\in{Q}
\end{array}
$$

$$
\begin{array}{c}
  P \equiv P^\prime \quad P^\prime \longrightarrow Q^\prime \quad Q^\prime \equiv Q
  \\ \hline
  P \longrightarrow Q
\end{array}
$$
</div>

### Commuting conversions

<div class="mathpar">
$$
\begin{array}{lrl}
x\langle{y}\rangle.P \parallel Q 
& \longrightarrow 
& x\langle{y}\rangle.(P \parallel Q)
\\
x(y).P \parallel Q 
& \longrightarrow 
& x(y).(P \parallel Q)
\end{array}
$$
</div>

TODO: caveats about process calculus interpretation

TODO: caveats about unrestricted conversions

<div class="mathpar">
$$
\begin{array}{c}
(\nu xx^\prime)(x\langle{y}\rangle.P \parallel x^\prime(y^\prime).Q)
\\
\downarrow
\\
(\nu xx^\prime)(x\langle{y}\rangle.(P \parallel x^\prime(y^\prime).Q))
\end{array}
$$
</div>


### Label transition semantics

$$
\begin{array}{l}
\text{Label}\;{\ell}
\\
\quad
  \begin{array}{rll}
  \coloneqq & \tau                     &\text{— can do something by itself}
  \\\mid    & \ell\parallel\ell^\prime &\text{— can do }\ell\text{ and }\ell^\prime\text{ in parallel}
  \\\mid    & x\langle{y}\rangle       &\text{— can send }y\text{ on }x
  \\\mid    & x(y)                     &\text{— can receive }y\text{ on }x
  \end{array}
\end{array}
$$

<div class="mathpar">
$$
\begin{array}{c}
  x\langle{y}\rangle.P \xrightarrow{x\langle{y}\rangle} P
  \quad
  x(y).P \xrightarrow{x(y)} P
  \\
  \\
  \begin{array}{c}
  P \xrightarrow{x\langle{y}\rangle\parallel{x^\prime(z)}} P^\prime
  \\ \hline
  (\nu xx^\prime)P \xrightarrow{\tau} (\nu xx^\prime)P^\prime\{y/z\}
  \end{array}
\end{array}
$$
</div>

NOTE: mention this is _late_ semantics

<div class="mathpar">
$$
\begin{array}{c}
  \begin{array}{c}
    P \xrightarrow{\ell} P^\prime
    \quad
    x,x^\prime\not\in\text{fv}(\ell)
    \\ \hline
    (\nu xx^\prime)
    P \parallel Q \xrightarrow{\ell} P^\prime \parallel Q
  \end{array}
  \\
  \\
  \begin{array}{c}
    \begin{array}{c}
      P \xrightarrow{\ell} P^\prime
      \\ \hline
      P \parallel Q \xrightarrow{\ell} P^\prime \parallel Q
    \end{array}
    \quad
    \begin{array}{c}
      Q \xrightarrow{\ell} Q^\prime
      \\ \hline
      P \parallel Q \xrightarrow{\ell} P \parallel Q^\prime
    \end{array}
  \end{array}
  \\
  \\
  \begin{array}{c}
    P \xrightarrow{\ell} P^\prime
    \quad
    Q \xrightarrow{\ell\parallel\ell^\prime} Q^\prime
    \\ \hline
    P \parallel Q \xrightarrow{\ell\parallel\ell^\prime} P^\prime \parallel Q^\prime
  \end{array}
\end{array}
$$
</div>

## Session types

$$
\begin{array}{l}
\text{Protocol} \; A, B
\\
\quad
  \begin{array}{rl}
  \coloneqq & {!}A.B
  \;\mid\;    {?}A.B
  \;\mid\;    \mathbf{end}
  \end{array}
\end{array}
$$

<div class="mathpar">
$$
\overline{{!}A.B} = {?}{A}.\overline{B}
$$
$$
\overline{{?}A.B} = {!}{A}.\overline{B}
$$
$$
\overline{\mathbf{end}} = \mathbf{end}
$$
</div>

<div class="mathpar">
$$
\begin{array}{c}
  \begin{array}{c}
    \Gamma, x : A, x^\prime : \overline{A} \vdash P
    \\ \hline
    \Gamma \vdash (\nu xx^\prime)P
  \end{array}
  \quad
  \begin{array}{c}
    \Gamma \vdash P \quad \Delta \vdash Q
    \\ \hline
    \Gamma, \Delta \vdash P \parallel Q
  \end{array}
  \\
  \begin{array}{c}
    \\ \hline
    \varnothing \vdash 0
  \end{array}
\end{array}
$$

$$
\begin{array}{c}
  \begin{array}{c}
    \Gamma, x : B \vdash P
    \\ \hline
    \Gamma, x : {!}A.B, y : A \vdash x\langle{y}\rangle.P
  \end{array}
  \\
  \\
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
</div>


## Concurrent λ-calculus

+ If we want to add communication to our λ-calculus, 
  we’re gonna have to extend it with π-calculus constructs,
  which isn’t trivial, because…
+ Adding π-calculus constructs forces us to think of things we don’t normally consider for functional languages…
+ We’re used to thinking…

  - Reduction is deterministic
  - Reduction is syntax-driven
  - Syntactically distinct is morally distinct

  …but all of that goes out the window when we move to the π-calculus.


## Lessons from the π-calculus

### Dealing with non-determinism…
 
- Non-deterministic local choice
- Guarded local choice
- Controlled non-determinism

### Dealing with deadlocks…

- Syntactic restrictions
- Dependency analysis

<!--
-->
