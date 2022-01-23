---
title: The Comprehension Construction
author: Emily Riehl and Dom Verity
date: January 15, 2022
tags: mathematics, ∞-cosmoi
citeproc: true
---

In 2017 [Emily Riehl](https://emilyriehl.github.io/) and I posted a paper [@RiehlVerity:2018cc] on the [arXiv](https://arxiv.org/abs/1706.10023) entitled "The comprehension construction." and we blogged about it on the [n-Category Café](https://golem.ph.utexas.edu/category/). That post explains the use of the term *comprehension* in the title of that paper.

I have reproduced it here to show off some Haskell hacking I've been indulging in to support the typesetting of category theoretic diagrams in blog post. To do this I've used [LaTeX](https://en.wikipedia.org/wiki/LaTeX) to generate SVG images, from diagrams specified using packages such as [PGF/TikZ](https://en.wikipedia.org/wiki/PGF/TikZ), which are then inlined directly into HTML pages.

<!--more-->

# What is the comprehension construction?

The comprehension construction is somehow analogous to both the straightening and the unstraightening constructions introduced by Lurie in his development of the theory of quasi-categories. Most people use the term *∞-categories* as a rough synonym for quasi-categories, but we reserve this term for something more general: the objects in any [∞-cosmos](https://ncatlab.org/nlab/show/infinity-cosmos). There is an ∞-cosmos whose objects are quasi-categories and another whose objects are complete Segal spaces. But there are also more exotic ∞-cosmoi whose objects model (∞,n)-categories or fibered (∞,1)-categories, and our comprehension construction applies to any of these contexts.

The input to the comprehension construction is any cocartesian fibration between ∞-categories together with a third ∞-category $A$. The output is then a particular homotopy coherent diagram that we refer to as the *comprehension functor*. In the case $A=1$, the comprehension functor defines a "straightening" of the cocartesian fibration. In the case where the cocartesian fibration is the universal one over the quasi-category of small ∞-categories, the comprehension functor converts a homotopy coherent diagram of shape $A$ into its "unstraightening," a cocartesian fibration over $A$.

The fact that the comprehension construction can be applied in any ∞-cosmos has an immediate benefit. The codomain projection functor associated to an ∞-category $A$ defines a cocartesian fibration in the slice ∞-cosmos over $A$, in which case the comprehension functor specializes to define the Yoneda embedding.

## Classical comprehension

The comprehension scheme in ZF set theory asserts that for any proposition $\phi$ involving a variable $x$ whose values range over some set $A$ there exists a subset 
$$
    \{ x \in A \mid \phi(x)\}
$$
comprised of those elements for which the formula is satisfied. If the proposition $\phi$ is represented by its characteristic function $\chi_\phi \colon A \to 2$, then this subset is defined by the following pullback
$$
    \begin{tikzpicture}[commutative diagrams/every diagram]
    \matrix[matrix of math nodes, name=m, row sep=3em, column sep=3em, commutative diagrams/every cell] {
        {\{x\in A\mid \phi(x)\}} & {1} \\
        {A} & {2} \\};
    \begin{scope}[shift={($(m-1-1)+(0.6,-0.6)$)}]
        \draw +(-.3,0) -- +(0,0) -- +(0,.3);
        % \fill +(-.15,.15) circle (.03);
    \end{scope}
    \path[commutative diagrams/.cd, every arrow, every label]
    (m-1-1) edge (m-1-2)
    (m-1-1) edge (m-2-1)
    (m-1-2) edge node {$\top$} (m-2-2)
    (m-2-1) edge node[swap] {$\chi_\phi$} (m-2-2);
    \end{tikzpicture}
$$
of the canonical monomorphism $\top \colon 1 \to 2$. For that reason, $2$ is often called the _subobject classifier_ of the category $\Set$ and the morphism $\top\colon 1 \to 2$ is regarded as being its _generic subobject_. On abstracting this point of view, we obtain the theory of [elementary toposes](https://en.wikipedia.org/wiki/Topos).

## The Grothendieck construction as comprehension

What happens to the comprehension scheme when we pass from the 1-categorical context just discussed to the world of 2-categories?

A key early observation in this regard, due to Ross Street I believe, is that we might usefully regard the [Grothendieck construction](https://ncatlab.org/nlab/show/Grothendieck+construction) as an instance of a generalised form of comprehension for the category of categories. This analogy becomes clear when we observe that the [category of elements](https://en.wikipedia.org/wiki/Category_of_elements) of a functor $F \colon \cC \to \Set$ may be formed by taking the pullback:
$$
    \begin{tikzpicture}[commutative diagrams/every diagram]
    \matrix[matrix of math nodes, name=m, row sep=3em, column sep=3em, commutative diagrams/every cell] {
        {\int F} & {\prescript{*/}{}{\Set}} \\
        {\cC} & {\Set} \\};
    \begin{scope}[shift={($(m-1-1)+(0.5,-0.5)$)}]
        \draw +(-.3,0) -- +(0,0)  -- +(0,.3);
        % \fill +(-.15,.15) circle (.03);
    \end{scope}
    \path[commutative diagrams/.cd, every arrow, every label]
    (m-1-1) edge (m-1-2)
    (m-1-1) edge (m-2-1)
    (m-1-2) edge (m-2-2)
    (m-2-1) edge node[swap] {$F$} (m-2-2);
    \end{tikzpicture}
$$
Here the projection functor on the right, from the slice ${}^{\ast/}\Set$ of the category of sets under the one point set, is a [discrete cocartesian fibration](https://ncatlab.org/nlab/show/discrete+fibration). It follows, therefore, that this pullback is also a [2-pullback](https://ncatlab.org/nlab/show/2-pullback) and that its left-hand vertical is a discrete cocartesian fibration.

Street's point of view is (roughly) that in a 2-category $\cK$ it is the (suitably defined) discrete cocartesian fibrations that play the role that the sub-objects inhabit in topos theory. Then the generic sub-object $\top\colon 1\to \Omega$ becomes a discrete cocartesian fibration $\top\colon S_\ast\to S$ in $\cK$ with the property that pullback of $\top$ along 1-cells $a\colon A\to S$ provides us with equivalences between each hom-category $\Fun_{\cK}(A,S)$ and the category $\dCoCart(\cK)_{/A}$ of discrete cocartesian fibrations over $A$ in $\cK$.

This account, however, glosses over one important point; thus far we have only specified that each comparison functor $\Fun_{\cK}(A,S) \to \dCoCart(\cK)_{/A}$ should act by pulling back $\top\colon S_{\ast}\to S$ along each 1-cell $a\colon A\to S$. We have said nothing about how, or weather, this action might extend in any reasonable way to 2-cells $\phi\colon a\Rightarrow b$ in $\Fun_{\cK}(A,S)$!

The key observation in that regard is that for any fixed "representably defined" cocartesian fibration $p\colon E\to B$ in a (finitely complete) 2-category $\cK$, we may extend pullback to define a pseudo-functor $\Fun_{\cK}(A,B)\to\cK/A$. This carries each 1-cell $a\colon A\to B$ to the pullback $p_a\colon E_a\to A$ of $p$ along $a$ and its action on a 2-cell $\phi\colon a\Rightarrow b$ is constructed in the manner depicted in the following diagram:
$$
    \begin{tikzpicture}[commutative diagrams/every diagram]
        \node (Ea) at (0,5) {$E_a$} ; \node (Eb) at (2,3) {$E_b$} ;
        \node (E) at (6,4) {$E$} ; \node (Aa) at (0,2) {$A$} ;
        \node (Ab) at (2,0) {$A$} ; \node (B) at (6,1) {$B$} ;
        % Please macro-ize me!
        \begin{scope}[shift={($(Ea)+(0.5,-0.5)$)}]
            \draw +(-.3,0) -- +(0,0) -- +(0,.3);
            % \fill +(-.15,.15) circle (.03);
        \end{scope}
        %
        \begin{scope}[shift={($(Eb)+(0.5,-0.5)$)}]
            \draw +(-.3,0) -- +(0,0) -- +(0,.3);
            % \fill +(-.15,.15) circle (.03);
        \end{scope}
        %
        \path[commutative diagrams/.cd, every arrow, every label, /tikz/.search also = {/tikz/commutative diagrams}]
        (Ea) edge [two heads] node [swap] {$p_a$} (Aa)
        (Aa) edge [bend left = 10] node {$a$} node [name=L, below] {} (B)
        (Eb) edge [two heads, crossing over] node [swap] {$p_b$} (Ab)
        (E) edge [two heads] node {$p$} (B)
        (Ea) edge [bend left = 10] node {$\ell_a$} node [name=U, below] {} (E)
        (Eb) edge [bend right = 15] node [swap] {$\ell_b$}(E) 
        (Ab) edge [bend right = 15] node [swap] {$b$} (B)
        (Aa) edge [equal] (Ab) (Ea) edge [dotted] node [swap] {$E_\phi$} (Eb)
        (Ea) edge [dotted, bend right = 25] (E)
        (U) edge [phantom] node [pos=0, description, name = U1] {}
                            node [pos=0.5, description, name = U2] {} (Eb)
        (U1) edge [Rightarrow, dashed] node {$\chi$} (U2) 
        (L) edge [phantom] node [pos=0, description, name = L1] {}
                            node [pos=0.75, description, name = L2] {} (Ab)
        (L1) edge [Rightarrow] node {$\phi$} (L2) ;
    \end{tikzpicture}
$$
Here we make use of the fact that $p\colon E\to B$ is a cocartesian fibration in order to lift the whiskered 2-cell $\phi p_a$ to a cocartesian 2-cell $\chi$. Its codomain 1-cell may then be factored through $E_b$, using the pullback property of the front square, to give a 1-cell $E_{\phi}\colon E_a\to E_b$ over $A$ as required. Standard (essential) uniqueness properties of cocartesian lifts may now be deployed to provide canonical isomorphisms $E_{\psi\cdot\phi}\cong E_{\psi}\circ E_{\phi}$ and $E_{\id_a}\cong\id_{E_a}$ and to prove that these satisfy required coherence conditions.

It is this 2-categorical comprehension construction that motivates the key construction of our paper.

### Comprehension and 2-fibrations

In passing, we might quickly observe that the 2-categorical comprehension construction may be regarded as being but one aspect of the theory of [2-fibrations](https://ncatlab.org/nlab/show/n-fibration). Specifically the totality of all cocartesian fibrations and cartesian functors between them in $\cK$ is a 2-category whose codomain projection $\coCart(\cK)\to\cK$ is a cartesian 2-fibration, it is indeed the archetypal such gadget. Under this interpretation, the lifting construction used to define the pseudo-functor $\Fun_{\cK}(A,B) \to \cK_{/A}$ is quite simply the typical cartesian 2-cell lifting property characteristic of a 2-fibration.

In an early draft of our paper, our narrative followed just this kind of route. There we showed that the totality of cocartesian fibrations in an ∞-cosmos could be assembled to give the total space of a kind of cartesian fibration of [(weak) 2-complicial sets](https://ncatlab.org/nlab/show/weak+complicial+set). In the end, however, we abandoned this presentation in favour of one that was more explicitly to the point for current purposes. Watch this space, however, because we are currently preparing a paper on the complicial version of this theory which will return to this point of view. For us this has become a key component of our work on foundations of complicial approach to $(\infty,\infty)$-category theory.

## An ∞-categorical comprehension construction

In an [∞-cosmos](https://ncatlab.org/nlab/show/infinity-cosmos) $\cK$, by which we mean a category enriched over quasi-categories that admits a specified class of isofibrations and certain simplicially enriched limits, we may again define $p \colon E \twoheadrightarrow B$ to be a cocartesian fibration representably. That is to say, $p$ is a *cocartesian fibration* if it is an isofibration in the specified class and if
$$
    \Fun_{\cK}(X,p) \colon \Fun_{\cK}(X,E) \to \Fun_{\cK}(X,B)
$$
is a cocartesian fibration of quasi-categories for every ∞-category $X$. Then a direct "homotopy coherent" generalisation of the 2-categorical construction discussed above demonstrates that we define an associated comprehension functor:
$$
    c_{p,A} \colon \mathfrak{C}\Fun_{\cK}(A,B)\to \coCart(\cK)_{/A}.
$$
The image lands in the maximal Kan complex enriched subcategory of the quasi-categorically enriched category of cocartesian fibrations and cartesian functors over $A$, so the comprehension functor transposes to define a map of quasi-categories
$$
    c_{p,A} \colon \Fun_{\cK}(A,B) \to \hN(\coCart(\cK)_{/A})
$$
whose codomain is defined by applying the homotopy coherent nerve.

### Straightening as comprehension

The "straightening" of a cocartesian fibration into a homotopy coherent diagram is certainly one of early highlights in Lurie's account of quasi-category theory. Such functors are intrinsically tricky to construct, since that process embroils us in specifying an infinite hierarchy of homotopy coherent data.

We may deploy the ∞-categorical comprehension to provide a alternative approach to straightening. To that end we work in the ∞-cosmos of quasi-categories $\qCat$ and let $A=1$, then observe that the comprehension functor $c_{p,1}\colon \mathfrak{C}B \to \qCat$ is itself the straightening of $p$. Indeed, it is possible to use the constructions in our paper to extend this variant of unstraightening to give a functor of quasi-categories:
$$
    \hN(\coCart(\cK)_{/B}) \to \Fun(B,Q)
$$
Here $Q$ is the (large) quasi-category constructed by taking the homotopy coherent nerve of (the maximal Kan complex enriched subcategory of) $\qCat$. So the objects of $\Fun(B,Q)$ correspond bijectively to "straight" simplicial functors $\mathfrak{C}B\to\qCat$. We should confess, however, that we do not explicitly pursue the full construction of this straightening functor there.

### Unstraightening as comprehension

In the ∞-categorical context, the Grothendieck construction is christened [unstraightening](https://ncatlab.org/nlab/show/%28infinity%2C1%29-Grothendieck+construction) by Lurie. It is inverse to the straightening construction discussed above.

We may also realise unstraightening as comprehension. To that end we follow Ross Street's lead by taking $Q_{\ast}$ to be a quasi-category of pointed quasi-categories and apply the comprehension construction to the "forget the point" projection $Q_{\ast}\to Q$. The comprehension functor thus derived
$$
    c_{p,A} \colon Fun(A,Q) \to \hN\left(\dCoCart(\cK)_{/A}\right)
$$
defines a quasi-categorical analogue of Lurie's unstraightening construction. In an upcoming paper we use the quasi-categorical variant of Beck's monadicity theorem to prove that this functor is an equivalence. We also extend this result to certain other ∞-cosmoi, such as the ∞-cosmos of (co)cartesian fibrations over a fixed quasi-category.

### Constructing the Yoneda embedding

Applying the comprehension construction to the cocartesian fibration $\cod\colon A^2\to A$ in the slice ∞-cosmos $\cK_{/A}$, we obtain a map
$$
    \yo\colon\Fun_{\cK}(1,A)\to\hN(\Cart(\cK)_{/A})
$$
that carries an element $a \colon 1 \to A$ to the groupoidal cartesian fibration $\dom\colon A\downarrow a \to A$. This provides us with a particularly explicit model of the Yoneda embedding, whose action on hom-spaces is easily computed. In particular, this allows us to easily demonstrate that the Yoneda embedding is fully-faithful and thus that every quasi-category is equivalent to the homotopy coherent nerve of some Kan complex enriched category.

## References
