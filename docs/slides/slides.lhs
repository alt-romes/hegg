\documentclass[aspectratio=169,dvipsnames]{beamer}

\usepackage{hyperref}
% \usepackage{svg}

\usetheme{Copenhagen}
%\usetheme{Singapore}
\usecolortheme{spruce}
\setbeamertemplate{navigation symbols}{}
\setbeamertemplate{itemize items}[circle]
\setbeamercovered{transparent}
\setbeamertemplate{footline}[frame number]

%include polycode.fmt
%subst keyword a = "\textcolor{BlueViolet}{\textbf{" a "}}"
\newcommand{\myFor}[2]{\For{$#1$}{$#2$}}
\newcommand{\id}[1]{\textsf{\textsl{#1}}}
\renewcommand{\Varid}[1]{\textcolor{Sepia}{\id{#1}}}
\renewcommand{\Conid}[1]{\textcolor{OliveGreen}{\id{#1}}}

\title{E-Graphs and Equality Saturation (in Haskell)}
\author{Rodrigo Mesquita}

\begin{document}

\frame[plain]{\titlepage}

\begin{frame}{E-graphs and eq-sat are cool}
\only<1>{
Published applications of equality saturation and e-graphs
\begin{itemize}
  \item \href{https://asplos-conference.org/abstracts/asplos21-paper142-extended_abstract.pdf}{Automatic vectorization of digital signal processing code}
  \item \href{https://arxiv.org/abs/2101.01332}{Tensor graph superoptimization}
  \item \href{https://github.com/JuliaSymbolics/Metatheory.jl}{Algebraic metaprogramming and symbolic computation}
  % \item \href{https://dl.acm.org/doi/10.1145/3485496}{Rewrite rule inference}
  \item \href{https://egraphs-good.github.io/}{and more...}
\end{itemize}
}

\only<2>{
And perhaps in the near future...
\begin{itemize}
  \item A symbolic mathematics library in Haskell
  \item \href{https://gitlab.haskell.org/ghc/ghc/-/issues/19272}{Pattern-match coverage checking in GHC}
  \item $<$Insert your idea here$>$
\end{itemize}
}
\end{frame}

\begin{frame}{What are e-graphs?}

\begin{definition}[E-graphs]
\begin{itemize}
\item<1> An e-graph is a data structure that
  \begin{itemize}
  \item compactly represents \emph{equivalence classes} of expressions
  \item while maintaining a key invariant: the equivalence relation is closed under \emph{congruence}\footnote{Intuitively, $a \equiv b \Rightarrow f(a) \equiv f(b)$}
  \end{itemize}

\item<2-> Concretely, an e-graph is a set of equivalence classes.
  \begin{itemize}
  \item<3-> Each e-class is a set of equivalent e-nodes
  \item<4-> An e-node represents a expression from a given language (e.g. $x*1$)
  \item<5-> An e-node is a function symbol paired with a list of children e-classes $f(c_1,c_2,\dots)$
  \end{itemize}
\end{itemize}
\end{definition}

\end{frame}

\begin{frame}{E-graphs represent congruence relations over expressions}
\begin{example}[E-graph]

% NOTES: Each e-class only has 1 e-node
%        (*) pointing to two other e-classes is an e-node.

\only<1-2>{
\begin{itemize}
\item<1-> This e-graph represents $(a*2)/2$
\item<2-> In fact, it represents $a$, $2$ and $a*2$ too
\end{itemize}
\center
\includegraphics[width=0.5\textwidth,height=0.5\textheight,keepaspectratio]{example-drawings/egraphs-1-crop.pdf}
}

\only<3-5>{
\begin{itemize}
% represent and merge are e-graph primitives
\item<3-> Now we \textbf{represent} the expression $a\ll1$ in a new e-class
\item<4-> And \textbf{merge} that e-class with the e-class representing $a*2$
  \begin{itemize}
    \item<5> We merge these two classes because they represent equivalent expressions
    \item<5> By congruence, $(a*2)/2$ is now seen as equivalent to $(a\ll1)/2$
  \end{itemize}
\end{itemize}
\center
\only<3>{
\includegraphics[width=0.5\textwidth,height=0.5\textheight,keepaspectratio]{example-drawings/egraphs-2-crop.pdf}
}
\only<4->{
\includegraphics[width=0.5\textwidth,height=0.5\textheight,keepaspectratio]{example-drawings/egraphs-3-crop.pdf}
}
}

\end{example}
\end{frame}

\begin{frame}{What about equality saturation?}
\begin{definition}[Equality Saturation]
\begin{itemize}
\item<1-> Eq-sat is a technique that leverages e-graphs to implement term rewriting systems
\item<2-> In short, we
%that are used for rewrite-driven compiler optimizations, program synthesizers,
%and all the other cool applications we saw in the beginning
  \begin{itemize}
  \item<2-> Represent an expression in the e-graph
  \item<3-> Repeatedly apply pattern-based rewrites non-destructively until
  saturation (rewrite rules like $x*1 \to x$ where $x$ matches any expression)
  \item<4> Extract the best expression equivalent to the input
  \end{itemize}
\end{itemize}
\end{definition}
\end{frame}

\begin{frame}
\begin{example}[Rewriting $(a*2)/2$]
Rewrite rules:
\begin{itemize}
\item $a*2 = a\ll1$
\item $(a*b)/c = a*(b/c)$
\item $x/x = 1$
\item $x*1 = x$
\end{itemize}
\center
\only<1>{
\includegraphics[width=0.5\textwidth,height=0.5\textheight,keepaspectratio]{example-drawings/egraphs-1-crop.pdf}
}
\only<2>{
\includegraphics[width=0.5\textwidth,height=0.5\textheight,keepaspectratio]{example-drawings/egraphs-3-crop.pdf}
}
\only<3>{
\includegraphics[width=0.5\textwidth,height=0.5\textheight,keepaspectratio]{example-drawings/egraphs-4-crop.pdf}
}
\only<4>{
\includegraphics[width=0.5\textwidth,height=0.5\textheight,keepaspectratio]{example-drawings/egraphs-5-crop.pdf}
}
\end{example}
\end{frame}

\begin{frame}{Fast and extensible equality saturation\footnote{\href{https://arxiv.org/pdf/2004.03082.pdf}{egg: Fast and Extensible Equality Saturation (POPL 2021)}}}
\begin{itemize}
\item Invariant restoration technique called \emph{rebuilding}
\item Mechanism called \emph{e-class analysis} to integrate domain specific analysis into the e-graph
\end{itemize}
\end{frame}

\begin{frame}{Rebuilding}
The key idea: defer e-graph invariant restoration to the call of \emph{rebuild}
\begin{itemize}
\item<1-> Under certain workloads (automated theorem provers such as Z3), e-graph
invariants are restored after every operation
\item<2> For equality saturation, we can greatly benefit from deferred invariant maintenance:
  \begin{itemize}
  \item Search for all pattern matches
  \item Apply all rewrites that matched\footnote{We can't search patterns while applying rewrites -- the invariants aren't maintained w/out \textbf{rebuild}}
  \item Restore invariants\footnote{Potentially saves work because we deduplicate the \emph{worklist}}
  \end{itemize}
\end{itemize}
\end{frame}

\begin{frame}{E-class Analysis}
The key idea: attach data to each e-class and use it to modify the e-graph
\begin{itemize}
\item<1-> Purely syntactic rewrites are insufficient in certain applications, in which we need domain knowledge (e.g. constant folding)
\item<2-> Previously, this knowledge was added by modifying the e-graph with ad-hoc passes
\item<3> E-class analysis allows the expression of analysis over the e-graph
\begin{itemize}
\item Each e-class has data from a semilattice
\item Merging e-classes merges the data
\item We can use the data to modify the e-graph
\item (See interface of Analysis)
\end{itemize}
\end{itemize}
\end{frame}

\begin{frame}{E-graphs in hegg (Demo)}

\begin{alertblock}{Remark}
Follow through with the documentation: \href{https://hackage.haskell.org/package/hegg-0.3.0.0/docs/Data-Equality-Graph.html}{Data.Equality.Graph}
\end{alertblock}

\begin{itemize}
\item Why are expressions in their functorial form?
\end{itemize}

\center
\begin{code}
data SymExpr a
  = Const Double
  | Symbol String
  | a :+: a
  | a :*: a
  | a :/: a
  deriving (Functor, Foldable, Traversable)
\end{code}
\end{frame}

% \begin{frame}{E-graphs represent congruence relations over expressions}
% 
% % \begin{definition}[Congruence relation]
% % \center
% % Intuitively, $a \equiv b \Rightarrow f(a) \equiv f(b)$
% % \end{definition}
% 
% \only<1>{
% 
% \begin{example}[E-graph]
% \begin{itemize}
% \item Initial e-graph represents $(a*2)/2$
% \end{itemize}
% \center
% \includesvg[scale=0.5]{example-1.1}
% \end{example}
% }
% 
% \only<2>{
% \begin{example}[E-graph]
% \begin{itemize}
% \item Apply rewrite\footnote{NB: E-graphs are unknown to the concept of rewriting. Rewriting is an external operation defined in terms of e-graph primitives} $x*2 \to x \ll 1$
% \end{itemize}
% \center
% \includesvg[scale=0.5]{example-1.2}
% \end{example}
% }
% 
% \only<3>{
% \begin{example}[E-graph]
% \begin{itemize}
% \item Apply rewrite $(x*y)/z \to x*(y/z)$
% \end{itemize}
% \center
% \includesvg[scale=0.5]{example-1.3}
% \end{example}
% }
% 
% \only<4>{
% \begin{example}[E-graph]
% \begin{itemize}
% \item Apply rewrites $x/x \to 1$ and $x*1 \to x$
% \end{itemize}
% \center
% \includesvg[scale=0.5]{example-1.4}
% \end{example}
% }
% 
% \end{frame}

\begin{frame}{References}
\begin{itemize}
\item \href{https://arxiv.org/pdf/2004.03082.pdf}{egg: Fast and Extensible Equality Saturation}
\item \href{https://arxiv.org/pdf/2108.02290.pdf}{Relational E-matching}
\end{itemize}
\end{frame}


\end{document}
