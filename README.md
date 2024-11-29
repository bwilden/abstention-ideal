# Ideal Point Estimation with 99% Missing Data


<span id="eq-irt">$$
\begin{aligned}
  \text{Pr}(y_{ij} = \text{Support}) &= \Phi(\gamma_j\theta_i + \xi_j)
\end{aligned}
 \qquad(1)$$</span>

<div id="fig-irt">

![](README_files/figure-commonmark/fig-irt-1.png)

Figure 1: Binary IRT Model

</div>

<span id="eq-irt-tau">$$
\begin{aligned}
   \text{Pr}(y_{ij} = \text{Support}) &= \Phi(\gamma_j\theta_i + \xi_j - \tau_i) \\
   \text{Pr}(y_{ij} = \text{Abstain}) &= \Phi(\tau_i - (\gamma_j\theta_i + \xi_j)) - \Phi(-\tau_i - (\gamma_j\theta_i + \xi_j)) \\
   \text{Pr}(y_{ij} = \text{Oppose}) &= 1 - \Phi(\gamma_j\theta_i + \xi_j + \tau_i) 
\end{aligned}
 \qquad(2)$$</span>

<div id="fig-irt-abs">

![](README_files/figure-commonmark/fig-irt-abs-1.png)

Figure 2: Abstention IRT Model

</div>

# Simulation Study

<div class="panel-tabset">

## 1

![](README_files/figure-commonmark/chnk1-1.png)

## 2

![](README_files/figure-commonmark/chnk2-1.png)

## 3

![](README_files/figure-commonmark/chnk3-1.png)

## 4

![](README_files/figure-commonmark/chnk4-1.png)

## 5

![](README_files/figure-commonmark/chnk5-1.png)

</div>

# Replication

![](README_files/figure-commonmark/unnamed-chunk-6-1.png)

![](README_files/figure-commonmark/unnamed-chunk-7-1.png)
