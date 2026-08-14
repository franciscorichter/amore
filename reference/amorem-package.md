# amorem: Augmented Modelling of Relational Events

An integrated workflow for relational event models, in which timestamped
interactions among actors arrive in continuous time and the event rate
depends on the history of the network itself. Event streams are
simulated by an exact Gillespie kernel or an approximate tau-leap
kernel; non-events are drawn by case-control sampling in the manner of
Lerner and Lomi (2020)
[doi:10.1017/nws.2019.57](https://doi.org/10.1017/nws.2019.57) ; the
timing- and closure-based endogenous statistics of Juozaitiene and Wit
(2024)
[doi:10.1093/jrsssa/qnae132](https://doi.org/10.1093/jrsssa/qnae132) are
computed from that history; and one front-end fits the resulting
case-control data by conditional-logistic partial likelihood, by
penalised regression on its degenerate one-control binomial form (which
admits smooth, time-varying and actor random effects), or by stochastic
gradient descent, including the additive-spline construction of
Filippi-Mazzola and Wit (2024)
[doi:10.1093/jrsssc/qlae023](https://doi.org/10.1093/jrsssc/qlae023) .
Martingale-residual goodness-of-fit diagnostics are provided for the
linear fit, together with simulation and statistic computation for
relational hyper-events, and a collection of documented event logs from
published interaction networks.

## See also

Useful links:

- <https://franciscorichter.github.io/amorem/>

- <https://github.com/franciscorichter/amorem>

- Report bugs at <https://github.com/franciscorichter/amorem/issues>

## Author

**Maintainer**: Francisco Richter <richtf@usi.ch> \[copyright holder\]

Authors:

- Martina Boschi

- Ernst C. Wit

- Melania Lembo
