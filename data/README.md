This folder contains a synthetic dataset (`realSim_data.csv`) for Phase 2 of the Adaptive Strategies for Preventing and Treating Lapses of Retention in HIV Care (ADAPT-R) trial. 

In ADAPT-R Phase 2, participants who experienced a lapse in care after receiving Phase 1 treatment were re-randomized to either Navigator (peer navigation) or SMS+CCT (SMS appointment reminders plus conditional cash transfers).

We use `realSim_data.csv` as the data-generating basis for the simulations. Each row corresponds to one participant. 
Column `ID` is a unique identifier for each participant.

Two Phase-1 baseline covariates obtained in the original trial are included in the conditioning set of conditional average treatment effects for Phase 2 treatment:
- `W1`, the initial Phase 1 treatment arm (1 = SMS, 2 = standard of care, 3 = CCT);
- `W2`, the number of days from Phase 1 enrollment to the participant’s first lapse in care.

Phase 2 treatment arm `A` is defined as: 1 = Navigator, 0 = SMS+CCT.

Outcomes are the proportion of time in care over cumulative follow-up windows starting at the initiation of Phase 2 treatment. 
Specifically, we discretize the study timeline into 50-day stages and define outcomes as the proportion of time in care during which a participant is engaged in across five windows, from the first day of receiving the re-engagement strategy to five specific time points: 50, 100, 150, 200, and 250 days following the initiation of the Phase 2 treatment. 
These outcomes are denoted by `Y_1`, `Y_2`, `Y_3`, `Y_4`, and `Y_5`, respectively, where `Y_5` (the proportion of time in care in 250 days) is the primary outcome, and `Y_1` through `Y_4` are surrogate outcomes. 
`realSim_data.csv` includes simulated counterfactual outcomes under Phase 2 treatment of Navigator and SMS+CCT, obtained from generalized linear models fitted to the original trial (see `code/utils/realSim_prep.R`). 
For each outcome index `k = 1,...,5`, the counterfactual outcomes are provided in columns `EYk_a`. For example:
- `EY1_1` is the expected value of `Y_1` if assigned Navigator, while `EY0_1` is the expected value of `Y_1` if assigned SMS+CCT; 
- `EY1_5` is the expected value of `Y_5` if assigned Navigator, while `EY0_5` is the expected value of `Y_5` if assigned SMS+CCT.

Column `ti` is the 50-day allocation stage of enrollment within Phase 2 trial. For example:
- `ti` indicates that the participant was enrolled and allocated with Phase 2 treatment at the `ti`-th 50-day stage since the Phase 2 treatment allocation of the first enrollee.
