# Scripts for "Using sampled network data with the Autologistic Actor Attribute Model"


Social science research increasingly benefits from statistical methods for understanding the structured nature of social life, including for social network data. However, the application of statistical network models within large-scale community research is hindered by too little understanding of the validity of their inferences under realistic data collection conditions, including sampled or missing network data. The autologistic actor attribute model (ALAAM) is a statistical model based on the well-established exponential random graph model (ERGM) for social networks. ALAAMs can be regarded as a social influence model, predicting an individual-level outcome based on the actor’s network ties, concurrent outcomes of his/her network partners, and attributes of the actor and his/her network partners. In particular, an ALAAM can be used to measure contagion effects, that is, the propensity of two actors connected by a social network tie to both have the same value of an attribute. We investigate the effect of using simple random samples and snowball samples of network data on ALAAM parameter inference, and find that parameter inference can still work well even with a nontrivial fraction of missing nodes. However it is safer to take a snowball sample of the network and estimate conditional on the snowball sampling structure.

## Software

The scripts in this repository were imported from the alaam_scripts.tar.gz file availble from https://sites.google.com/site/alexdstivala/home/alaam_sampling.


## References 

Stivala, A., Gallagher, H. C., Rolls, D., Wang, P., & Robins, G. (2020). Using sampled network data with the autologistic actor attribute model. arXiv preprint arXiv:2002.00849 https://arxiv.org/abs/2002.00849
