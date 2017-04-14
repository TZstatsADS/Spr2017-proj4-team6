# Project 4: Who Is Who -- Entity Resolution

### [Project Description](doc/project4_desc.md)

Term: Spring 2017

+ Team #6
+ Project title: Entity Resolution Algorithms Evaluation
+ Team members
	+ team member: Zeyu Gan
	+ team member: Virgile Mison
	+ team member: Galen Simmons
	+ team member: Qingyuan Zhang

+ Project summary:

Entity Resolution refers to the process of identifying multiple references to the same object and distinguishing them from mentions of different objects.
For example, entity resolution may operate on natural language text; A special case of entity resolution, author name disambiguation, operates primarily on metadata about authors and articles.

For this project, our team reproduced works in [paper 3](doc/paper/3-Han(2005).pdf) and [paper 6](doc/paper/6-Zhang(2007).pdf), where
paper 3 deployed k-way spectral clustering method to solve name disambiguation in author citation, paper 6 developed a constraint-based objective function
and applied EM algorithm to solve for name disambiguation. Due to limitation of data source we had, for paper 6 we only implemented the objective function with
two constraints c_2 and c_6 as defined by the author.

**Contribution statement**: ([default](doc/a_note_on_contributions.md))

+ Zeyu Gan contributed on the implementation of EM algorithm, differential function,objective function and c_6 constraints in paper 6.
+ Virgile Mison contributed for paper 3 on the replication of spectral clustering algorithm with K-means, the implementation of K-way spectral clustering with Pivoted QR Decomposition, updated `Main.Rmd` for LaTeX explanations on spectral clustering, and on paper 6 with improving performance of EM algorithm by optimizing code on differential function, distance function, objective function and EM.
+ Galen Simmons wrote the `feature_extraction.R`, `run_studies.R` , and `test_feature_extraction.Rmd` scripts, added the `mcc` calculation to the `evaluation_measures.R`, ran the completed algorithms on all the datasets, and wrote most of the text in the `main.Rmd` file
+ Qingyuan Zhang contributed on the data cleaning part used for both paper 3 and paper 6, implementation of c_2 and c_6 constraint functions, the distance function, objective function as defined in paper 6, assembled codes together and updated `main.Rmd` file related to reproducing works in paper 6.

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is organized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a `README` file.
