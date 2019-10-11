# Project Sport Analytics 2017

Master Thesis Project 

## Abstract  

Predicting the outcome of sports events is a hard task. We quantify this difficulty with a coefficient that measures the distance between the observed final results of sports leagues and idealized perfectly balanced competitions in terms of skill. This indicates the relative presence of luck and skill. We collected and analyzed all games from 198 sports leagues comprising 1503 seasons from 84 countries of 4 different sports: basketball, soccer, volleyball and handball. We measured the competitiveness by countries and sports. We also identify in each season which teams, if removed from its league, result in a completely random tournament. Surprisingly, not many of them are needed. As another contribution of this paper, we propose a probabilistic graphical model to learn about the teams' skills and to decompose the relative weights of luck and skill in each game. We break down the skill component into factors associated with the teams' characteristics. The model also allows to estimate as 0.36 the probability that an underdog team wins in the NBA league, with a home advantage adding 0.09 to this probability. As shown in the first part of the paper, luck is substantially present even in the most competitive championships, which partially explains why sophisticated and complex feature-based models hardly beat simple models in the task of forecasting sports' outcomes.

## Publications
[1] Aoki, Raquel YS, Renato M. Assunção, and Pedro OS Vaz de Melo. "Medindo o tamanho da caixinha de surpresas em ligas de futebol." SBBD. 2016.  
[2] Aoki, Raquel, Renato M. Assuncao, and Pedro OS Vaz de Melo. "Luck is hard to beat: The difficulty of sports prediction." Proceedings of the 23rd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining. ACM, 2017.

## Results
- kdd2017_presentation slides. You can see the full publication at https://dl.acm.org/citation.cfm?id=3098045 and a promo video at https://www.youtube.com/watch?v=ACHhhOEzWAY&t=1s   
- sbbd2016_paper: Paper from the poster presentation made at the Brazilian Symposium on Databases (SBBD) (Portuguese)  
- pgm2015_coursereport: Hierarchical Bayesian Model to predict the number of goals in soccer leagues. Report presented at the Probabilistic Graphical Models course (Portuguese)

## Data
- model: NBA's player salaries, network effects, matches results between 2012 and 2016 (source www.basketball-reference.com). 
- scale:  results from soccer, handball, basketball and voleyball matches between 2007 and 2016. Data download from  (source ewww.betexplorer.com)

## Source 
Plots and data download code, probabilistic graphical models, c++ for simulations. 

