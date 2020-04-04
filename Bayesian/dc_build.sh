sudo docker run -d \
	-p 8787:8787\
	-e PASSWORD=a\
	-v "$(pwd)":/home/rstudio\
	mavelli/rocker-bayesian
