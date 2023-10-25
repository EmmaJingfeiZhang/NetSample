dyn.load("try.so") # load in the executable file
dyn.load("R.so")
dyn.load("bdraft.so")

Drafting<-function(vertex,degrees,edges,C)
{
	a<-c()
	currentRowsum<-degrees[vertex]
	n<-length(C)
	nd<-length(degrees)
	weight<-rep(0,nd)
	prob=1
	
	if (currentRowsum==0) {
		prob<-1.0
		degrees<-0
	}
	else if(sum(degrees)==2*currentRowsum){
		prob<-1/factorial(currentRowsum)
		degrees<-0
		
	}
	
	else {
		for (i in C) {
			if (degrees[i]==0) {
				weight[i]=0
			}
			else {
				weight[i]=degrees[i]*exp((degrees[i]*(sum(degrees[-vertex]^2)+currentRowsum))/((sum(degrees)-2*currentRowsum)*(sum(degrees)-2*currentRowsum)))
			}
		}
		
		j<-1
		F<-c()
		
		while(currentRowsum>=j){
			numCand <- 0
			cand<-c()
			
#if(is.null(F)) {
#cand<-c(1:nd)[weight>0]
#numCand<-length(cand)
			
#}
#else 
#if(!is.null(F) && length(F)==0) {
			if(length(F)==0){
				CC<-as.integer(as.vector((C-1)[(C-1)>=0]))
				candidate=as.integer(as.vector(rep(0,length(CC))))
				hub=as.integer(vertex-1)
				dd<-as.integer(as.vector((degrees)))
				FF<-as.integer(nd)
				nF=as.integer(1)
				nC=as.integer(length(CC))
				nn=as.integer(nd)
				result<-.C("listCandidates",candidate=candidate,hub=hub,d=dd,F=FF,C=CC,nF=nF,nC=nC,nn=nn)
				F<-F[-1]
				cand<-sort(result$candidate)
				cand<-cand[cand!=nd]
				cand<-cand+1
				numCand<-length(cand)
				C<-sort(cand)
			}			
			else {
				CC<-as.integer(as.vector((C-1)[(C-1)>=0]))
				candidate=as.integer(as.vector(rep(0,length(CC))))
				hub=as.integer(vertex-1)
				dd<-as.integer(as.vector((degrees)))
				FF<-as.integer(as.vector(F-1))
				nF=as.integer(length(FF))
				nC=as.integer(length(CC))
				nn=as.integer(nd)
				result<-.C("listCandidates",candidate=candidate,hub=hub,d=dd,F=FF,C=CC,nF=nF,nC=nC,nn=nn)
				cand<-sort(result$candidate)
				cand<-cand[cand!=nd]
				cand<-cand+1
				numCand<-length(cand)
				C<-sort(cand)
			}
			if(numCand==1){
				a[j]<-cand
			}
			else {
				p<-c()
				for (i in c(1:numCand)) {
					tmp=weight[cand[i]]
					kk=as.integer(currentRowsum-j)
					w=as.double(as.vector(weight))	  
					can=as.integer(as.vector(cand[-i]-1))
					candNum=as.integer(length(can))
					temp=as.double(1)
                    t<-.C("R",kk=kk,w=w,candidate=can,candNum=candNum,temp=temp)
					p[i]=tmp*(t$temp*(t$temp!=0)+1*(t$temp==0))
				    weight[cand[i]]=tmp
					
				}
				p<-p/sum(p)
				a[j]<- sample(cand,1,replace=FALSE,prob=p)
				prob<-prob*p[cand==a[j]];
			}
			
            edges<-rbind(edges,c(vertex,a[j]))
			degrees[vertex]<-degrees[vertex]-1
			degrees[a[j]]<-degrees[a[j]]-1
			F<-c(F,a[j])
			F<-F[degrees[F]>0]
			j<-j+1
			
		}
		
		
	}
    return(list(probability=prob,edges=edges,a=a,degrees=degrees))
#return(list(probability=prob,a=a,degrees=degrees))
	
}

durfee <- function(d)
{
	n <- length(d)
	if (n==1) return(1)
	durfeeLB <- 1
	durfeeUB <- n
	m <- floor((n + 1)/2)
	while (durfeeLB < durfeeUB - 1) {
		if (d[m] >= m - 1) durfeeLB <- m
		else durfeeUB <- m - 1	
		m <- floor((durfeeLB + durfeeUB)/2)
	}
	if (d[m + 1] >= m) m <- m + 1
	return(m)
}

isGraphical <- function(d)
{
	if (sum(d) %% 2 >0) return(FALSE)
	dsorted <- sort(d, decreasing=TRUE)
	n <- length(d)
	if (dsorted[n] < 0) return(FALSE)
	m <- durfee(dsorted)
	partialsums <- rep(0,n)
	minflips <- rep(0,n) 
	if (dsorted[n] > 0) {
		for (j in 1:dsorted[n]) {
			minflips[j] <- n
		}
	}
	for (i in 1:(n-1)) {
		if (dsorted[n-i] > dsorted[n-i+1]) {
			for (j in (dsorted[n-i+1]+1):dsorted[n-i]) {
				minflips[j] <- n-i
			}
		}
	}
	partialsums[1] <- dsorted[1]
	for (i in 2:n) {
		partialsums[i] <- partialsums[i-1] + dsorted[i]
	}
	for (k in 1:m) {
		t = max(0,minflips[k] - k)
		if (partialsums[k] > k*(k-1) + k*t + max(c(0,(partialsums[n]-partialsums[k+t])))) {
			return(FALSE)
		}
	}
	return(TRUE)
}

generateRandomGraph<- function(d)
{
	if (!isGraphical(d)) return("error: non-graphical input in generateRandomGraph")
	edges <- matrix(nrow=0, ncol=2)
    degrees <- sort(d,decreasing=T)
#degrees<-d
    l <- length(d)
	nodes<-seq(1:l)
	seqProb <- 1
	i<-1
	while(isGraphical(degrees)  && i<=(l-3))
	{
		index<-c()
		de<-degrees
		w<-as.integer(as.vector(de[-1]))
		s<-as.integer(0)
		e<-as.integer(l-i-1)
		currentRowsum<-as.integer(de[1])
		a<-as.integer(as.vector(rep(0,de[1])))
		N<-as.integer(l-i)
		draft<-.C("Drafting",w=w,s=s,e=e,currentRowsum=currentRowsum,a=a,N=N,probability=as.double(1))
		seqProb<-seqProb*draft$probability
		ku<-draft$a
		index<-nodes[draft$a+2]
		edges_i<-cbind(rep(nodes[1],degrees[1]),index)
        edges<-rbind(edges,edges_i)
		w<-s<-e<-currentRowsum<-a<-N<-probability<-de<-NULL
        draft[["w"]]<-draft[["s"]]<-draft[["e"]]<-draft[["currentRowsum"]]<-draft[["a"]]<-draft[["N"]]<-draft[["probability"]]<-NULL
		degrees[ku+2]<-degrees[ku+2]-1
		ind<-order(degrees[-1],decreasing=T)
		nodes<-nodes[-1][ind]
		degrees<-sort(degrees[-1],decreasing=T)
		i<-i+1
		
	}
	if(!isGraphical(degrees)) seqProb=0
	else{
		res_de<-degrees
		n<-l
		if(sum(res_de^2)==6)
		{
			if(res_de[1]==2)
			edges<-rbind(edges,rbind(c(nodes[1],nodes[2]),c(nodes[1],nodes[3])))
			if(res_de[2]==2)
			edges<-rbind(edges,rbind(c(nodes[2],nodes[1]),c(nodes[2],nodes[3])))
			if(res_de[3]==2)
			edges<-rbind(edges,rbind(c(nodes[3],nodes[1]),c(nodes[3],nodes[2])))
		}
		if(sum(res_de^2)==2)
		{
			if(res_de[1]==0)
			edges<-rbind(edges,c(nodes[2],nodes[3]))
			if(res_de[2]==0)
			edges<-rbind(edges,c(nodes[1],nodes[3]))
			if(res_de[3]==0)
			edges<-rbind(edges,c(nodes[1],nodes[2]))
		}
		if(sum(res_de^2)==12)
		{
			edges<-rbind(edges,rbind(c(nodes[1],nodes[2]),c(nodes[1],nodes[3]),c(nodes[2],nodes[3])))
		}
		
	}
	return(list(probability=seqProb,edges=edges,res_de=degrees,nodes=nodes))
}



generateGraphs<-function(d,trials)
{
	if (!isGraphical(d)) return("error: non-graphical input in generateRandomGraph")
	weights <- rep(0, trials)
    counts <- rep(0, trials)
	for (i in 1:trials) {
		re<-generateRandomGraph(d)
		q <- re$probability
		if(q==0) 
		{
			weights[i]<-0
            counts[i]<-0
		}
		else 
		{
			if(q>0) 
		    {
				weights[i] <- 1/q
			}
		}
	}
	valid<-length(weights[weights>0])
	return(list(degrees=d,weights=weights,estimatedNumRealizations=1/trials * (sum(weights)),validTrial=valid,estimatedStandardError=1/sqrt(trials) * sd(weights)))
}


generateRandomGraph_Non_Stuck<-function(d)
{
	degrees<-d
	n<-length(d)
	edges<-matrix(nrow=0, ncol=2)
	seqProb<-1
	orders<-1
	list<-c(1:n)
	p<-c()
	while(sum(degrees)>0) {
		vertex<-order(degrees,decreasing=TRUE)[1]
		orders<-orders * factorial(degrees[vertex])
		C<-list[list!=vertex]
		if(sum(degrees)==2){
			edges<-rbind(edges,c(1:length(degrees))[degrees==1])
			degrees<-0
		}
		else{
			q<-Drafting(vertex, degrees,edges,C)
			p<-c(p,q$probability)
			seqProb<-seqProb*q$probability
			degrees<-q$degrees
			edges<-q$edges
			list<-list[list!=vertex]
		}
	}
	
	return(list(n=n,degrees=d,sequentialProb=seqProb,p=p,numOrders=orders,weight=1/(seqProb*orders)))
}

generateGraphs_Non_Stuck<-function(d, trials)
{
	if (!isGraphical(d)) return("error: non-graphical input in generateRandomGraph")
	weights <- rep(0, trials)
	d<-sort(d,decreasing=TRUE)
	
	for (i in 1:trials) {
		q <- generateRandomGraph_Non_Stuck(d)
		weights[i] <- q$weight
		
	}
	graphResults=list(degrees=d,weights=weights,estimatedNumRealizations=1/trials *sum(weights), estimatedStandardError=1/sqrt(trials) * sd(weights))
	return(graphResults)
}

####examples########
d<-rep(3,6)
generateGraphs_Non_Stuck(d,1000)
generateGraphs(d,1000)




