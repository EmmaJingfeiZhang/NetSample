/*
 *  listCandidate.c
 *  
 *
 *  Created by Jingfei Zhang on 4/22/11.
 *  Copyright 2011 University of Illinois. All rights reserved.
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#include <R.h>
#include <Rmath.h>





int sum(int x[], int n)//good
{
	int i;
	int s;
	s=0;
	
	for(i=0; i<n; i++){
		s= s+x[i];
	}
	return (s);
}

int durfee(int d[], int n) //good
{
	int m=0,durfeeLB=0,durfeeUB=0;
	if(n==1) {
		m=1;
	}
	else {
		durfeeLB=1;
		durfeeUB=n;
		m=floor((n+1)/2);
		while (durfeeLB<durfeeUB-1) {
			if(d[m-1]>=m-1) {
				durfeeLB=m;
			}
			else {
				durfeeUB=m-1;
			}
			m=floor((durfeeLB+durfeeUB)/2);
		}
		if (d[m]>=m) {
			m=m+1;
		}
	}
	return m;
}
void sort(int d[],int n)//good
{
	int temp,i,j;
	for(i=0;i<n;i++) {
		for(j=i+1;j<n;j++)
		{
			if(d[i]<d[j])
			{
				temp=d[i];
				d[i]=d[j];
				d[j]=temp;
			}
		}
	}
	return;
}


int isGraphical(int degree[], int n)
{
	int i,j,k,t,r,m,type=1,partialsums[n];
	int *dsorted;
	
	dsorted=malloc((n+1)*sizeof(int));
	
	for(i=0;i<n;i++) {
		dsorted[i]=degree[i];
	}
	sort(dsorted,n);
	
	int minflips[dsorted[0]+n];
    for(i=0;i<dsorted[0]+n;i++) {
		minflips[i]=0;
	}
	if (sum(dsorted,n)%2>0)
	{
		type=0;
	}
	else if(dsorted[n-1]<0)
	{
		type=0;
	}
	else {
	
	m=durfee(dsorted,n);
		
	if(dsorted[n-1]>0) {
		for (j=0;j<dsorted[n-1];j++) {
			minflips[j]=n;
		}
	}
	for ( i=0;i<n-1;i++) { 
		if( dsorted[n-i-2]>dsorted[n-i-1]) {
			for (j=dsorted[n-i-1]+1;j<=dsorted[n-i-2];j++) {
				minflips[j-1]=n-i-1;
			}
		}
	}
	partialsums[0]=dsorted[0];
	for (i=1;i<n;i++) {
		partialsums[i]=partialsums[i-1]+dsorted[i];
	}
	for (k=0;k<m;k++)
	{
		if((minflips[k]-k-1)>0) {
			t=minflips[k]-k-1;
		}
		else {
			t=0;
		}
		if((partialsums[n-1]-partialsums[k+t])>0) {
			r=partialsums[n-1]-partialsums[k+t];
		}
		else {
			r=0;
		}
		if (partialsums[k]>(k+1)*k+(k+1)*t+r) {
			type=0;
		}
	}
	}
	free(dsorted);
	return type;
}


int isEdge(int x[2],int edge[][2],int row)
{
	int i,type=0,m=row;
	int edges[m+1][2];
	if(m==0) {
		type=0;
	}
	else {
	for(i=0;i<m;i++) {
		edges[i][0]=edge[i][0];
		edges[i][1]=edge[i][1];
	}
	edges[m][1]=0;
	edges[m][0]=0;
	
	i=0;		
	while((i<m)&&(edges[i][0]!=x[0]||edges[i][1]!=x[1])&&(edges[i][0]!=x[1]||edges[i][1]!=x[0])) {
		i=i+1;
	}
	if(i<m) {
		type=1;
	}
	}
	
	return type;
}

void sort_ind(int d[],int n,int ind[])
{
	int dd[n][2],i,j,tmp1,tmp2;
	for( i=0;i<n;i++) {
		dd[i][0]=d[i];
		dd[i][1]=ind[i];
	}
	for(i=0;i<n;i++) {
		for(j=i+1;j<n;j++)
		{
			if(dd[i][0]>=dd[j][0])
			{
				tmp1=dd[i][0];
				tmp2=dd[i][1];
				dd[i][0]=dd[j][0];
				dd[i][1]=dd[j][1];
				dd[j][0]=tmp1;
				dd[j][1]=tmp2;
			}
		}
	}
	for (i=0;i<n;i++) {
		ind[i]=dd[n-i-1][1];
	}
	return;
}

void sort_increase(int d[],int n)//good
{
	int temp,i,j;
	for(i=0;i<n;i++) {
		for(j=i+1;j<n;j++)
		{
			if(d[i]>d[j])
			{
				temp=d[i];
				d[i]=d[j];
				d[j]=temp;
			}
		}
	}
	return;
}
void sort_ind_increase(int d[],int n,int ind[])
{
	int dd[n][2],i,j,tmp1,tmp2;
	for( i=0;i<n;i++) {
		dd[i][0]=d[i];
		dd[i][1]=ind[i];
	}
	for(i=0;i<n;i++) {
		for(j=i+1;j<n;j++)
		{
			if(dd[i][0]>dd[j][0])
			{
				tmp1=dd[i][0];
				tmp2=dd[i][1];
				dd[i][0]=dd[j][0];
				dd[i][1]=dd[j][1];
				dd[j][0]=tmp1;
				dd[j][1]=tmp2;
			}
		}
	}
	for (i=0;i<n;i++) {
		ind[i]=dd[i][1];
	}
	return;
}



int identifyNode(int d[],int n, int x) {
	int i=0;
	while(i<n&&d[i]!=x) {
		i++;
	}
	return i;
}



void listCandidates(int *candidate,int *hub, int *d, int *F,int *C,int *nF, int *nC,int *nn)

{
	int sizeofF=*nF;
	int sizeofC=*nC;
	int n=*nn;
	int i,j,k,x[2],vertex=*hub;
	int fail=0,d_vertex=d[vertex],d_n[n],d_temp[n],d_star[n],f[sizeofF][2],newf[sizeofF+d_vertex-1][2],d_star_C[sizeofC];
	int nodes[n+1],ind[n],newF[sizeofF+d_vertex-1],d_star_star[n],ind_C[sizeofC];
	int tmp=0,temp_nodes[n],temp_C[sizeofC];
	int leftmost_ad_set[d_vertex],num=0;
	

	
	
	
	for(i=0;i<n;i++) {
		d_n[i]=d[i];
	}
	for(i=0;i<=n;i++) {
		nodes[i]=i;
	}
	
	for(i=0;i<n;i++) {
		ind[i]=i;
	}
	
	for(i=0;i<sizeofC;i++) {
		candidate[i]=n;//works as 0
	}
	for(i=0;i<sizeofF;i++) {
		f[i][0]=F[i];
		f[i][1]=0;
	}
	if(F[0]==n) {      //means the forbidden set is empty
		for (i=0;i<sizeofC;i++) {
			for(j=0;j<n;j++) {
				d_n[j]=d[j];
			}
			d_n[C[i]]=d[C[i]]-1;
			d_n[vertex]=d[vertex]-1;
			
			if (d[C[i]]>0 && (isGraphical(d_n,n)==1)){
				candidate[num]=C[i];
				num++;
			}
		}
		


	}
	
	else {
		
		for(i=0;i<n;i++) {
			d_star[i]=d[i];
		} 
		
		for(i=0;i<n;i++) {
			ind[i]=i;
		}
		
		sort_ind_increase(d_n,n,ind);
		
		j=0;
		for(i=0;i<d_vertex;i++) {
			x[0]=ind[n-j-1];
			x[1]=0;
			while(isEdge(x,f,sizeofF)!=0|| ind[n-j-1]==vertex) 
			{
				j++;
				x[0]=ind[n-j-1];
				x[1]=0;
			}
			leftmost_ad_set[i]=ind[n-j-1];
			j++;
		}
		for(i=0;i<n;i++) {
			d_temp[i]=d_star[i];
		}
		for(i=0;i<d_vertex-1;i++) {
			d_star[leftmost_ad_set[i]]=d_temp[leftmost_ad_set[i]]-1;
		}
		d_star[vertex]=1;
		
		for(i=0;i<n;i++) {
			d_temp[i]=d_star[i];
		}
		for(i=0;i<sizeofF;i++) {
			newF[i]=F[i];
		}
		for(i=sizeofF;i<sizeofF+d_vertex-1;i++) {
			newF[i]=leftmost_ad_set[i-sizeofF];
		}
		sort_increase(newF,sizeofF+d_vertex-1);
		
		for(i=0;i<sizeofF+d_vertex-1;i++) {
			newf[i][0]=newF[i];
			newf[i][1]=0;
		}
		for (i=0;i<sizeofF+d_vertex-1;i++) {
			nodes[i]=newF[i];
		}
		
		j=0;
		for (i=sizeofF+d_vertex-1;i<n;i++) {
			x[0]=j;
			x[1]=0;
			while(isEdge(x,newf,sizeofF+d_vertex-1)!=0) {
				j=j+1;
				x[0]=j;
				x[1]=0;				
			}
			nodes[i]=j;
			j++;
		}
		for (i=0;i<n;i++) {
			d_star[i]=d_temp[nodes[i]];
		}
		for(i=0;i<n;i++) {
			ind[i]=i;
		}
		sort_ind(d_star,n,ind);
		
		for (i=0;i<n;i++) {
			temp_nodes[i]=nodes[i];
		}
		for (i=0;i<n;i++) {
			nodes[i]=temp_nodes[ind[i]];
		}
		
		i=identifyNode(nodes,n+1,vertex);
		j=i;
		
		if(i<n-1) {
			while ((i<n-1) && d_star[nodes[i+1]]==1) {
				i++;
			}
			tmp=nodes[j];
			nodes[j]=nodes[i];
			nodes[i]=tmp;
		}
	
		for(i=0;i<n;i++) {
			d_star[i]=d_temp[nodes[i]];
		}
		
		for(i=0;i<sizeofC;i++) {
			d_star_C[i]=d_star[identifyNode(nodes,n+1,C[i])];
		}
		for(i=0;i<sizeofC;i++) {
			ind_C[i]=i;
		}
		sort_ind(d_star_C,sizeofC,ind_C);
		
		for (i=0;i<sizeofC;i++) {
			temp_C[i]=C[i];
		}
		for (i=0;i<sizeofC;i++) {
			C[i]=temp_C[ind_C[i]];
		}
		
		k=sizeofC-1;
		x[0]=C[k];
		x[1]=0;
		while (k>=0 &&fail==0) {
			for(i=0;i<n;i++) {
				d_star_star[i]=d_star[i];
			}
			j=identifyNode(nodes,n+1,vertex);
			d_star_star[j]=d_star_star[j]-1;
			j=identifyNode(nodes,n+1,C[k]);
			d_star_star[j]=d_star_star[j]-1;
			if(isGraphical(d_star_star,n)==1 && isEdge(x,newf,sizeofF+d_vertex-1)==0 )
			{
				fail=d_star[j];
			}
			else {
				fail=0;
			}
			k=k-1;
			if(k>=0) {
			x[0]=C[k];
			x[1]=0;
			}
		}
		
	
		
		for (i=0;i<sizeofC;i++) {
			x[0]=0;
			x[1]=C[i];
			if (d[C[i]] >= fail && isEdge(x,f,sizeofF)==0)  {
				candidate[num]=C[i];
				num++;
			}
		}
					
	


	
		
		//candiates are the numbers that are smaller than n
}
}


