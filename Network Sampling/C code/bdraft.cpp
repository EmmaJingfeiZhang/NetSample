/*
 *  aa.cpp
 *  
 *
 *  Created by Jingfei Zhang on 3/1/11.
 *  Copyright 2011 University of Illinois. All rights reserved.
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#include <R.h>
#include <Rmath.h>

double T(int i, double w[], int start, int end) 
{
	double sum;
	int j, k;
	double tmp;
	
	sum=0;
	for (j=start; j<=end; j++) {
		tmp=1.0;
		for (k=0; k<i; k++) {
			tmp=tmp*w[j];
		}
		sum = sum+tmp;
	}
	return (sum);
}



double R(int k, double w[], int start, int end, int NumRow)
{
	int i, j;
	double *t; 
	double *r, temp;
	
	//t=malloc((NumRow+1)*sizeof(double));
	t = new double[NumRow+1];
	//r=malloc(NumRow*sizeof(double));
	r = new double[NumRow+1];
	
	for (i=1; i<=k; i++) {
		t[i]=T(i, w, start, end);
	}
	
	r[0]=1;
	for (j=1; j<=k; j++) {
		r[j]=0;
		for (i=1; i<=j; i++) {
			if (i%2==0) 
				r[j]=r[j]-t[i]*r[j-i];
			else 
				r[j]=r[j]+t[i]*r[j-i];
		}
		r[j]=r[j]/j;
	}
	temp=r[k];
	
	delete[] t; t = NULL;
	delete[] r; r = NULL;
	
	return (temp);
}

double sum(int x[], int n)
{
	int i;
	int s;
	s=0;
	
	for(i=0; i<n; i++){
		s= s+x[i];
	}
	return (s);
}

double summ(double x[], int n)
{
	int i;
	double s;
	s=0;
	
	for(i=0; i<n; i++){
		s= s+x[i];
	}
	return (s);
}

double ssum(int x[], int n)
{
	int i;
	int s;
	s=0;
	
	for(i=0; i<n; i++){
		s = s+x[i]*x[i];
	}
	return (s);
}

double RandomReal(double low, double high)
{
	double d;
	
	d=(double) rand()/((double) RAND_MAX+1);
	return (low+d*(high-low));
}


int ChooseOneInteger(double weight[], int start, int end, int NumRow)
{
	int i;
	double *totalWeight, temp;
	
	//totalWeight=malloc(NumRow*sizeof(double));
	totalWeight= new double [NumRow];
	
	totalWeight[start]=weight[start];
	for (i=start+1; i<=end; i++) {
		totalWeight[i]=totalWeight[i-1]+weight[i];
	}
	
	temp=RandomReal(0,totalWeight[end]);
	
	i=start;
	while (totalWeight[i]<=temp && i<end) {
		i++;
	}
	
	delete[] totalWeight; totalWeight = NULL;
	
	return (i);
}



extern "C"
{ 
	void Drafting(int *w, int *s, int *e, int *currentRowsum, int *a,int *N, double *probability)
	{
		
		int i,j,NumRow,end,start,n;
		double tmp,prob;
		double r1, r2;
		
		NumRow=*N;
		start=*s;
		end=*e;
		n=*currentRowsum;
		
		double *p;
		double *newP;
		double *currentWeight;
		double *weight;
		
		//p=malloc(NumRow*sizeof(double));
		p=new double[NumRow];
		//newP=malloc(NumRow*sizeof(double));
		newP=new double[NumRow];
		//currentWeight=malloc(NumRow*sizeof(double));
		currentWeight=new double[NumRow];
		//weight=malloc(NumRow*sizeof(double));
		weight=new double[NumRow];
		
		if (n==0) {
			*probability=1.0;
			return;
		}
		
		if (end-start+1==n) {
			for (i=start; i<=end; i++) {
				a[i-start]=i;
			}
			*probability=1.0;
			return;
		}
		
		if (sum(w,NumRow)==n) {
			int k=0;
			for (i=start;i<=end;i++){
				if (w[i]==1) {
					a[k]=i;
					k++;
				}
				
			}
			*probability=1.0;
			return;
		}
		
		
		for (i=start; i<=end; i++) {
			if (w[i]==0) {
				weight[i]=0;
			}
			else {
				weight[i]=w[i]*exp((w[i]*(ssum(w,NumRow)+n))/((sum(w,NumRow)-n)*(sum(w,NumRow)-n))-1);
			}
		}
		
		for (i=start; i<=end; i++) {
			tmp=weight[i];
			weight[i]=weight[end];
			p[i]=tmp*R(n-1, weight, start, end-1, NumRow);
			weight[i]=tmp;
			currentWeight[i]=weight[i];
		}
		
	
		
		a[0]=ChooseOneInteger(p, start, end, NumRow);
		currentWeight[a[0]]=0;
		j=0;
		
		while (n>(j+1)) {
			for (i=start; i<=end; i++) {
				if (currentWeight[i]<=0.00000000001)
					
					newP[i]=0;
				else if (abs(weight[i]-weight[a[j]])<0.0000000001) {
					
					currentWeight[i]=0;
					r1=R(n-j-2, currentWeight, start, end, NumRow);
					currentWeight[i]=weight[i];
					r2=R(n-j-1, currentWeight, start, end, NumRow);
					newP[i]=weight[i]*r1/((n)-j-1)/r2;
				}
				else {
					newP[i]=(weight[a[j]]*p[i]-weight[i]*p[a[j]])/((n)-j-1)/(weight[a[j]]-weight[i])/p[a[j]];
				}
			}
			j++;
			
			
			a[j]=ChooseOneInteger(newP, start, end, NumRow);
			
			currentWeight[a[j]]=0;
			
			for (i=start; i<=end; i++) {
				p[i]=newP[i];
			}
		}
		
		prob=1;
		for (i=0; i<n; i++) {
			prob=prob*weight[a[i]];
		}
		
		prob=prob/R(n, weight, start, end, NumRow);
		
		*probability=prob;
		
		delete[] p; p = NULL;
		delete[] newP; newP = NULL;
		delete[] currentWeight; currentWeight = NULL;
		delete[] weight; weight = NULL;
		
		//free(p);p=NULL;
		//free(newP);newP=NULL;
		//free(currentWeight);currentWeight=NULL;
		//free(weight);weight=NULL;
		
		
		
	}
	
}


