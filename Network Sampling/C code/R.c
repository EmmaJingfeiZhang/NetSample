/*
 *  R.c
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



double T(int i, double *w, int *candidate, int candNumber) //good
{
	double sum;
	int j, k;
	double tmp;
	
	sum=0;
	for (j=0; j<candNumber; j++) {
		tmp=1.0;
		for (k=0; k<i; k++) {
			tmp=tmp*w[candidate[j]];
		}
		sum = sum+tmp;
	}
	return (sum);
}



void R(int *kk, double *w, int *candidate, int *candNum, double *temp)//good
{
	int i, j,candNumber=*candNum,k=*kk;
	double *t; 
	double *r;
	*temp=1;
	
	
	t=malloc((candNumber+1)*sizeof(double));
	r=malloc((candNumber+1)*sizeof(double));

	if(k==0) {
		*temp=1;
	}
	//else if(k>candNumber) {
	//	*temp=0;
	//}
	else if(k==1 && candNumber<2) {
		*temp=w[candidate[0]];
	}
	else {
		
		for (i=1; i<=k; i++) {
			t[i]=T(i, w, candidate, candNumber);
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
		*temp=r[k];
	}
	free(t);
	free(r);
}
