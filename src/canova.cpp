#include <Rcpp.h>
#include <R.h>
#include <cstdlib>
#include "Sort.h"

#include <iostream>
#include <string>

using namespace Rcpp;

void random_shuffle(NumericVector::iterator first, NumericVector::iterator last){
  int n = last - first;
  NumericVector temp(n);
  NumericVector::iterator front = first;

  for(int i = 0;front != last; ++front){
    temp[i] = *front;
    ++i;
  }
  front = first;

  std::vector<int> index(n);
  int x = 0;
  for(int i=0; i<n; ++i){
    bool flag = true;
     while(flag){
       x = runif(1, 0, n)[0];
       flag = false;
       for(int j=0; j<i; ++j){
         if(index[j] == x){
           flag = true;
           break;
         }
       }
     }
     index[i] = x;
  }

  int i = 0;
  for(; front != last; ++front){
    *front = temp[index[i]];
    ++i;
  }

}

//when x ties, their corresponding y are shuffled
void shuffleY(NumericVector& x, NumericVector& y){
  int n = x.length();

  int curId = 0;
  for(int i=0; i<n; ++i){
    if(x.at(i) != x.at(curId)){
      for(int j=curId; j<i; ++j){
        random_shuffle(y.begin() + curId, y.begin() + j+1);
      }

      curId = i;
    }
  }

}


double sum(std::vector<double>& vec){
  double result = 0;
  for(int i=0; i<(int)vec.size(); ++i)
    result += vec.at(i);
  return result;
}

int max(int a, int b){
  return a>b? a:b;
}

// [[Rcpp::export]]
double W_ties(NumericVector x, NumericVector y, int k){
  if(k < 2) return 0;
  int n = x.length();
  SortByX(x,y);

  std::vector<double> result(100);
  for(int m=0; m<100; ++m){
    shuffleY(x,y);
    for(int i=1; i<n; ++i)
      for(int j=max(i-k+1, 0); j<i; ++j)
       result.at(m) += pow(y.at(j) - y.at(i), 2);

  }

  return sum(result)/100;
}

// [[Rcpp::export]]
double W_no_ties(NumericVector x, NumericVector y, int k){
  if(k < 2) return 0;
  int n = x.length();
  SortByX(x,y);

  double W = 0;

  for(int i=1; i<n; ++i)
    for(int j=max(i-k+1, 0); j<i; ++j)
      W += pow(y.at(j) - y.at(i), 2);

  return W;
}

// [[Rcpp::export]]
double MC_p(NumericVector x, NumericVector y, double W, int sn, int k){
  int n = x.length();
  int count = 0;
  for(int m=0; m<sn; ++m){

    random_shuffle(y.begin(), y.end());

    double randW = 0;

    for(int i=1; i<n; ++i)
      for(int j=max(i-k+1, 0); j<i; ++j)
        randW += pow(y.at(j) - y.at(i), 2);

    if(W >= randW)
       ++count;
  }
  return (double)count/sn;
}

