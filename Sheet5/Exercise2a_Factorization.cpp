#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <ctime>

int findDivisor(int n, int m);
int gcd(int a,int b);
int eulerPhi(int n);
int myRand(int min, int max);
bool isInArray(int a, int* arrayi, int length);
bool isPrime(int n);

int main(void)
{
    srand(time(0));
    int n = 13*7*89;
    int tmp;
    int* factors = (int*) malloc(sizeof(int));
    int numFacs = 0;
    int prodFacs = 1;
    while(prodFacs != n){
        tmp = findDivisor(n, myRand(1,n)*eulerPhi(n));
        //printf("tmp = %d\n", tmp);
        if(!isInArray(tmp, factors, numFacs) && tmp > 1 && isPrime(tmp)){
            factors[numFacs] = tmp;
            numFacs++;
            prodFacs = prodFacs*tmp;
            printf("factor added %d\n",tmp);
        }
    }
    return 0;
}

int findDivisor(int n, int m){
    // random number between 2 and (n-2);
    int a = myRand(2, (n-2));
    int k = m;
    int d = gcd(a,n);
    //printf("find divisor: n=%d \tm=%d \ta=%d \td=%d\n", n, m, a, d);
    if(d != 1)
        return d;

    while(1){
        d = gcd(n, pow(a,k)-1);
        if(d == 1){
            //printf("returned in case 1\n");
            return findDivisor(n, m);
        }
        if(d > 1 && d < n){
            //printf("returned in case 2\n");
            return d;
        }
        if(k % 2 == 1){
            //printf("returned in case 3\n");
            return findDivisor(n, m);
        }
        k=k/2;
    }
    return -1; //error value, this case should never happen
}

bool isPrime(int n){

}


bool isInArray(int a, int* array, int length){
    for(int i = 0; i < length; ++i){
        if(array[i] == a){
            return true;
        }
    }
    return false;
}

int myRand(int min, int max){
// random number between 2 and (n-2) = rand() % (max-min+1)+min
    return rand() % (max-min+1) + min;
}

int gcd(int a, int b){
    int c = a % b;
    while (c != 0) {
        a = b;
        b = c;
        c = a % b;
    }
    return b;
}

int eulerPhi(int n){
    int res = 0;
    for(int i = 1; i <= n; i++){
        if(gcd(i, n) == 1){
            res++;
        }
    }
    return res;
}
