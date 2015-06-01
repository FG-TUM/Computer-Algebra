#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <ctime>
#include <list>

int findDivisor(int n, int m);
int gcd(int a,int b);
int eulerPhi(int n);
int myRand(int min, int max);
bool isInIntList(int a, std::list<int> l);
bool isPrime(int n);
void coef(int n, long long* c);

int main(void)
{
    // TEST AREA. FOR SCIENCE!
    int n = 10;
    printf("isPrime(%d) = %d\n", n, isPrime(n) );
    return 42;

    srand(time(0));
    //int n = 13*7*89;
    int tmp;
    std::list<int> factors;
    int prodFacs = 1;
    while(prodFacs != n){
        tmp = findDivisor(n, myRand(1,n)*eulerPhi(n));
        //printf("tmp = %d\n", tmp);
        if(!isInIntList(tmp, factors) && tmp > 1 && isPrime(tmp)){
            factors.push_back(tmp);
            printf("factor added %d\n",tmp);
            prodFacs = prodFacs*tmp;
        }
    }
    return 0;
}

int findDivisor(int n, int m){
    // random number between 2 and (n-2);
    int a = myRand(2, (n-2));
    int k = m;
    int d = gcd(a,n);
    if(d != 1)
        return d;

    while(1){
        d = gcd(n, pow(a,k)-1);
        if(d == 1){
            return findDivisor(n, m);
        }
        if(d > 1 && d < n){
            return d;
        }
        if(k % 2 == 1){
            return findDivisor(n, m);
        }
        k=k/2;
    }
    return -1; //error value, this case should never happen
}

/*
 * calculates the first half of coefficients of the polynomial (x+1)^n (mod n)
 */
void coef(int n, long long* c){

    c[0] = 1;
    for(int k = 1; k <= n/2; ++k){
        //    c[k]=1;
        //c[k] = c[k] * ((double)(n+1-j)/j);
        //c[k] = (c[k-1] * ((n+1-k)%n) %n) / k;
        printf("(n+1-%d)=%d\n", k, (n+1-k));
        printf("(n+1-%d)mod %d = %d\n", k,n, (n+1-k)%n);
        if(c[k-1] == 0)
            c[k] = ((n+1-k)%n) / k;
        else {
            c[k] = (c[k-1] * ((n+1-k)%n) %n) / k;

        }
    }
}

bool isPrime(int n){
    long long a[n/2];
    coef(n, a);
    for(int i = 1; i< (n/2); ++i){
        printf("a[%d] = %lld\n", i, a[i]);
        if(a[i]){
            return false;
        }
    }
    return true;
}


bool isInIntList(int a, std::list<int> l){
    for(std::list<int>::iterator i = l.begin(); i != l.end(); ++i){
        if(*i == a){
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
