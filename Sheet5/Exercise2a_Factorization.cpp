#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <ctime>
#include <list>

int findDivisor(int n, int m);
int gcd(int a,int b);
int eulerPhi(int n);
int myRand(int min, int max);

int main(void)
{
    // TEST AREA. FOR SCIENCE!
/*    int n = 1;
    printf("isPrime(%d) = %d\n", n, isPrime(n) );
    return 42;
*/
    srand(time(0));
    // number to test
    int n = 13*7*89*3;
    int tmp;
    //here all found factors will be saved
    std::list<int> factors;
    int prodFacs = 1;
    // repeat until all primefactors are found
    while(prodFacs != n){
        // tmp is the factor we are about to find
        tmp = findDivisor(n, myRand(1,n)*eulerPhi(n));
        if(tmp > 1){
            // check whether tmp should be saved
            bool added = false;
            for(std::list<int>::iterator i = factors.begin(); i != factors.end(); ++i){
                // when tmp is already in the list or is divisible
                // by a factor in the list there is nothing to do
                if(tmp == *i || tmp % *i == 0){
                    added = true;
                    break;
                }
                // if tmp divides a factor, the factor is replaced
                if(*i % tmp == 0){
                    printf("factor %d replaced by %d\n", *i, tmp);
                    prodFacs = prodFacs / (*i);
                    prodFacs = prodFacs*tmp;
                    *i = tmp;
                    added = true;
                    break;
                }
            }
            // add factor to the list when above criteria don't hold
            if(!added){
                factors.push_back(tmp);
                printf("factor added %d\n",tmp);
                prodFacs = prodFacs*tmp;
            }
        }
    }
    return 0;
}

/*
 * Algorithm 1 §4 from the lecture
 */
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
 * random number between 2 and (n-2) = rand() % (max-min+1)+min
 */
int myRand(int min, int max){
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
