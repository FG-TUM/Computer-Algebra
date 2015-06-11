#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <ctime>
#include <list>

int findDivisor(int n, int m);
int gcd(int a,int b);
int eulerPhi(int n);
int myRand(int min, int max);

/*
 * solution:
 * n = 48007 = 61*787
 *
 * Output (most of the time):
 * factor added 61
 * factor added 787
 */

int main(void)
{
    // some random seed
    srand(time(0));
    // number to test
    //int n = 13*7*89*3;
    int n = 48007;
    int e = 1493;
    int f = 4517;
    // this will be the candidate for a prime-factor
    int tmp;
    //here all found factors will be saved
    std::list<int> factors;
    // product of all factors found so far
    int prodFacs = 1;
    // repeat until exactly all prime-factors are found
    while(prodFacs != n){
        // tmp is the factor we are about to find.
        // In general the second argument should be a random
        // multiple of phi(n). Here e*f = 1 (mod n) is given.
        //tmp = findDivisor(n, myRand(1,n)*eulerPhi(n));
        tmp = findDivisor(n, e*f);
        if(tmp <= 1){
            continue;
        }
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

/*
 * naive implementation, here only for test purpose and not used
 * in the solution for the exercise
 */
int eulerPhi(int n){
    int res = 0;
    for(int i = 1; i <= n; i++){
        if(gcd(i, n) == 1){
            res++;
        }
    }
    return res;
}
