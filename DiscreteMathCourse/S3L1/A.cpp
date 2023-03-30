#include <cstdio>
#include <vector>
#include <deque>
#include <algorithm>
 
std::vector< std::vector< int > > table;
std::deque< int > q;
int n;
char u;
 
void solve()
{
    for (int i = 1; i < n + 1; i++)
    {
        q.push_back(i);
    }
 
    for (int i = 0; i < n * (n - 1); i++)
    {
        if ( !(table[q[0]][q[1]] ))
        {
            int r = 2;
            while (!(table[q[0]][q[r]] && table[q[1]][q[r + 1]]))
            {
                r++;
            }
 
            int k = 0;
            while (1 + k < r - k)
            {
                std::swap(q[1 + k], q[r - k]);
                k++;
            }
        }
 
        q.push_back(q.front());
        q.pop_front();
    }
}
 
 
int main()
{
    scanf("%i ", &n);
    table = std::vector< std::vector< int > >(n + 1, std::vector< int >(n + 1));
 
    for (int i = 2; i < n + 1; i++)
    {
        for (int k = 1; k < i; k++)
        {
            scanf("%c", &u);
            if (u == '1')
            {
                table[i][k] = 1;
                table[k][i] = 1;
            }
        }
        scanf("%c", &u);
 
    }
 
    solve();
 
    for (const int& i : q)
    {
        printf("%i ", i);
    }
 
    // DEBUG OUTPUT
//    for (int i = 1; i < n + 1; i++)
//    {
//        printf("%i: ", i);
//        for (const auto &child: childs[i])
//        {
//            printf("%i ", child);
//        }
//        printf("\n");
//    }
 
}