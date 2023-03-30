#include <algorithm>
#include <functional>
#include <queue>
#include <cstdio>
#include <vector>
 
int n, u, v;
int* code;
std::vector< std::vector< int > > childs;
std::priority_queue< int, std::vector< int >, std::greater<> > vert;
 
 
int main()
{
    scanf("%i ", &n);
    code = new int[n - 2];
    childs = std::vector< std::vector< int > >(n + 1, std::vector< int >());
    std::vector< int > d(n + 1);
    vert = std::priority_queue< int, std::vector< int >, std::greater<> >();
 
    for (int i = 0; i < n - 2; i++)
    {
        scanf("%i", &u);
        code[i] = u;
        d[u]++;
    }
 
    for (int i = 1; i < n + 1; i++)
    {
        if (!d[i])
        {
            vert.push(i);
        }
    }
 
    for (int i = 0; i < n - 2; i++)
    {
        v = vert.top();
        vert.pop();
 
        u = code[i];
        if (--d[u] == 0)
        {
            vert.push(u);
        }
 
        printf("%i %i\n", u, v);
    }
 
    u = vert.top();
    vert.pop();
 
    v = vert.top();
    vert.pop();
 
    printf("%i %i\n", u, v);
}