#include <algorithm>
#include <functional>
#include <queue>
#include <cstdio>
#include <vector>
 
std::vector<std::vector<int>> nodes;
std::priority_queue< int, std::vector< int >, std::greater<> > leafes;
std::vector<int> d;
std::vector<int> id;
int n, u, v;
 
 
int main()
{
    scanf("%i ", &n);
    nodes = std::vector<std::vector<int>>(n + 1, std::vector<int>());
    leafes = std::priority_queue< int, std::vector< int >, std::greater<> >();
    d = std::vector<int>(n + 1);
    d[0] = 1000000;
    id = std::vector<int>(n + 1);
 
    for (int i = 0; i < n + 1; i++) { id[i] = i; }
 
    for (int i = 0; i < n - 1; i++)
    {
        scanf("%i %i", &u, &v);
 
        nodes[u].push_back(v);
        nodes[v].push_back(u);
 
        d[u]++;
        d[v]++;
    }
 
    for (int i = 1; i < n + 1; i++)
    {
        if (d[i] == 1)
        {
            leafes.push(i);
        }
    }
 
    for (int i = 0; i < n - 2; i++)
    {
        int leaf = leafes.top();
        leafes.pop();
 
        int parent = nodes[leaf][0];
 
        if (--d[parent] == 1)
        {
            leafes.push(parent);
        }
 
        d[leaf] = 1000000;
        nodes[parent].erase(std::find(nodes[parent].begin(), nodes[parent].end(), leaf));
        printf("%i ", parent);
    }
}