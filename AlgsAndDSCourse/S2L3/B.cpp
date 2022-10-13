#include <cmath>
#include <vector>
#include <cstdio>

struct Node
{
    std::vector< int > childs;
    int parent = 1;

    Node() = default;
};

Node* nodes;

int n, m, u, v, depth;
int* parent, *d, **dp;

// d[i] - глубина вершины с номером i (примеч.: d[1] = 0, т.к. она корень)
// parent[i] - номер вершины, которая является родителем вершины с
// номером i, (примеч.: parent[0] = parent[1] = 1)

int calc(int &u, int &v)
{
    if (u == v) { return u; }

    if (d[v] > d[u])
    {
        std::swap(u, v);
    }

    int diff = d[u] - d[v];
    if (diff != 0)
    {
        for (int i = depth - 1; i >= 0; i--)
        {
            if (d[dp[u][i]] >= d[v])
            {
                u = dp[u][i];
            }
        }
    }

    if (u == v) { return u; }

    for (int i = depth - 1; i >= 0; i--)
    {
        if (dp[u][i] != dp[v][i])
        {
            u = dp[u][i];
            v = dp[v][i];
        }
    }

    return parent[v];
}

void lcaPreprocess()
{
    parent = new int[n + 1];
    d = new int[n + 1]();
    dp = new int*[n + 1];
    depth = static_cast< int >(log2(n + 1)) + 1;

    for (int i = 1; i < n + 1; i++)
    {
        dp[i] = new int[depth];
    }

    parent[0] = parent[1] = 1;
    for (int i = 2; i < n + 1; i++)
    {
        scanf("%i", &parent[i]);
    }

    for (int i = 2; i < n + 1; i++)
    {
        d[i] = d[parent[i]] + 1;
    }

    for (int i = 1; i < n + 1; i++)
    {
        dp[i][0] = parent[i];
    }

    for (int i = 1; i < depth; i++)
    {
        for (int j = 1; j < n + 1; j++)
        {
            dp[j][i] = dp[dp[j][i - 1]][i - 1];
        }
    }
}



int main()
{
    scanf("%i", &n);
    nodes = new Node[n + 1]();

    lcaPreprocess();

    scanf("%i", &m);

    for (int i = 0; i < m; i++)
    {
        scanf("%i %i", &u, &v);
        printf("%i\n", calc(u, v));
    }

    return 0;
}
