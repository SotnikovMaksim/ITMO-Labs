#include <cstdio>
#include <vector>
#include <algorithm>

struct Node
{
    std::vector< int > childs;
    std::vector< int > incoming;

    Node() = default;
};

int n, m, u, v;
Node* nodes;

void solve()
{
    std:
    std::vector< int > S;
    std::vector< int > L;

//    Find all vertices with no incoming edge
    for (int i = 1; i <= n; i++)
    {
        if (nodes[i].incoming.empty())
        {
            S.push_back(i);
        }
    }

//    Main loop
    while (!S.empty())
    {
        int source = S.back();
        S.pop_back();
        L.push_back(source);

        for (const auto &child: nodes[source].childs)
        {
            nodes[child].incoming.erase(
                    std::find(
                            nodes[child].incoming.begin(),
                            nodes[child].incoming.end(),
                            source));
            if (nodes[child].incoming.empty())
            {
                S.push_back(child);
            }
        }

        m -= nodes[source].childs.size();
    }

    if (m > 0)
    {
        printf("-1");
    } else
    {
        for (auto const &node: L)
        {
            printf("%i ", node);
        }
    }
}

int main()
{
    scanf("%i %i", &n, &m);
    nodes = (Node*) new Node[n + 1];
    std::vector< int > answer;

    for (int i = 0; i < m; i++)
    {
        scanf("%i %i", &u, &v);
        nodes[u].childs.push_back(v);
        nodes[v].incoming.push_back(u);
    }

    solve();
}
