#include <algorithm>
#include <cstdio>
#include <vector>

struct Node {
    std::vector<int> childs;
    int size;

    Node() = default;
};

bool *wasThere, *centr, *searched;
int n, m, u, v;
int *parent;
Node *nodes;
std::vector<std::vector<std::pair<int, int>>> dist;
std::vector<std::vector<int>> prefix;
std::vector<std::vector<int>> distToCentr;


void sizeDFS(const int &node, const int &c, const int &level)
{
    if (wasThere[node] || centr[node])
    {
        return;
    }
    wasThere[node] = true;
    searched[node] = false;
    distToCentr[node].emplace_back(level);
    if (c != 0)
    {
        dist[c].emplace_back(std::pair< int, int >(node, level));
    }

    int s = 0;
    for (auto child: nodes[node].childs)
    {
        if (!wasThere[child] && !centr[child])
        {
            sizeDFS(child, c, level + 1);
            s += nodes[child].size;
        }
    }
    nodes[node].size = s + 1;
    wasThere[node] = false;
}

void prepare(const int &c)
{
    std::sort(dist[c].begin(),
              dist[c].end(),
              [](const std::pair<int, int> &a, const std::pair<int, int> &b) { return a.second < b.second; });

    int min = n + 1;
    for (const std::pair<int, int> &item: dist[c])
    {
        min = std::min(min, item.first);
        prefix[c].emplace_back(min);
    }
}

int findCentroid(const int &node, const int &size)
{
    searched[node] = true;
    for (const int &child: nodes[node].childs)
    {
        if (nodes[child].size > size / 2 && !searched[child])
        {
            return findCentroid(child, size);
        }
    }

    centr[node] = true;
    for (const int &child: nodes[node].childs)
    {
        if (!centr[child])
        {
            sizeDFS(child, node, 1);
            parent[findCentroid(child, nodes[child].size)] = node;
        }
    }

    dist[node].emplace_back(std::pair< int, int >(node, 0));// Саму вершину тоже добавляем в её массив
    prepare(node);                                // Сортируем и ищем минимумы на префиксах
    return node;
}

int getIndex(const std::vector<std::pair<int, int>> &V, int val)
{
    auto iter =
            std::upper_bound(V.begin(), V.end(), val, [](int val, std::pair<int, int> const &p) { return val < p.second; });

    return iter != V.begin() ? std::distance(V.begin(), std::prev(iter)) : 0;
}

int calc(const int &node, const int &d)
{
    if (!d)
    {
        return node;// ноль получается
    }

    int ans = prefix[node][getIndex(dist[node], d)];
    int c = parent[node];// текущий центроид

    for (int i = distToCentr[node].size() - 1; i > 0; i--)
    {
        if (distToCentr[node][i] <= d)
        {
            ans = std::min(ans, prefix[c][getIndex(dist[c], d - distToCentr[node][i])]);
        }
        c = parent[c];
    }

    return ans;
}

int main()
{
    scanf("%i %i", &n, &m);
    nodes = (Node *) new Node[n + 1]();
    wasThere = new bool[n + 1]();
    searched = new bool[n + 1]();
    centr = new bool[n + 1]();
    parent = new int[n + 1]();
    dist = std::vector<std::vector<std::pair<int, int>>>(n + 1, std::vector<std::pair<int, int>>());
    distToCentr = std::vector<std::vector<int>>(n + 1, std::vector<int>());
    prefix = std::vector<std::vector<int>>(n + 1, std::vector<int>());

    for (int i = 0; i < n - 1; i++)
    {
        scanf("%i %i", &u, &v);
        nodes[u].childs.emplace_back(v);
        nodes[v].childs.emplace_back(u);
    }

    sizeDFS(1, 0, 0);

    delete[] wasThere;
    wasThere = new bool[n + 1]();

    findCentroid(1, n);

    delete[] searched;
    delete[] wasThere;
    delete[] centr;

    for (int i = 0; i < m; i++)
    {
        scanf("%i %i", &u, &v);
        printf("%i\n", calc(u, v));
    }

    delete[] nodes;
    delete[] parent;
    return 0;
}
