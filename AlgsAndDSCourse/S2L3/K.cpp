#include <algorithm>
#include <cstdio>
#include <vector>

struct Node
{
    std::vector<long long> whiteSumTo;
    std::vector<long long> blackSumTo;

    std::vector<int> whiteCountTo;
    std::vector<int> blackCountTo;

    std::vector<int> childs;

    long long whiteSum = 0;
    long long blackSum = 0;

    int whiteCount = 0;
    int blackCount = 0;

    bool black = true;
    int size;

    Node() = default;
};

bool *wasThere, *centr, *searched;
int n, m, u, v;
int *parent, *childNumber;
Node *nodes;
std::vector<std::vector<int>> distToCentr;

// почти такой же, как и тот, который используется дальше, только без лишних операций,
// которые на этапе первичного подсчёта размеров нам не нужны
void firstDFS(const int &node)
{
    if (wasThere[node]) { return; }
    wasThere[node] = true;

    int s = 0;
    for (auto child: nodes[node].childs)
    {
        if (!wasThere[child] && !centr[child])
        {
            firstDFS(child);
            s += nodes[child].size;
        }
    }
    nodes[node].size = s + 1;
    wasThere[node] = false;
}

void sizeDFS(const int &node, const int &c, const int &level)
{
    if (wasThere[node] || centr[node]) { return; }
    wasThere[node] = true;
    searched[node] = false;
    distToCentr[node].push_back(level);

    // Так как по умолчанию все вершины покрашены в чёрный,
    // мы можем не спрашивать цвет текущей вершины и просто прибавить центроиду расстояние до них
    nodes[c].blackSum += level;
    nodes[c].blackCount += 1;

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

int findCentroid(const int &node, const int &size)
{
    searched[node] = true;
    for (const int &child: nodes[node].childs)
    {
        if (nodes[child].size > size / 2 && !searched[child]) { return findCentroid(child, size); }
    }

    centr[node] = true;
    int number = 0;
    for (const int &child: nodes[node].childs)
    {
        if (!centr[child])
        {
            long long whiteSumBefore = nodes[node].whiteSum;
            long long blackSumBefore = nodes[node].blackSum;

            int whiteCountBefore = nodes[node].whiteCount;
            int blackCountBefore = nodes[node].blackCount;

            sizeDFS(child, node, 1);
            int childCentr = findCentroid(child, nodes[child].size);
            parent[childCentr] = node;
            childNumber[childCentr] = number++;

            nodes[node].whiteSumTo.push_back(nodes[node].whiteSum - whiteSumBefore);
            nodes[node].blackSumTo.push_back(nodes[node].blackSum - blackSumBefore);

            nodes[node].whiteCountTo.push_back(nodes[node].whiteCount - whiteCountBefore);
            nodes[node].blackCountTo.push_back(nodes[node].blackCount - blackCountBefore);
        }
    }

    return node;
}

long long calc(const int &node)
{
    long long ans = nodes[node].black ? nodes[node].blackSum : nodes[node].whiteSum;
    int prevC = node;    // предыдущий центроид
    int c = parent[node];// текущий центроид

    if (nodes[node].black)
    {
        for (int i = distToCentr[node].size() - 1; i >= 0; i--)
        {
            ans += (nodes[c].blackSum - nodes[c].blackSumTo[childNumber[prevC]]) +
                   static_cast<long long>(distToCentr[node][i]) *
                           (static_cast<long long>(nodes[c].blackCount) - static_cast<long long>(nodes[c].blackCountTo[childNumber[prevC]]) +
                            static_cast<long long>(nodes[c].black));
            prevC = c;
            c = parent[c];
        }
    } else
    {
        for (int i = distToCentr[node].size() - 1; i >= 0; i--)
        {
            ans += nodes[c].whiteSum - nodes[c].whiteSumTo[childNumber[prevC]] +
                   static_cast<long long>(distToCentr[node][i]) *
                           (static_cast<long long>(nodes[c].whiteCount) - static_cast<long long>(nodes[c].whiteCountTo[childNumber[prevC]]) +
                            static_cast<long long>(!nodes[c].black));
            prevC = c;
            c = parent[c];
        }
    }

    return ans;
}

void paint(const int &node)
{
    bool black = nodes[node].black;
    nodes[node].black ^= true;
    int prevC = node;
    int c = parent[node];

    if (black)
    {
        for (int i = distToCentr[node].size() - 1; i >= 0; i--)
        {
            nodes[c].blackSum -= distToCentr[node][i];
            nodes[c].whiteSum += distToCentr[node][i];

            nodes[c].blackCount -= 1;
            nodes[c].whiteCount += 1;

            nodes[c].blackSumTo[childNumber[prevC]] -= distToCentr[node][i];
            nodes[c].whiteSumTo[childNumber[prevC]] += distToCentr[node][i];

            nodes[c].blackCountTo[childNumber[prevC]] -= 1;
            nodes[c].whiteCountTo[childNumber[prevC]] += 1;

            prevC = c;
            c = parent[c];
        }
    } else
    {
        for (int i = distToCentr[node].size() - 1; i >= 0; i--)
        {
            nodes[c].blackSum += distToCentr[node][i];
            nodes[c].whiteSum -= distToCentr[node][i];

            nodes[c].blackCount += 1;
            nodes[c].whiteCount -= 1;

            nodes[c].blackSumTo[childNumber[prevC]] += distToCentr[node][i];
            nodes[c].whiteSumTo[childNumber[prevC]] -= distToCentr[node][i];

            nodes[c].blackCountTo[childNumber[prevC]] += 1;
            nodes[c].whiteCountTo[childNumber[prevC]] -= 1;

            prevC = c;
            c = parent[c];
        }
    }
}


int main()
{
    scanf("%i %i", &n, &m);
    nodes = (Node *) new Node[n + 1]();
    wasThere = new bool[n + 1]();
    searched = new bool[n + 1]();
    centr = new bool[n + 1]();
    parent = new int[n + 1]();
    childNumber = new int[n + 1]();
    distToCentr = std::vector<std::vector<int>>(n + 1, std::vector<int>());

    for (int i = 0; i < n - 1; i++)
    {
        scanf("%i %i", &u, &v);
        nodes[u].childs.push_back(v);
        nodes[v].childs.push_back(u);
    }

    firstDFS(1);

    delete[] wasThere;
    wasThere = new bool[n + 1]();

    findCentroid(1, n);

    delete[] searched;
    delete[] wasThere;
    delete[] centr;

    for (int i = 0; i < m; i++)
    {
        scanf("%i %i", &u, &v);
        if (u == 1)
        {
            paint(v);
        } else
        {
            printf("%lli\n", calc(v));
        }
    }

    delete[] nodes;
    delete[] parent;
    return 0;
}
