#include <cstdio>
#include <vector>
#include <map>
#include <iostream>
#include <sstream>
#include <algorithm>
#include <set>

std::map< int, std::string > index_name;
std::map< std::string, int > name_index;
std::vector< std::set< int > > childs;
std::vector< std::set< int > > parents;
std::vector< int > color;
std::vector< bool > visited;
std::vector< int > revOrder;
std::string line;
int n, m, u, v, ind;

void dfs(const int& x)
{
    visited[x] = true;

    for (const int& child: childs[x])
    {
        if (!visited[child])
        {
            dfs(child);
        }
    }

    revOrder[ind--] = x;
}

void dfsRev(const int& x, int cc)
{
    color[x] = cc;

    for (const int& parent: parents[x])
    {
        if (color[parent] == -1)
        {
            dfsRev(parent, cc);
        }
    }
}

void solve()
{
    std::vector< std::string > answer;
    for (int i = 1; i < n + 1; i++)
    {
        if (color[i] == color[i + n])
        {
            printf("-1");
            return;
        } else  if (color[i] > color[i + n])
        {
            answer.push_back(index_name[i]);
        }
    }

    std::cout << answer.size() << std::endl;
    for (const auto& name: answer)
    {
        std::cout << name << std::endl;
    }
}

int main()
{
    scanf("%i %i", &n, &m);

    color = std::vector< int >(n * 2 + 1, -1);
    childs = std::vector< std::set< int > >(n * 2 + 1, std::set< int >());
    parents = std::vector< std::set< int > >(n * 2 + 1, std::set< int >());
    visited = std::vector< bool >(n * 2 + 1, false);
    revOrder = std::vector< int >(2 * n);
    line = "";
    ind = 2 * n - 1;

    for (int i = 1; i < n + 1; i++)
    {
        std::cin >> index_name[i];

        name_index[index_name[i]] = i;
    }

    std::cin.ignore();
    for (int i = 0; i < m; i++)
    {
        getline(std::cin, line);

        std::stringstream line_split(line);

        std::string left_name, right_name, temp;
        getline(line_split, left_name, ' ');
        getline(line_split, temp, ' ');
        getline(line_split, right_name, ' ');

        u = name_index[left_name.substr(1, left_name.size() - 1)];
        v = name_index[right_name.substr(1, right_name.size() - 1)];

        int u2 = u + n, v2 = v + n;

        if (left_name[0] == '-')
        {
            u2 -= n;
            u += n;
        }

        if (right_name[0] == '-')
        {
            v2 -= n;
            v += n;
        }

        childs[u].insert(v);
        parents[v].insert(u);

        childs[v2].insert(u2);
        parents[u2].insert(v2);
    }

    for (int i = 1; i < 2 * n + 1; i++)
    {
        if (!visited[i])
        {
            dfs(i);
        }
    }

    int current_color = 1;
    for (const int& i: revOrder)
    {
        if (color[i] == -1)
        {
            dfsRev(i, current_color);
            current_color++;
        }
    }

    solve();
}
