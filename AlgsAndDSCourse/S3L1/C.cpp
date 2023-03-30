#include <cstdio>
#include <vector>
#include <algorithm>

std::vector< std::vector< int > > childs;
std::vector< bool > visited;
std::vector< int > tin;
std::vector< int > rev;
std::vector< int > answer;
int n, m, l, r;

void dfs(int& t, const int& v, const int& p)
{
    visited[v] = true;

    tin[v] = t;
    rev[v] = t;
    t++;
    int c = 0; // кол-во детей не считая родителя

    for (const int& child: childs[v])
    {
        if (child == p) { continue; }

        if (visited[child])
        {
            rev[v] = std::min(rev[v], tin[child]);
        } else
        {
            dfs(t, child, v);

            rev[v] = std::min(rev[v], rev[child]);

            if (p != -1 && rev[child] >= tin[v])
            {
                answer.push_back(v);
            }

            c++;
        }
    }

    if (c > 1 && p == -1)
    {
        answer.push_back(v);
    }
}

int main()
{
    int t = 0;
    scanf("%i %i", &n, &m);
    visited = std::vector< bool >(n + 1);
    tin = std::vector< int >(n + 1);
    rev = std::vector< int >(n + 1);
    childs = std::vector< std::vector< int > >(n + 1, std::vector< int >());

    for (int i = 0; i < m; i++)
    {
        scanf("%i %i", &l, &r);

        childs[l].push_back(r);
        childs[r].push_back(l);
    }


    for (int i = 1; i < n + 1; i++)
    {
        if (!visited[i])
        {
            dfs(t, i, -1);
        }
    }

    sort( answer.begin(), answer.end() );
    answer.erase( unique( answer.begin(), answer.end() ), answer.end() );

    printf("%lu\n", answer.size());

    for (int i : answer)
    {
        printf("%i ", i);
    }

}
