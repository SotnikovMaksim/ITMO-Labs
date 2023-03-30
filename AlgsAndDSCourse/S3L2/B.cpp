#include <cstdio>
#include <vector>
#include <queue>
#include <algorithm>

int t, n, m, u, v;

void solve()
{
	scanf("%i %i", &n, &m);

	std::vector< std::vector< int > > childs = std::vector< std::vector< int > >(n + 1, std::vector< int >());
	std::vector< std::vector< int > > parents = std::vector< std::vector< int > >(n + 1, std::vector< int >());
	std::vector< int > d = std::vector< int >(n + 1, 0);
	std::vector< int > state = std::vector< int >(n + 1, -1);
	std::queue< int > q;

	for (int i = 0; i < m; i++)
	{
		scanf("%i %i", &u, & v);

		childs[u].push_back(v);
		parents[v].push_back(u);
		d[u]++;
	}

	for (int i = 1; i < n + 1; i++)
	{
		if (d[i] == 0)
		{
			q.push(i);
			state[i] = 0;
		}
	}

	while (!q.empty())
	{
		int s = q.front();
		q.pop();

		if (state[s] == 0)
		{
			for (const int& parent: parents[s])
			{
				if (state[parent] == -1)
				{
					state[parent] = 1;
					q.push(parent);
				}
			}
		} else
		{
			for (const int& parent: parents[s])
			{
				d[parent]--;
				if (d[parent] == 0)
				{
					state[parent] = 0;
					q.push(parent);
				}
			}
		}
	}

	for (int i = 1; i < n + 1; i++)
	{
		printf("%s\n", state[i] == 0 ? "SECOND" : (state[i] == 1 ? "FIRST" : "DRAW"));
	}
	printf("\n");
}

int main()
{
	scanf("%i", &t);

	for (int i = 0; i < t; i++)
	{
		solve();
	}
}
