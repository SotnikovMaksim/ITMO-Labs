#include <algorithm>
#include <cstdio>
#include <list>
#include <optional>
#include <vector>

#define INF 2000000000

struct Triple
{
	Triple(int i, int i1, int i2) {
		a = i;
		b = i1;
		w = i2;
	}

	int a;
	int b;
	int w;
};

int n, w;
std::vector< Triple > edges;

void solve()
{
	std::vector< int > g(n + 1, INF);
	std::vector< int > p(n + 1, -1);
	g[1] = 0;

	for (int i = 0; i < n - 1; i++)
	{
		for (const auto &edge : edges)
		{
			if (g[edge.a] + edge.w < g[edge.b])
			{
				p[edge.b] = edge.a;
				g[edge.b] = g[edge.a] + edge.w;
			}
		}
	}

	std::optional< int > jaba;

	for (const auto &edge: edges)
	{
		if (g[edge.a] + edge.w < g[edge.b])
		{
			p[edge.b] = edge.a;
			g[edge.b] = g[edge.a] + edge.w;
			jaba = edge.b;
		}
	}

	if (!jaba)
	{
		printf("NO");
	} else
	{
		int y = *jaba;

		for (int i = 1; i < n + 1; i++)
		{
			y = p[y];
		}

		std::list< int > path;

		for (int cur = y; !(cur == y && !path.empty()); cur = p[cur])
		{
			path.push_back(cur);
		}

		std::reverse(path.begin(), path.end());
		printf("YES\n%lu\n", path.size());

		for (int i : path)
		{
			printf("%i ", i);
		}
	}
}

int main()
{
	scanf("%i", &n);

	for (int i = 1; i < n + 1; i++)
	{
		for (int j = 1; j < n + 1; ++j)
		{
			scanf("%i", &w);

			if (w != 100000)
			{
				edges.emplace_back(i, j, w);
			}
		}
	}
	
	solve();
}

/*
 *
#&&########BGGGGPGPGGPYP55B########BBB#BBBGBBPGPJ5PYGGPGBGJPBPPGG55BBGBBG55PGYYYPBB##BBBBBPGPP5Y555P
BB##BBB#GBG###BB55P55PGPGBPPGGBGB####BBBGP5PBBBBBBBGYPBBGGPPGBBGB##BPGG#PYGGGGBBG5YB###GP55PP5Y5BBG5
########BB#####BGP555PB#BG5Y55GPPGGBG5GGGBBPB5PBBPJJJ5GYYPYYYB#BB###BBBBBBGJ?JJYPP5PBBGY5555YY55555Y
##&#####&##&B#&###GBBB##GPBPPGBGGBGBPG#PGB5PBJPBB5J?YGGGGP5BGYJGBGY5BBBBB5YYJJJ?YGPP5P5Y5P5JYGPYJYGG
##&##&##&##&B5G#B###GBGPYBBGBBBBGBB#GBGGGP5GGGBGBBGGBBBBBBGBP??5GYJYGB#GPY5PBGPYYGBB555PYYYYYYYYPPB#
#####GB#&###BB#BB###BBBPPB#GBBBB#GBBGGPGPP55Y55YYY5GGGGPG5??5GPG55#####5YB#####BBBBB555PYJY5YJ5PBB#&
#####PB########BB###BG########BP5BGGBBBG5YJY5555YJ?YGPJJ5YYYPGGY775#GGB5J5BB###BBBBGG55YJJYYYYPG5PB5
B##BGB##&&###########BB#####BBG5JBGP55PP5YYYYYYJJYYY5YY55PP5GBG5PPPPJJ5PJYPGBBB5PGPPPP5Y55PGP55BGBGP
7?????JYYY5JY5PPPGGGGGGBPPPBBGPG555JJJ?YJJJYYYJJYYY55YYYJYPP55PPPBP5PGPGPGPGPPG5Y55YYY5GGGBBGGBPGGYP
^^^^^^^::::::::::::::~!77!~!JJYJJYJJYJJJ??JJYJY555PPPPPP555YYYYY5PPPPGGBGGPGGBGP55YY55PPBYJYBGBBBGY5
~~~~~^^^^^^^^^^^^::~!?Y5555YYJJJJJJ?JJ5555YJJJY5Y555P55YYYY55YJJYYYYY555YY?77!!!!7777??JJ7?JJJJ777~~
~~~~~^^^^^^^^^^^^^~J?!JJJJY5YJJJ??JYYJYYY555YYYYYY5555YYYYYYYYYYJ?55YYYJ55P557:::::::::::^^^::^^^^^^
~~~~~~^^^^^^^^^^^^^7?~?7?JYY??JJ?77?!!YGGG5PP55555PPPPGGPGPYYY55Y5P5Y55Y5555PPY?~::::::::^^^^^^^^^~~
^^^^^^^^^^^^^^^^^^^~7???JJ?JJJJJYJ^!P5PGP5JY55Y55PPPPPPGBB5YJJY5YY5YYY5YYPP55PPP5J?7^:^^^^^^^^^^^^^^
^^^^^^^^^^^::::^::7???J??J?JJJYY5?!?YYJJJYYY5555555YY5YY5P5J??J55JJY55555555555PPPPP7::^^^^^^^^^^^^^
^^^^^^:::^:::::::!?777?7?Y5JYYJ55YJJ??YY55555PP555PJJY5Y55P5JJJJJJJ5GG55Y555555PPPPPPJ^:::::::::^^^:
^^^^^^^^^^:^:::^77777??JJ?JJYJJYYYYJJY55555555555YJ?YYY5555P5YJ??YP55YYY555555PGGP55PP57::::::::::::
^^^^^^^^^^^:::~777777?JJJJ??JJJY5YYYYYJYYYYYYYYYYJYYYY555PPPP5J??JJY5J?YPP555555P55P55PPJ^::::::::::
^^^^^^^^^^^^^^7?7!!7?JJJJ??JJYYY5YYYYYYYYY5555555YY5YY555P5P5JJJJJYY5555P555YYY55P55PPPPG5?::::^::::
^^^^^^^^:::^^^777777?7????YYYYYYY55555YY55555Y5YYYJYYYY55555YYP5JYYJJY5YYYJYYY55P555PPPPPPY:::::::::
~!!!~~~~~~~~~~~!!7?77??JJYY5555555555YY55YYYYYYYYJJYYYY55YYYYYJ??J?7?JJ?JYJJYY55555555PP555?~^^::::^
777777!!!!!!!!~~:~~~~7??7?JJJYYYYYYYYYYYJJJJJJJJ?????JJJJJYY?!~!7?7?????JJJJJJJJJYYYYY5555555YJ?!~~~
777777777!77777!!!^^~~!!7777??????????7777777777?????JJ???7!!!7?JYY5555YJJJ??????JYY55PP5555P5555J77
7?777777777!77!!!?J7!~~~~~~~!!7!!!!!77777777777??????J?7!!!7?JY55PPPGGGGP5J?7777??JJY55555YPGPPGPPY?
777777777!!!!77!!7JY??JJ?7?7!~~~~~!!!!!!!!!!!77777777???JY5YYJJJY55PPPPPP555J77777??JJYYYYYGBPPGGGP5
7777777!!!!!!!!!!!!777JP577JY5J?7!!!!~~~!!!!!!77????JYYY55Y?JJYY5PGGGGGGPP555Y777????JJJYYPBBPP5Y5P5
7777777777!!!!!!!!!!777J55J?7JY5YJ?77!!!!!77777JYJY555Y5YYJYY555PGGGGGGGGGPP55J?7????JJJY5PPPP5YYYYY
7777777!77!!!!!~~~~~!!!7????7!7?77!!777?JJJYYY555YYJJJJYY555PPPPPGGGGGGGGGPP5YJ?????JJJY55555YYYYYYY
777777!!!!!!!!!!!!!~!!!!!!!!!!!!!!!!!!77??JJJYYYYYJJJYYY555PPPPPPPPPPGPPPPP55YYYYJYYYY55YYYYJJJJJJJJ
77!!!!!!!!!!!!!!!~~~!!!!!!~~~!!!!!!!!7??J?JJJJJYYYYYYY5555555PPPPPPGGGPP555PPPP555555YYYYYJJJJJJJJ??
!7!!!!!!!!!!!!!~~~~~~~~~~~~~~~~!!!!~!77??JJYYJJYY55YYY555555555PPPGGGPPPPPP555YYYYJJJJ??JJ??????????
77!!!!!!!!!!!!!~~~~~~~!!!~~~!!!!!!!!!!!!!777?????JJJYYY555PPPPPPPPPPP5555YYJJJ???????7777???????77??
77777!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!7777?????JJJJJJJJJJJJJJ?????777777777777777777777777
777777!!!7!!!!!!!!!!!!!!!!!!!!!!!!~~~~!!!!!!!!!!!!!777777777777777777777777777777!!!!777777777777777
??777777777!!!!!!!!!!!~~~!!!!!!!!~~~~~!!~~~~!!!!!!!!!!!!!!7777!777777777777777!!!!!!!!!!!!7777777777
 */
