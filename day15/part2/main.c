#include <stdio.h>
#define GWIDTH 100
#define GHEIGHT 100
#define WIDTH (GWIDTH * 5)
#define HEIGHT (GHEIGHT * 5)
#define UNDEF -1

char risk[WIDTH + 2][HEIGHT + 2];
int cost[WIDTH + 2][HEIGHT + 2];
int dx[] = {1, 0, -1, 0};
int dy[] = {0, -1, 0, 1};

int min(int a, int b) {
    return a > b ? b : a;
}

void showRisk() {
    for (int i = 0; i < HEIGHT; i++) {
        for (int j = 0; j < WIDTH; j++) {
            printf("%d", risk[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

void showCost() {
    for (int i = 0; i < HEIGHT; i++) {
        for (int j = 0; j < WIDTH; j++) {
            printf("%4d", cost[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

int inRange(int x, int y) {
    return (0 <= x && x < HEIGHT && 0 <= y && y < WIDTH);
}

int getMinCost(int x, int y) {
    int minCost = 987654321;

    for (int k = 0; k < 4; k++) {
        int newx = x + dx[k];
        int newy = y + dy[k];

        if (!inRange(newx, newy))
            continue;

        if (cost[newx][newy] == UNDEF)
            continue;

        minCost = min(minCost, cost[newx][newy]);
    }

    return minCost;
}

void runDP() {
    for (int i = 0; i < HEIGHT; i++) {
    for (int j = 0; j < WIDTH; j++) {
        if (i == 0 && j == 0) continue;

        int minCost = getMinCost(i, j);

        cost[i][j] = risk[i][j] + minCost;
    }
    }
}

void initCost() {
    for (int i = 0; i < HEIGHT; i++) {
        for (int j = 0; j < WIDTH; j++)
            cost[i][j] = UNDEF;
    }
    cost[0][0] = 0;
}

int wrapAdd(int x, int y) {
    return (x + y - 1) % 9 + 1;
}

void readMap() {
    char buf[GWIDTH + 1];

    for (int i = 0; i < GHEIGHT; i++) {
        scanf("%s", buf);
        for (int j = 0; j < GWIDTH; j++)
            risk[i][j] = buf[j] - '0';
    }

    for (int i = 0; i < 5; i++)
    for (int j = 0; j < 5; j++)
        for (int x = 0; x < GHEIGHT; x++) {
            for (int y = 0; y < GWIDTH; y++) {
                if (i == 0 && j == 0)
                    continue;

                int xx = i * GHEIGHT + x;
                int yy = j * GWIDTH + y;
                int taxyDist = i + j;

                risk[xx][yy] = wrapAdd(risk[x][y], taxyDist);
            }
        }
}

int main(void) {
    readMap();

    initCost();

    // not too confident on this one...
    for (int k = 0; k < 10; k++) {
        runDP();
    }

    printf("%d\n", cost[WIDTH - 1][HEIGHT - 1]);

    return 0;
}
