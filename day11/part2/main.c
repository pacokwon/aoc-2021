#include <stdio.h>

#define THRESHOLD 9
#define WIDTH 10
#define HEIGHT 10

int grid[HEIGHT][WIDTH];
int dx[] = {1, 1, 1, 0, -1, -1, -1, 0};
int dy[] = {1, 0, -1, -1, -1, 0, 1, 1};

int inRange(int x, int y) {
    return (0 <= x && x < HEIGHT && 0 <= y && y < WIDTH);
}

void flash(int x, int y) {
    for (int k = 0; k < 8; k++) {
        int newx = x + dx[k];
        int newy = y + dy[k];

        if (!inRange(newx, newy))
            continue;

        grid[newx][newy] += 1;

        // has just surpassed the threshold
        if (grid[newx][newy] == THRESHOLD + 1) {
            flash(newx, newy);
        }
    }
}

int step() {
    int count = 0;

    for (int i = 0; i < HEIGHT; i++) {
        for (int j = 0; j < WIDTH; j++) {
            grid[i][j] += 1;

            if (grid[i][j] == THRESHOLD + 1)
                flash(i, j);
        }
    }

    for (int i = 0; i < HEIGHT; i++)
        for (int j = 0; j < WIDTH; j++) {
            if (grid[i][j] > THRESHOLD) {
                count += 1;
                grid[i][j] = 0;
            }
        }

    return count;
}

int printOcto() {
    for (int i = 0; i < HEIGHT; i++) {
        for (int j = 0; j < WIDTH; j++) {
            printf("%d", grid[i][j]);
        }

        printf("\n");
    }

    printf("\n");
}

int main(void) {
    char buf[WIDTH + 1];
    int ans = 0;

    for (int i = 0; i < HEIGHT; i++) {
        scanf("%s", buf);
        for (int j = 0; j < WIDTH; j++)
            grid[i][j] = buf[j] - '0';
    }

    for (int i = 1; ; i++) {
        int count = step();
        if (count == WIDTH * HEIGHT) {
            ans = i;
            break;
        }
    }
    printf("%d\n", ans);


    return 0;
}
