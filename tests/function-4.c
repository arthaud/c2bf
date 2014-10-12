int f(int x, int y) {
    return 2 * x + y;
}

int x = 0;
int y = 3;

while(f(x, y) < 20) {
    x = x + 1;
}

write_char('0' + x);
