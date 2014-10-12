// stupid function to compute 10 - x
int f1(int x) {
    int y = 0;
    while(x < 10) {
        x = x + 1;
        y = y + 1;
    }

    return y;
}

int f2(int x, int y) {
    return f1(x) + y;
}


int x = 0;
while(f2(x, 0) > 5) {
    x = x + 1;
}

write_char('0' + x);
