int f1(int x, int y) {
    int z = x * y;
    return x + y + z;
}

int f2(int x, int y, int z) {
    return x + y - z;
}

int x = f1(1, 2);
int y = f2(1, 2, 3);
    
write_char('0' + x);
write_char('\n');
write_char('0' + y);
