void null() {
}

void print_small_int(int x) {
    write_char('0' + x);
}

int add(int x, int y) {
    return x + y;
}

bool print_if(bool c, int x) {
    if(c) {
        write_char(x);
    }
    return !c;
}

write_char('o');
write_char('k');
write_char('\n');
