void null() {
}

void print_small_int(int x) {
    null();
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

null();

print_small_int(1);
print_small_int(2);
int x = 1 + add(2, 3 * 4);

write_char('o');
write_char('k');
write_char('\n');
