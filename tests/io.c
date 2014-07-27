int o = 'a' - 1;
int i = 'a' - 1;

while(o == i) {
    o = o + 1;
    write_char(o);
    i = read_char();
    int endline = read_char(); // useless \n
}
