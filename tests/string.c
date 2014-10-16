void write_string(int[] s) {
    int i = 0;
    while(s[i] != 0) {
        write_char(s[i]);
        i = i + 1;
    }
}


int s[] = "hello world\n";
write_string(s);
