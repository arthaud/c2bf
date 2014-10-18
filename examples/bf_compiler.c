void write_string(int[] s) {
    int i = 0;
    while(s[i] != 0) {
        write_char(s[i]);
        i = i + 1;
    }
}

int init[] = "#include <stdio.h>\n\nint main() {\n\tunsigned char tab[30000];\n\tunsigned char* ptr = tab;";
write_string(init);

int read = -1;
while(read != 0) {
    read = read_char();

    if(read == '>') {
        int instr[] = "\n\tptr++;";
        write_string(instr);
    }
    if(read == '<') {
        int instr[] = "\n\tptr--;";
        write_string(instr);
    }
    if(read == '+') {
        int instr[] = "\n\t(*ptr)++;";
        write_string(instr);
    }
    if(read == '-') {
        int instr[] = "\n\t(*ptr)--;";
        write_string(instr);
    }
    if(read == '.') {
        int instr[] = "\n\tputchar(*ptr);";
        write_string(instr);
    }
    if(read == ',') {
        int instr[] = "\n\t(*ptr) = getchar();";
        write_string(instr);
    }
    if(read == '[') {
        int instr[] = "\n\twhile(*ptr) {";
        write_string(instr);
    }
    if(read == ']') {
        int instr[] = "\n\t}";
        write_string(instr);
    }
}

int end[] = "\n\treturn 0;\n}";
write_string(end);
