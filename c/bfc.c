#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static char *readall(FILE *fp) {
	char buf[256];
	size_t size = 0;
	char *s = NULL;
	while (fgets(buf, sizeof(buf), fp) != NULL) {
		size += sizeof(buf);
		s = (char *) realloc(s, size);
		strcat(s, buf);
	}
	return s;
}

static void compile(const char *s) {
	puts("#include <stdio.h>");
	puts("int main() {");
	puts("char mem[30000] = {0};");
	puts("char *ptr = mem;");
	while (*s) {
		switch (*s++) {
			case '>': puts("ptr++;"); break;
			case '<': puts("ptr--;"); break;
			case '+': puts("(*ptr)++;"); break;
			case '-': puts("(*ptr)--;"); break;
			case '.': puts("putchar(*ptr);"); break;
			case ',': puts("*ptr = getchar();"); break;
			case '[': puts("while (*ptr) {"); break;
			case ']': puts("}"); break;
		}
	}
	puts("return 0;");
	puts("}");
}

int main(int argc, char *argv[]) {
	char *s = NULL;

	if (argc == 2) {
		FILE *fp;
		if ((fp = fopen(argv[1], "r")) == NULL) {
			printf("file open error: %s\n", argv[1]);
			exit(EXIT_FAILURE);
		}
		s = readall(fp);
		fclose(fp);
	} else if (argc == 1) {
		s = readall(stdin);
	}

	if (s != NULL) {
		compile(s);
	}
}

