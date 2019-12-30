#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

void printInt(int x) {
  printf("%d\n", x);
}

void printString(const char *str) {
  printf("%s\n", str);
}

char* readString() {
  size_t n = 0;
  char* buf = NULL;
  size_t len = getline(&buf, &n, stdin);

  if (len > 0 && buf[len - 1] == '\n') {
    buf[len - 1] = '\0';
  }

  return buf;
}

int readInt() {
  int x;
  scanf("%d", &x);
  return x;
}

void error() {
  fprintf(stderr, "runtime error\n");
  exit(1);
}

bool streq(const char* str1, const char* str2) {
  return strcmp(str1, str2) == 0;
}

bool strne(const char* str1, const char* str2) {
  return strcmp(str1, str2) != 0;
}

char* concat(const char* str1, const char* str2) {
  char* ret = malloc(strlen(str1) + strlen(str2) + 1);
  strcpy(ret, str1);
  strcat(ret, str2);
  return ret;
}
