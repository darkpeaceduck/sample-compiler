#include <stdio.h>
#include <memory.h>
#include <stdlib.h>

#define CON(a, b) a##b
#define BT(name) CON(bt_,name)

extern int BT(read) () {
  int d;
  printf ("> ");
  scanf ("%d", &d);
  return d;
}

extern void BT(write) (int x) {
  printf ("%d\n", x);
}

extern char * BT(strmake) (int i, char c) {
	char * new = malloc(i + 1);
	memset(new, c, i);
	new[i] = 0;
	return new;
}

extern char * BT(strset)(char *s, int i, char c) {
	s[i] = c;
	return s;
}

extern char BT(strget)(char *s, int i) {
	return s[i];
}

extern char * BT(strsub)(char *a, int i, int len) {
	char * new = malloc(len + 1);
	strncpy(new, a+i, len);
	new[len] = 0;
	return new;
}

extern char * BT(strdup)(char *a) {
	return strdup(a);
}

extern char * BT(strcat)(char *a, char *b) {
	char *aa = malloc(strlen(a) + strlen(b) + 2);
	aa[0] = 0;
	strcat(aa, a);
	strcat(aa, b);
	return aa;
}

extern int BT(strcmp) (char *a, char *b) {
	return strcmp(a, b);
}

extern int BT(strlen) (char *a) {
	return strlen(a);
}

