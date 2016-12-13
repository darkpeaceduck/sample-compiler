#include <stdio.h>
#include <memory.h>
#include <stdlib.h>
#include<stdarg.h>

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

typedef struct {
	int boxed;
	int n_args;
	void * contents[0];
} array;

array * arralloc(int n_args) {
	return malloc(sizeof(array) + n_args * sizeof(void *));
}

#define FILL_ARR_ARGS(ar, n_args)  \
	va_list ap; \
	void * item; \
	int i; \
	va_start(ap, n_args); \
	for(i = 0; i < n_args; i++) { \
	  item =  va_arg(ap, void *); \
	  ar->contents[i] = item; \
	} \
	va_end(ap)


array * BT(arrcreate_boxed)(int n_args, ...) {
	array * ar = arralloc(n_args);
	ar->boxed = 1;
	ar->n_args = n_args;
	FILL_ARR_ARGS(ar, n_args);
	return ar;
}

array * BT(arrcreate_unboxed)(int n_args, ...) {
	array * ar = arralloc(n_args);
	ar->boxed = 0;
	ar->n_args = n_args;
	FILL_ARR_ARGS(ar, n_args);
	return ar;
}

array * BT(arrmake)(int n, void * v) {
	array * ar = arralloc(n);
	ar->boxed = 1;
	ar->n_args = n;
	for(int i = 0; i < n; i++) {
		ar->contents[i] = v;
	}
	return ar;
}

array * arriterate(array *ar, int n_args, va_list *ap) {
	int index;
	for(int i = 1; i < n_args; i++) {
		if (!ar->boxed && n_args - i > 1) {
			fprintf(stderr, "subarray found in unboxed array");
			abort();
		}
		index = va_arg(*ap, int);
		ar = ar->contents[index];
	}
	return ar;
}

void * BT(arrget)(int n_args, ...) {
	va_list ap;
	void *out;
	array * ar;

	va_start(ap, n_args);
	ar = va_arg(ap, void*);
	out = arriterate(ar, n_args, &ap);
	va_end(ap);

	return out;
}

void BT(arrset)(int n_args, ...) {
	va_list ap;
	array * ar;

	va_start(ap, n_args);
	ar = va_arg(ap, void*);
	ar = arriterate(ar, n_args - 2, &ap);
	int index = va_arg(ap, int);
	void * value = va_arg(ap, void*);
	ar->contents[index] = value;
	va_end(ap);
}

int BT(arrlen)(array * ar) {
	return ar->n_args;
}
