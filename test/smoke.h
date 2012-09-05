typedef unsigned int uint;

/******************************************************************************
 * Functions                                                                  *
 ******************************************************************************/

void   foo1(void);
void   foo2(int);
void   foo3(int, int);
int    foo4(void);
char   foo5(int);
char * foo6(int, int);
char * foo7(char *);
char * foo8(char * b);
char * foo9(char * (*b)(void));
char * foo10(char * (*b)(int));
char * foo11(char []);
char * foo12(char b[]);
char * foo13(char b[5]);
char * foo14(int);
int    foo15(char ***);
int    foo16(unsigned);
int    foo17(unsigned int);
int    foo18(uint);
int    foo19(int (*)(int));

/******************************************************************************
 * Structs                                                                    *
 ******************************************************************************/

struct bar1_t {
  void * a;
  int    b;
  char   c;
  char * d;
  char * (*e)(void);
  char * (*f)(int);
};

typedef struct bar2_t {
  int a;
} bar2_t;

typedef struct {
  int a;
} bar3_t;

/******************************************************************************
 * Enums                                                                      *
 ******************************************************************************/

enum {
  BAZ1 = 1
};

typedef enum {
  BAZ2 = 1
} baz2_t;

enum baz3_t {
  BAZ3 = 1
};

typedef enum baz4_t {
  BAZ4 = 1
} baz4_t;

/* smoke.h ends here */
