#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

#include <caml/mlvalues.h>
#include <caml/fail.h>

/* We colorize external values in black to be future-proof with Mark's
   patches to remove [caml_page_table_lookup]. */

#include <caml/gc.h> /* Make_header etc */

/* We insist on 8-byte alignment even though only 2 is required, for future
   expansion. */

static inline value zero_encode_ext_pointer(void *ptr) {
  if (((long)ptr & 7) != 0) {
    fprintf(stderr, "zero_encode_ext_pointer: pointer not aligned\n");
    abort();
  };
  return (value)((long)ptr + 1);
}

static inline void *zero_decode_ext_pointer(value v) {
  if (((long)v & 7) != 1) {
    fprintf(stderr, "zero_decode_ext_pointer: not an encoded pointer\n");
    abort();
  };
  return (void *)((long)v - 1);
}

#define Val_ext_pointer(x) zero_encode_ext_pointer(x)
#define Ext_pointer_val(x) zero_decode_ext_pointer(x)
