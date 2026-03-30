#define _GNU_SOURCE

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>

#include "ext_common.h"
#include "core_bigstring.h"
#include <ocaml_utils.h>
#include <unix_utils.h>

#ifndef MAP_HUGETLB
#define MAP_HUGETLB 0x40000 /* arch specific */
#endif

#ifndef MAP_HUGE_2MB
#define MAP_HUGE_2MB (21 << MAP_HUGE_SHIFT)
#endif

CAMLprim value zero_mmap_get_page_size_in_bytes() {
  return Val_long(sysconf(_SC_PAGESIZE));
}

#define FLAG(x)                                                                          \
  CAMLprim value zero_mmap_##x() { return Val_long(x); }

FLAG(PROT_EXEC)
FLAG(PROT_READ)
FLAG(PROT_WRITE)
FLAG(PROT_NONE)

// MAP_32BIT is omitted on purpose because it doesn't exist on aarch64
FLAG(MAP_SHARED)
FLAG(MAP_PRIVATE)
FLAG(MAP_ANONYMOUS)
FLAG(MAP_FIXED)
FLAG(MAP_GROWSDOWN)
FLAG(MAP_HUGETLB)
FLAG(MAP_HUGE_2MB)
FLAG(MAP_LOCKED)
FLAG(MAP_NONBLOCK)
FLAG(MAP_NORESERVE)
FLAG(MAP_POPULATE)
FLAG(MAP_STACK)

FLAG(MREMAP_MAYMOVE)
FLAG(MREMAP_FIXED)

FLAG(MS_ASYNC)
FLAG(MS_SYNC)
FLAG(MS_INVALIDATE)

FLAG(MADV_NORMAL)
FLAG(MADV_RANDOM)
FLAG(MADV_SEQUENTIAL)
FLAG(MADV_WILLNEED)
FLAG(MADV_DONTNEED)
FLAG(MADV_REMOVE)
FLAG(MADV_DONTFORK)
FLAG(MADV_DOFORK)
FLAG(MADV_HUGEPAGE)
FLAG(MADV_NOHUGEPAGE)

/* We don't use [Bigstring.unsafe_destroy] because it calls [munmap] */
static void destroy_bigstring(value v_bstr) {
  struct caml_ba_array *b = Caml_ba_array_val(v_bstr);
  if ((b->flags & CAML_BA_MANAGED_MASK) != CAML_BA_MAPPED_FILE)
    caml_failwith("destroy_bigstring: not a memory mapped bigstring");
  core_bigstring_destroy(v_bstr, CORE_BIGSTRING_DESTROY_DO_NOT_UNMAP |
                                     CORE_BIGSTRING_DESTROY_ALLOW_EXTERNAL);
}

extern value caml_unix_mapped_alloc(int flags, int num_dims, void *data, intnat *dim);

static value alloc_bigstring(void *data, uintnat delta, intnat len) {
  data = (void *)(((uintnat)data) + delta);
  return caml_unix_mapped_alloc(CORE_BIGSTRING_FLAGS, 1, data, &len);
}

/* Returns a pointer to the start of the data, not the header */
CAMLprim value zero_mmap_bigstring_to_ext_pointer(value v_bstr) {
  return Val_ext_pointer(Caml_ba_data_val(v_bstr));
}

void mmap_grow_file(int fd, off64_t size) {
  struct stat st;
  char c;
  int res;

  /* anonymous mmaps have FD = -1, and don't need file modification. */
  if (fd == -1)
    return;

  caml_enter_blocking_section();

  if (fstat(fd, &st) == -1) {
    caml_leave_blocking_section();
    uerror("fstat", Nothing);
  }

  /* Non-regular files (character special files, etc) can have [st_size] of 0
     and resizing them with [pwrite] or [ftruncate] can result in errors. Thus,
     it's better to restrict the resizing to only regular files. */
  if (S_ISREG(st.st_mode) && st.st_size < size) {
    /* Avoid accidental truncation of files by trying [pwrite] first.
       (See comment in function caml_grow_file in
       ocaml/otherlibs/bigarray/mmap_unix.c for details.) */
    c = 0;
    res = pwrite(fd, &c, 1, size - 1);

    if (res == -1) {
      if (errno == ESPIPE) {
        res = ftruncate(fd, size);
        if (res == -1) {
          caml_leave_blocking_section();
          uerror("ftruncate", Nothing);
        }
      } else {
        caml_leave_blocking_section();
        uerror("pwrite", Nothing);
      }
    }
  }

  caml_leave_blocking_section();
}

CAMLprim value zero_mmap_grow_file(value v_fd, value v_pos, value v_len) {
  mmap_grow_file(Int_val(v_fd), Int64_val(v_pos) + Long_val(v_len));
  return Val_unit;
}

CAMLprim value zero_mmap_mmap64(value v_fd, value v_pos, value v_len, value v_prot,
                                value v_flags) {
  intnat len = Long_val(v_len);
  off64_t pos = Int64_val(v_pos);
  uintnat page, delta;
  void *data;

  /* Mmap requires the offset to be aligned on a page size. We start
     the mapping on the beginning of the page the offset is part of
     and shift the beginning of the bigstring to make it start where
     the user requested. This is what the bigarray library is doing
     too. */
  page = sysconf(_SC_PAGESIZE);
  delta = pos % page;

  caml_enter_blocking_section();
  data = mmap64(NULL, len + delta, Int63_val(v_prot), Int63_val(v_flags), Int_val(v_fd),
                pos - delta);
  caml_leave_blocking_section();

  if (data == (void *)MAP_FAILED)
    uerror("mmap64", Nothing);

  return alloc_bigstring(data, delta, len);
}

CAMLprim value zero_mmap_mlock(value v_bstr, value v_pos, value v_len) {
  CAMLparam1(v_bstr);
  void *data;
  int res;

  data = get_bstr(v_bstr, v_pos);

  caml_enter_blocking_section();
  res = mlock(data, Long_val(v_len));
  caml_leave_blocking_section();

  if (res == -1)
    uerror("mlock", Nothing);

  CAMLreturn(Val_unit);
}

CAMLprim value zero_mmap_munlock(value v_bstr, value v_pos, value v_len) {
  CAMLparam1(v_bstr);
  void *data;
  int res;

  data = get_bstr(v_bstr, v_pos);

  caml_enter_blocking_section();
  res = munlock(data, Long_val(v_len));
  caml_leave_blocking_section();

  if (res == -1)
    uerror("munlock", Nothing);

  CAMLreturn(Val_unit);
}

CAMLprim value zero_mmap_mprotect(value v_bstr, value v_pos, value v_len,
                                  value v_protection) {
  CAMLparam1(v_bstr);
  void *data;
  int res;

  data = get_bstr(v_bstr, v_pos);

  caml_enter_blocking_section(); /* Not sure this is needed but it
                                    doesn't seem crazy to assume
                                    mprotect might block. */
  res = mprotect(data, Long_val(v_len), Int63_val(v_protection));
  caml_leave_blocking_section();

  if (res == -1)
    uerror("mprotect", Nothing);

  CAMLreturn(Val_unit);
}

CAMLprim value zero_mmap_msync(value v_bstr, value v_pos, value v_len, value v_flags) {
  CAMLparam1(v_bstr);
  void *data;
  int res;

  data = get_bstr(v_bstr, v_pos);

  caml_enter_blocking_section();
  res = msync(data, Long_val(v_len), Int63_val(v_flags));
  caml_leave_blocking_section();

  if (res == -1)
    uerror("msync", Nothing);

  CAMLreturn(Val_unit);
}

CAMLprim value zero_mmap_madvise(value v_bstr, value v_pos, value v_len, value v_advice) {
  CAMLparam1(v_bstr);
  void *data;
  int res;

  data = get_bstr(v_bstr, v_pos);

  caml_enter_blocking_section(); /* Not sure this is needed but it
                                    doesn't seem crazy to assume
                                    madvise might block. */
  res = madvise(data, Long_val(v_len), Int63_val(v_advice));
  caml_leave_blocking_section();

  if (res == -1)
    uerror("madvise", Nothing);

  CAMLreturn(Val_unit);
}

CAMLprim value zero_mmap_mincore(value v_bstr, value v_pos, value v_len, value v_vec) {
  CAMLparam2(v_bstr, v_vec);
  void *data;
  unsigned char *vec;
  int res;

  data = get_bstr(v_bstr, v_pos);
  vec = (unsigned char *)Caml_ba_data_val(v_vec);

  caml_enter_blocking_section();
  res = mincore(data, Long_val(v_len), vec);
  caml_leave_blocking_section();

  if (res == -1)
    uerror("mincore", Nothing);

  CAMLreturn(Val_unit);
}

CAMLprim value zero_mmap_mremap(value v_bstr, value v_newsize, value v_flags) {
  struct caml_ba_array *b = Caml_ba_array_val(v_bstr);
  intnat oldsize = b->dim[0];
  intnat newsize = Long_val(v_newsize);
  void *newdata;
  void *addr = b->data;
  uintnat page, delta;

  destroy_bigstring(v_bstr);

  caml_enter_blocking_section();
  /* Same as in zero_mmap_mmap64 */
  page = sysconf(_SC_PAGESIZE);
  delta = (uintnat)addr % page;
  newdata = mremap((void *)(((uintnat)addr) - delta), oldsize + delta, newsize + delta,
                   Int63_val(v_flags));
  caml_leave_blocking_section();

  if (newdata == (void *)MAP_FAILED)
    uerror("mremap", Nothing);

  return alloc_bigstring(newdata, delta, newsize);
}

CAMLprim value zero_fallocate(value v_fd, value v_pos, value v_len) {
  intnat len = Long_val(v_len);
  off64_t pos = Int64_val(v_pos);

  /* 0 is the "mode" argument.  From the man pages: "The default
     operation (i.e., mode is zero) of fallocate() allocates and
     initializes to zero the disk space within the range specified
     by offset and len." */
  if (fallocate(Int_val(v_fd), 0, pos, len)) {
    return Val_int(errno);
  }
  return Val_int(0);
}
