#ifndef GHDL_TYPES_H
#define GHDL_TYPES_H

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

// Range/bounds of a dimension of an unconstrained array with dimensions of type 'natural'
typedef struct {
  int32_t left;
  int32_t right;
  int32_t dir;
  int32_t len;
} range_t;

// Range/bounds of an unconstrained array with 1, 2 or 3 dimensions of type 'natural'
typedef struct {
  range_t dim_1;
} bounds_t;
typedef struct {
  range_t dim_1;
  range_t dim_2;
} bounds2D_t;
typedef struct {
  range_t dim_1;
  range_t dim_2;
  range_t dim_3;
} bounds3D_t;

// Unconstrained array with dimensions of type 'natural'
typedef struct {
  void* array;
  bounds_t* bounds;
} ghdl_NaturalDimArr_t;

// Access to an unconstrained array with 1 dimension of type 'natural'
typedef struct {
  range_t range;
  uint8_t array[];
} ghdl_AccNaturalDimArr_t;

/*
*  Print custom types
*/

void print(ghdl_NaturalDimArr_t* ptr) {
    printf("array: %p\n", ptr->array);
    printf("bounds: %p\n", ptr->bounds);
    printf("bounds.left: %d\n", ptr->bounds->dim_1.left);
    printf("bounds.right: %d\n", ptr->bounds->dim_1.right);
    printf("bounds.dir: %d\n", ptr->bounds->dim_1.dir);
    printf("bounds.len: %d\n", ptr->bounds->dim_1.len);
}

/*
*  Convert a fat pointer of an unconstrained string, to a (null terminated) C string
*/

// @umarcor
char* ghdlToString(ghdl_NaturalDimArr_t* ptr) {
  assert(ptr != NULL);
  assert(ptr->bounds != NULL);
  int len = ptr->bounds->dim_1.len;
  char* str = malloc(sizeof(char) * len + 1);
  strncpy(str, ptr->array, len);
  str[len] = '\0';
  return str;
}

// In the prototype, Bradley declares a value instead of a reference. Why?

// @bradleyharden
/*
char* ghdl_array_to_string(array_t array) {
  // Add a null character, because GHDL strings are not null-terminated
  char *string = malloc(array.range->len + 1);
  strncpy(string, array.array, array.range->len);
  string[array.range->len] = '\0';
  return string;
}
*/

/*
*  Convert a fat pointer of an uncontrained array with (up to 3) dimensions of type 'natural', to C types
*/

void ghdlToArray(ghdl_NaturalDimArr_t* ptr, void** vec, int* len, int num) {
  assert(ptr != NULL);
  assert(ptr->bounds != NULL);
  *vec = ptr->array;

  void* b = ptr->bounds;
  switch (num) {
    case 1:
      len[0] = ((bounds_t*)b)->dim_1.len;
      break;
    case 2:
      len[0] = ((bounds2D_t*)b)->dim_2.len;
      len[1] = ((bounds2D_t*)b)->dim_1.len;
      break;
    case 3:
      len[0] = ((bounds3D_t*)b)->dim_3.len;
      len[1] = ((bounds3D_t*)b)->dim_2.len;
      len[2] = ((bounds3D_t*)b)->dim_1.len;
      break;
  }
}

/*
*  Convert a (null terminated) C string, to a fat pointer of an unconstrained string
*/

// @umarcor
/*
ghdl_NaturalDimArr_t* ghdlFromString(char* str) {
  uint32_t len = strlen(str);
  ghdl_NaturalDimArr_t* ptr = malloc(sizeof(ghdl_NaturalDimArr_t));
  ptr->array = malloc(sizeof(char) * len);
  strncpy((char*)(ptr->array), str, len);
  ptr->bounds = malloc(sizeof(bounds_t));
  bounds_t* b = ptr->bounds;
  b->dim_1.left = 1;
  b->dim_1.right = len;
  b->dim_1.dir = 0;
  b->dim_1.len = len;
  return ptr;
}
*/

// Again, the prototype I had (above) returns a reference instead of a value (Bradley's below)

// @bradleyharden
ghdl_NaturalDimArr_t ghdlFromString(char *string) {
  range_t *range = malloc(sizeof(range_t));
  assert(range != NULL);
  uint32_t len = strlen(string);
  range->left = 1;
  range->right = len;
  range->dir = 0;
  range->len = len;
  // Don't bother copying the string, because GHDL will do that anyway
  return (ghdl_NaturalDimArr_t){.array=string, .bounds=(bounds_t*)range};
}

/*
*  Convert C types representing an unconstrained array with a dimension of type 'natural', to a fat pointer
*/

ghdl_NaturalDimArr_t ghdlFromArray(void* vec, int* len, int num) {
  bounds_t* b = malloc(sizeof(bounds_t));
  assert(b != NULL);
  switch (num) {
    case 3:
      // TODO
    case 2:
      // TODO
    case 1:
      b->dim_1.left = 0;
      b->dim_1.right = len[0]-1;
      b->dim_1.dir = 0;
      b->dim_1.len = len[0];
  }
  return (ghdl_NaturalDimArr_t){.array=vec, .bounds=b};
}

/*
*  Convert an access to an unconstrained string, to a (null terminated) C string
*/

char* ghdlAccToString(ghdl_AccNaturalDimArr_t *line) {
  // Add a null character, because GHDL strings are not null-terminated
  char *string = malloc(line->range.len + 1);
  strncpy(string, line->array, line->range.len);
  string[line->range.len] = '\0';
}

/*
*  Convert C types representing an unconstrained array with a dimension of type 'natural', to an access
*/

// TODO: support 2 and 3 dimensions
ghdl_AccNaturalDimArr_t* ghdlAccFromArray(uint32_t length, size_t bytes) {
  ghdl_AccNaturalDimArr_t *access = malloc(sizeof(ghdl_AccNaturalDimArr_t) + length * bytes);
  assert(access != NULL);
  access->range.left = 0;
  access->range.right = length - 1;
  access->range.dir = 0;
  access->range.len = length;
  return access;
}

/*
*  Convert a (null terminated) C string, to an access to an unconstrained string
*/

/*
// @umarcor
ghdl_AccNaturalDimArr_t* ghdlLineFromString(char *str) {
  uint32_t len = strlen(str);
  ghdl_AccNaturalDimArr_t *line = malloc(sizeof(ghdl_AccNaturalDimArr_t) + sizeof(char) * len);
  line->bounds.left = 1;
  line->bounds.right = len;
  line->bounds.dir = 0;
  line->bounds.len = len;
  strncpy(line->array, str, len);
  return line;
}
*/

// @bradleyharden
ghdl_AccNaturalDimArr_t* ghdlAccFromString(char *string) {
  uint32_t length = strlen(string);
  ghdl_AccNaturalDimArr_t *line = ghdlAccFromArray(length, 1);
  // New access objects default to numbering from 0,
  // but VHDL strings must be numbered from 1
  line->range.left++;
  line->range.right++;
  // Don't copy the null termination
  strncpy(line->array, string, length);
  return line;
}

#endif
