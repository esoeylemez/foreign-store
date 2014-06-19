#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include <stdint.h>

void **values = NULL;
size_t size = 0;

uint32_t x_store(void*value) {
  int no_slot = 1;
  uint32_t i;
  uint32_t index;
  for (i = 0; i < size; i++) {
    if (values[i] == NULL) {
      index = i;
      no_slot = 0;
      break;
    }
  }
  if (values == NULL || no_slot) {
    size++;
    values = realloc(values,size * sizeof(void*));
    index = size-1;
  }
  values[index] = value;
  return index;
}

void x_set(uint32_t index,void*value) {
  if (index + 1 > size) {
    size_t new_size = index + 1;
    values = realloc(values,new_size * sizeof(void*));
    memset(values + size,0,new_size - size);
    size = new_size;
  }
  values[index] = value;
}

void *x_get(uint32_t index) {
  if (index >= 0 && index < size) {
    return values[index];
  } else {
    return NULL;
  }
}

uint32_t x_lookup(uint32_t index) {
  if (values == NULL)
    return 0;
  else {
    if (index >= 0 && index < size) {
      return values[index] == NULL? 0 : 1;
    } else {
      return 0;
    }
  }
}

void x_delete(uint32_t index) {
  values[index] = NULL;
}
