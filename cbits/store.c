#include <stdlib.h>
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

void *x_get(uint32_t index) {
  return values[index];
}

uint32_t x_lookup(uint32_t index) {
  if (values == NULL)
    return 0;
  else {
    if (index < size) {
      return values[index] == NULL? 0 : 1;
    } else {
      return 0;
    }
  }
}

void x_delete(uint32_t index) {
  values[index] = NULL;
}
