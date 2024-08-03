#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    double* ptr;
    uint32_t len;
    uint32_t cap;
} KoanArray;

KoanArray init_array(uint32_t size) {
    KoanArray res = {
        .ptr = calloc(size, sizeof(double)),
        .len = 0,
        .cap = size,
    };

    if (res.ptr == NULL) {
        // TODO: potentially gracefully handle errors
        printf("Failed to allocate memory for KoanArray");
        exit(1);
    }

    return res;
}

static inline void assert_not_freed(KoanArray* array) {
    if (array->ptr == NULL) {
        printf("Tried to operate on a freed array\n");
        exit(1);
    }
}

double nth_array(KoanArray* array, uint32_t index) {
    assert_not_freed(array);

    if (index >= array->len) {
        printf("Index `%u` is out of bounds", index);
    }

    return array->ptr[index]; 
}

void resize_array(KoanArray* array) {
    assert_not_freed(array);

    array->ptr = realloc(array->ptr, 2 * array->cap * sizeof(double));
    if (!array->ptr) { 
        printf("Memory Re-allocation failed."); 
        exit(1);
    }
    array->cap = 2 * array->cap;
}

void set_array(KoanArray* array, uint32_t nth, double value) {
    assert_not_freed(array);

    if (nth >= array->len) {
        printf("Value `%u` is out of bounds", nth);
        exit(1);
    }
    array->ptr[nth] = value;
}

void push_array(KoanArray* array, double value) {
    assert_not_freed(array);

    if (array->len + 1 >= array->cap) {
        resize_array(array);
    }
    array->len += 1;

    set_array(array, array->len - 1, value);
}

void free_array(KoanArray* array) {
    free(array->ptr);
    array->ptr = NULL;
    array->cap = 0;
    array->len = 0;
}

// main function serves as a quick way to test the stdlib
// int main() {
//     KoanArray arr = init_array(2);
//     push_array(&arr, 0.1);
//     push_array(&arr, 0.2);
//     push_array(&arr, 0.3);
//
//     for (size_t i = 0; i < arr.len; ++i) {
//         printf("%f\n", nth_array(&arr, i));
//     }
//
//     free_array(&arr);
//
//     // Test that pushing to a freed array doesn't work
//     push_array(&arr, 0.2);
// }
