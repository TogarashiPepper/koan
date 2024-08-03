#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct {
    double* data;
    uint32_t refcount;
    uint32_t len;
    uint32_t cap;
} KArrBox;

typedef struct {
    KArrBox* ptr;
    bool freed;
} KoanArray;

KoanArray init_array(uint32_t size) {
    KArrBox box = {
        .data = malloc(size * sizeof(double)),
        .refcount = 1,
        .len = 0,
        .cap = size,
    };

    KArrBox* ptr = malloc(sizeof(KArrBox));
    *ptr = box;

    KoanArray res = {
        .ptr = ptr,
        .freed = false,
    };

    if (ptr->data == NULL) {
        // TODO: potentially gracefully handle errors
        printf("Failed to allocate memory for KoanArray");
        exit(1);
    }

    return res;
}

static inline void assert_not_freed(KoanArray* array) {
    if (array->ptr == NULL || array->ptr->data == NULL || array->freed) {
        printf("Tried to operate on a freed array\n");
        exit(1);
    }
}

uint32_t len_array(KoanArray* array) {
    assert_not_freed(array);

    return array->ptr->len;
}


uint32_t cap_array(KoanArray* array) {
    assert_not_freed(array);

    return array->ptr->cap;
}

double nth_array(KoanArray* array, uint32_t index) {
    assert_not_freed(array);

    if (index >= len_array(array)) {
        printf("Index `%u` is out of bounds", index);
    }

    return array->ptr->data[index]; 
}

void resize_array(KoanArray* array) {
    assert_not_freed(array);

    array->ptr->data = realloc(array->ptr->data, 2 * array->ptr->cap * sizeof(double));
    if (!array->ptr->data) { 
        printf("Memory Re-allocation failed."); 
        exit(1);
    }
    array->ptr->cap = 2 * array->ptr->cap;
}

void set_array(KoanArray* array, uint32_t nth, double value) {
    assert_not_freed(array);

    if (nth >= len_array(array)) {
        printf("Value `%u` is out of bounds", nth);
        exit(1);
    }
    array->ptr->data[nth] = value;
}

void push_array(KoanArray* array, double value) {
    assert_not_freed(array);

    if (len_array(array) + 1 >= cap_array(array)) {
        resize_array(array);
    }
    array->ptr->len += 1;

    set_array(array, len_array(array) - 1, value);
}

void free_array(KoanArray* array) {
    assert_not_freed(array);

    if (array->ptr->refcount == 1) {
        free(array->ptr->data);
        free(array->ptr);
        array->freed = true;
        array->ptr = NULL;
    }
    else {
        array->ptr->refcount -= 1;
        array->freed = true;
        array->ptr = NULL;
    }
}

KoanArray copy_array(KoanArray* arr) {
    assert_not_freed(arr);

    KoanArray copied = {
        .ptr = arr->ptr,
        .freed = arr->freed,
    };

    arr->ptr->refcount += 1;

    return copied;
}

void print_array(KoanArray* arr) {
    assert_not_freed(arr);

    KArrBox* box = arr->ptr;

    printf("KoanArray {\n");
    printf("\t.ptr = %p\n", box->data);
    printf("\t.refcount = %u\n", box->refcount);
    printf("\t.len = %u\n", box->len);
    printf("\t.cap = %u\n", box->cap);
    printf("};\n");
}

void print_arr_elems(KoanArray* arr) {
    assert_not_freed(arr);

    printf("[");

    for (uint32_t i = 0; i < len_array(arr); ++i) {
        double val = nth_array(arr, i);

        printf("%f, ", val);
    }

    printf("\b\b]\n");
}
