#include "./stdlib.c"

int main() {
    KoanArray arr = init_array(2);
    push_array(&arr, 0.1);
    push_array(&arr, 0.2);
    push_array(&arr, 0.3);

    for (size_t i = 0; i < arr.len; ++i) {
        printf("%f\n", nth_array(&arr, i));
    }

    free_array(&arr);

    // Test that pushing to a freed array doesn't work
    push_array(&arr, 0.2);
}
