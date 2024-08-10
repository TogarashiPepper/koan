#include "./stdlib.c"

int main() {
	KoanArray arr;
    init_array(2, &arr);

    push_array(&arr, 1);
    push_array(&arr, 2);
    push_array(&arr, 3);

	KoanArray copied;

	copy_array(&arr, &copied);

    print_array(&arr);
    print_arr_elems(&arr);

    free_array(&arr);
    free_array(&copied);
}
