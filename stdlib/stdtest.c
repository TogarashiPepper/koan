#include "./stdlib.c"

int main() {
	KoanArray arr;
    init_array(2, &arr);

    push_array(&arr, 1);
    push_array(&arr, 2);
    push_array(&arr, 3);

    print_arr_elems(&arr);

	KoanArray copied;

	map_array(std_plus, 2, &arr, &copied);

	print_arr_elems(&copied);

    free_array(&copied);
    free_array(&arr);
}
