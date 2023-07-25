#include <fcntl.h>
#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

double answer(uint8_t* val_buf, uint64_t val_buf_max);

void die(char* msg) {
    dprintf(1, "diededed: %s\n", msg);
    exit(1);
}

int main() {
    uint8_t val_buf[2048];
    dprintf(1, "answer: %.2f\n", answer(val_buf, sizeof(val_buf)));
}
