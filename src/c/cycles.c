static inline int rdcycle() {
    int cycle;
    asm volatile ("rdcycle %0" : "=r" (cycle));
    return cycle;
}

int main() {
    int start = rdcycle();
    for (volatile int i = 0; i < 10; i++);
    int end = rdcycle();
    asm volatile("unimp");
    return 0;
}
