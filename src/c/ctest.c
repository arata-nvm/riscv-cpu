int main() {
  volatile const unsigned int x = 1;
  volatile const unsigned int y = 2;
  volatile unsigned int z = x + y;
  if (z == 1) {
    z = z + 1;
  } else {
    z = z + 2;
  }
  asm volatile("unimp");
  return 0;
}
