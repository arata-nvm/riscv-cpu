int main() {
  volatile char *uart_tx = (volatile char *)0x10000000;
  *uart_tx = 'H';
  *uart_tx = 'e';
  *uart_tx = 'l';
  *uart_tx = 'l';
  *uart_tx = 'o';
  *uart_tx = ' ';
  *uart_tx = 'W';
  *uart_tx = 'o';
  *uart_tx = 'r';
  *uart_tx = 'l';
  *uart_tx = 'd';
  *uart_tx = '!';
  *uart_tx = '\n';
  asm volatile("unimp");
  return 0;
}
