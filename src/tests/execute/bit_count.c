/*
 * Testing of some bit count functions.
 * Functions taken from:
 * http://stackoverflow.com/questions/109023/how-to-count-the-number-of-set-bits-in-a-32-bit-integer
 */
#include <stdio.h>

int NumberOfSetBits(int i)
{
     i = i - ((i >> 1) & 0x55555555);
     i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
     return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

int pop(unsigned x)
{
    x = x - ((x >> 1) & 0x55555555);
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
    x = (x + (x >> 4)) & 0x0F0F0F0F;
    x = x + (x >> 8);
    x = x + (x >> 16);
    return x & 0x0000003F;
}

int bitcount1(int n) {
    int c; // c accumulates the total bits set in n
    for (c = 0; n; c++)
        n = n & n-1; // clear the least significant bit set
    return c;
}

int bitcount2(unsigned x)
{
    int b;

    for (b = 0; x != 0; x >>= 1)
        if (x & 01)
            b++;
    return b;
}

static unsigned char byte_bit_count[256];  /* lookup table */
void initialize_count_bits(void)
{
  int cnt, i, data;

  for( i = 0; i < 256; i++ ) {
    cnt = 0;
    data = i;
    while( data != 0 ) {
      data = data & (data - 1);
      cnt++;
    }
    byte_bit_count[i] = cnt;
  }
}
int bitcount3( unsigned int data )
{
  const unsigned char * dword = ( unsigned char *) & data;

  return byte_bit_count[dword[0]] + byte_bit_count[dword[1]] +
         byte_bit_count[dword[2]] + byte_bit_count[dword[3]];
}

int bitcount4(unsigned int num){
    int count = 0;
    static int nibblebits[] =
        {0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4};
    for(; num != 0; num >>= 4)
        count += nibblebits[num & 0x0f];
    return count;
}

unsigned int bitcount5(unsigned int x)
{
    switch (x) {
        case 0:
            return 0;
        case 1:
            return 1;
        case 2:
            return 1;
        case 3:
            return 2;
        default:
            return bitcount5(x/4) + bitcount5(x%4);
    }
}

int bitcount6(int n) {
    return !n ? 0 : 1 + bitcount6(n & (n-1));
}

int main(void)
{
    unsigned val = 0xDEADBEEF; // 0xDEADBEEF = 11011110 10101101 10111110 11101111 (24-bits set)
    printf("%d\n", NumberOfSetBits(val));
    printf("%d\n", pop(val));
    printf("%d\n", bitcount1(val));
    printf("%d\n", bitcount2(val));
    initialize_count_bits();
    printf("%d\n", bitcount3(val));
    printf("%d\n", bitcount4(val));
    printf("%d\n", bitcount5(val));
    printf("%d\n", bitcount6(val));

    return 0;
}
