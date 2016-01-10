/* source: http://austingwalters.com/radix-sort-in-c/ */

/**
 * Redix Sort written in C by Austin G Walters for austingwalters.com
 */

#include <stdio.h>

void printArray(int * array, int size){

  int i;
  printf("[ ");
  for (i = 0; i < size; i++)
    printf("%d ", array[i]);
  printf("]\n");
}

int findLargestNum(int * array, int size){

  int i;
  int largestNum = -1;

  for(i = 0; i < size; i++){
    if(array[i] > largestNum)
      largestNum = array[i];
  }

  return largestNum;
}

// Radix Sort
void radixSort(int * array, int size){
  // Base 10 is used
  int i;
  int semiSorted[12];//size];
  int significantDigit = 1;
  int largestNum = findLargestNum(array, size);

  printf("\n\nRunning Radix Sort on Unsorted List!\n\n");

  // Loop until we reach the largest significant digit
  while (largestNum / significantDigit > 0){
    int bucket[10] = { 0 };

    printf("\tSorting: %d's place ", significantDigit);
    printArray(array, size);

    // Counts the number of "keys" or digits that will go into each bucket
    for (i = 0; i < size; i++)
      bucket[(array[i] / significantDigit) % 10]++;

    /**
     * Add the count of the previous buckets,
     * Acquires the indexes after the end of each bucket location in the array
		 * Works similar to the count sort algorithm
     **/
    for (i = 1; i < 10; i++)
      bucket[i] += bucket[i - 1];

    // Use the bucket to fill a "semiSorted" array
    for (i = size - 1; i >= 0; i--)
      semiSorted[--bucket[(array[i] / significantDigit) % 10]] = array[i];


    for (i = 0; i < size; i++)
      array[i] = semiSorted[i];

    // Move to next significant digit
    significantDigit *= 10;

    printf("\n\tBucket: ");
    printArray(bucket, 10);

    printf("%d/%d = %d\n", largestNum, significantDigit, largestNum/significantDigit);
  }
}

int main(void){
  int size = 12;
  int list[] = {10, 2, 303, 4021, 293, 1, 0, 429, 480, 92, 2999, 14};

  printf("\n\nRunning Radix Sort Example in C!\n");
  printf("----------------------------------\n");

  printf("\nUnsorted List: ");
  printArray(&list[0], size);

  radixSort(&list[0], size);

  printf("\nSorted List:");
  printArray(&list[0], size);
  printf("\n");

  return 0;
}
