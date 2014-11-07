#!/usr/bin/python3

import random
import sys

def printRandomParameters(n):
  for _ in range(n):
    first = random.randrange(-10, 2)
    second = round(random.uniform(1, 2), 2)
    thirdNumber = random.randrange(0,2)
    third = True if thirdNumber == 1 else False

    print(str(first) + ' ' + str(second) + ' ' + str(third).lower())

if __name__ == "__main__":
  printRandomParameters(int(sys.argv[1]))
