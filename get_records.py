#!/usr/bin/python
import random
import sys

educationList = ['Secondary', 'Vocational', 'Undergraduate', 'Graduate', 'Doctor']

if __name__ == '__main__':
  number = int(sys.argv[1])
  for _ in range(number):
    education = random.choice(educationList)
    age = random.randrange(18, 51)
    isMarried = random.choice(['true', 'false'])
    levelOfTrust = str(random.randrange(0, 4))
    riskOfCancer = random.randrange(50,101)
    creditability = str(random.randrange(0, 4))
    print(education + ' ' + str(age) + ' ' + isMarried + ' ' + levelOfTrust + ' ' +
        str(riskOfCancer) + ' ' + creditability)

