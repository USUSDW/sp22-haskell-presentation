def myFunc1() -> int:
    print("Hello from function 1!")
    return 1

def myFunc2() -> int:
    print("Hello from function 2!")
    return 2

def getRandomNumber() -> int:
    # Wow, crazy how I rolled two 4's in a row
    return 4

if __name__ == "__main__":
    roll = getRandomNumber()
    func1Result = myFunc1()
    func2Result = myFunc2()
    if roll <= 3:
        # We don't really do anything with this value...
        func1Result     
    else:
        # Same with this one
        func2Result
