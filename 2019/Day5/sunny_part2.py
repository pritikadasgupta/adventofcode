#extract the code from the file and load it into an array for processing
input1 = open("exercise5.input.txt", "r")
code = input1.read()
input1.close()
code = code.split(",")

#the array elements are currently strings; turn them into integers
for i in range(len(code)):
    code[i] = int(code[i])

#function to return a five-element array to represent the intcode
def intcodeArray(int1):
    intcode = list(str(int1))
    if(len(intcode)<5):
        for i in range(5-len(intcode)):
            intcode.insert(0,"0")
    for i in range(len(intcode)):
        intcode[i] = int(intcode[i])
    return intcode

##inst[4] and inst[3] are the opcode
##inst[2] is the mode of the first parameter
##inst[1] is the mode of the second parameter
##inst[0] is the mode of the third parameter

#function to return the value of a position, given its mode
def value(mode, position):
    val = 0
    if(mode==0):
        val = code[code[position]]
    else:
        val = code[position]
    return val

pos = 0

#run the intcode
while(code[pos]!=99):
    
    inst = intcodeArray(code[pos])

    #add two values
    if(inst[4]==1):
        two = pos+1
        three = pos+2
        four = code[pos+3]
        
        code[four] = value(inst[2],two) + value(inst[1],three)
        pos += 4

    #multiply two values
    elif(inst[4]==2):     
        two = pos+1
        three = pos+2
        four = code[pos+3]

        code[four] = value(inst[2],two) * value(inst[1],three)
        pos += 4

    #take an input value
    elif(inst[4]==3):
        
        print("What is your input value?")
        choice = input()
        choice = int(choice)
        
        if(inst[2]==0):
            code[code[pos+1]] = choice
        else:
            code[pos+1] = choice
            
        pos += 2

    #print an output value
    elif(inst[4]==4):
        print(value(inst[2],(pos+1)))
        pos += 2

    #if first parameter != 0, jump to position given by the second parameter
    elif(inst[4]==5):
        
        two = pos+1
        three = pos+2
        
        if(value(inst[2],two)!=0):
            pos = value(inst[1],three)
        else:
            pos += 3

    #if first parameter is zero, jump to position given by the second parameter
    elif(inst[4]==6):
        
        two = pos+1
        three = pos+2
        
        if(value(inst[2],two)==0):
            pos = value(inst[1],three)
        else:
            pos += 3

    #based on whether first parameter < second parameter,
    #store 1 or 0 in the position given by the third parameter
    elif(inst[4]==7):
        
        two = pos+1
        three = pos+2
        four = code[pos+3]
        
        if(value(inst[2],two) < value(inst[1],three)):
            code[four] = 1
        else:
            code[four] = 0
        pos += 4

    #based on whether first parameter == second parameter,
    #store 1 or 1 in the position given by the third parameter.
    elif(inst[4]==8):
        
        two = pos+1
        three = pos+2
        four = code[pos+3]
        
        if(value(inst[2],two) == value(inst[1],three)):
            code[four] = 1
        else:
            code[four] = 0
        pos += 4