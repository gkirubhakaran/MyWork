 # Print the longest substring of a string that has no repeating characters
 def findLongestSubstring(str):
    n=len(str)
    op=[]
    for j in range (0,n):
        str_loop=str[j:n]
        m=len(str_loop)
        for i in range (0,m):
            t=str_loop[0:i]
            if (str_loop[i] in t):
                break
        op.append(t)
    max_length=max(len(x) for x in op)
    for x in op:
        if (len(x)==max_length):
           print x
           
