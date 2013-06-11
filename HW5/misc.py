#PA 4
#CSE 130: Programming Assignment 5
#misc.py
#Author: Daniel Shipps (A10239760)
#Date: 5/22/2013


import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions

def closest_to(l,v):
    """Return the element of the list l closest in value to v.  In the case of
       a tie, the first such element is returned.  If l is empty, None is returned."""
    if len(l) <= 0:
        return "None"
    
    value = l[0]
    for e in l:
        if abs(value - v) > abs(e - v):
            value = e
    return value

def make_dict(keys,values):
    """Return a dictionary pairing corresponding keys to values."""
    dictionary = dict(zip(keys,values))
    return dictionary
   
# file IO functions
def word_count(fn):
    """Open the file fn and return a dictionary mapping words to the number
       of times they occur in the file.  A word is defined as a sequence of
       alphanumeric characters and _.  All spaces and punctuation are ignored.
       Words are returned in lower case"""
    file = open(fn)
    words = "".join([o if o.isalnum() or o == '_' else ' ' for o in file.read().lower()])
    num = {}
    for word in words.split():
        num[word] = num.get(word, 0) + 1
    return num
	








