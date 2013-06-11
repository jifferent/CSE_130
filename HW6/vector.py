"""
CSE 130: Programming Assignment 6
vector.py
Author: Daniel Shipps (A10239760)
Date: 5/31/2013
"""

from misc import Failure

class Vector(object):
    """This is class that implements a fixed length vector
    and which implements a variety of operations :)"""

    def __init__(self, length):
        """constructor"""
        try:
            if length < 0:
                raise ValueError("You can't have a negative length!")
        except TypeError:
            pass

        try:
            self.data = length * [0.0]
        except Exception:
            self.data = list(length)

    def __repr__(self):
        """Return a string of python code which could be used to
        initialize the Vector."""
        return "Vector(" + repr(self.data) + ")"

    def __len__(self):
        """returns the length of the Vector."""
        return len(self.data)

    def __iter__(self):
        """Return an object that can iterate over the elements
        of the Vector."""
        for it in self.data:
            yield it

    def __add__(self, other):
        """implements the binary + operation"""
        return Vector(([a + b for a, b in zip(list(self), list(other))]))

    def __iadd__(self, other):
        """implements the binary += operation"""
        self.data =  Vector(([a + b for a, b in zip(list(self), list(other))]))
        return self.data

    def __radd__(self, other):
        """implements the binary + when left operand does not support
        the corresponding operation and the operands are of different types"""
        return Vector(([a + b for a, b in zip(list(self), list(other))]))

    def dot(self, other):
        """ takes either a Vector or a sequence and returns the dot product
        of the argument with current Vector instance. The dot product is
        defined as the sum of the component-wise products. The behavior of
        this function if any elements are not numeric is undefined."""
        try:
            return sum([a*b for a,b in zip(self, other)])
        except:
            return sum([str(a) + str(b) for a,b in zip(self, other)])

    def __getitem__(self, index):
        """Gets the data value at index."""
        return self.data[index]

    def __setitem__(self, index, value):
        """Sets the data value at index."""
        self.data[index] = value

    def __lt__(self, other):
        """Limplements the binary < operation"""
        if not isinstance(other, Vector):
            return self >= other
        for a,b in self.list_reverse(other):
            if not a >= b:
                return True
        return False

    def __gt__(self, other):
        """Limplements the binary > operation"""
        if not isinstance(other, Vector):
            return self > other
        for a,b in self.list_reverse(other):
            if not a > b:
                return False
        return True

    def __ge__(self, other):
        """Limplements the binary >= operation"""
        if not isinstance(other, Vector):
            return self >= other
        for a,b in self.list_reverse(other):
            if not a >= b:
                return False
        return True

    def list_reverse(self, other):
        """return the vector in descending order"""
        return zip(sorted(self, reverse=True), sorted(other, reverse=True))    
                
