#CSE 130: Programming Assignment 5
#crack.py
#Author: Daniel Shipps (A10239760)
#Date: 5/22/2013


from misc import *
import crypt

def product(*args, **kwds):
    # product('ABCD', 'xy') --> Ax Ay Bx By Cx Cy Dx Dy
    # product(range(2), repeat=3) --> 000 001 010 011 100 101 110 111
    pools = map(tuple, args) * kwds.get('repeat', 1)
    result = [[]]
    for pool in pools:
        result = [x+[y] for x in result for y in pool]
    for prod in result:
        yield tuple(prod)

def load_words(filename,regexp):
    """Load the words from the file filename that match the regular
       expression regexp.  Returns a list of matching words in the order
       they are in the file."""
    file = open(filename)
    match = []
    expr = re.compile(regexp)
    for word in file.readlines():
        word = word.strip()
        if expr.match(word):
            match.append(word)
    return match

def transform_reverse(str):
    """transform_reverse(str) should return a list with the original string and the reversal of the original string"""
    return [str, ''.join(reversed(str))]

def transform_capitalize(str):
    """transform_capitalize(str) should return a list of all the possible ways to capitalize the input string."""
    return [''.join(cap) for cap in product(*zip(str.lower(), str.upper()))]

def transform_digits(str):
    """transform_digits(str) should return return a list of all possible ways to replace letters with similar looking digits according to the following mappings."""
    equiv = {
        'o': '0',
        'i': '1',
        'l': '1',
        'z': '2',
        'e': '3',
        'a': '4',
        's': '5',
        'b': '6',
        't': '7',
        'b': '8',
        'g': '9',
        'q': '9'
        }
    dict_equiv = dict(equiv)
    dict_equiv['b'] = '8'
    temp1 = str
    temp2 = ''.join([equiv.get(char, char) for char in temp1.lower()])
    temp3 = ''.join([dict_equiv.get(char, char) for char in temp1.lower()])
    return list(set(''.join(char) for char in product(*zip(temp1, temp2, temp3))))

def check_pass(plain,enc):
    """check_pass(plain,enc) which will take a plain-text password, plain, and an encrypted password enc and will return True if plain encrypts to enc using the function crypt."""
    return crypt.crypt(plain, enc[0:2]) == enc

def load_passwd(filename):
    """Load the password file filename and returns a list of
       dictionaries with fields "account", "password", "UID", "GID",
       "GECOS", "directory", and "shell", each mapping to the
       corresponding field of the file."""
    result = []
    file = open(filename)
    for line in file.readlines():
        if not line:
            continue
        entry = {}
        (entry['account'], entry['password'], entry['UID'], entry['GID'],
        entry['GECOS'], entry['directory'], entry['shell']) = line.split(':')
        result.append(entry)
    return result

def crack_pass_file(fn_pass,words,out):
    """Crack as many passwords in file fn_pass as possible using words
       in the file words"""
    long = []
    passes = load_passwd(fn_pass)
    regex = r'.{6,8}'
    words = load_words(words, regex)
    with open(out, 'w') as of:
        for entry in passes:
            long.append(entry)
            username = entry['account']
            encoded = entry['password']
            for plain in words:
                if check_pass(plain, encoded):
                    long.pop()
                    of.write('{}={}\n'.format(username, plain))

        s = set()
        for word in words:
            for w in transform_capitalize(word):
                s.add(w)
            for w in transform_digits(word):
                s.add(w)
        words = list(s)

# Code for second run that will cause a memery error if left running.
 #       for entry in long:
 #           username = entry['account']
 #           encoded = entry['password']
 #           for plain in words:
 #               if check_pass(plain, encoded):
 #                   of.write('{}={}\n'.format(username, plain))
    return
