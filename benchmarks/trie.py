import math
import random
from typing import List
import time
import sys


class TrieNode(object):
    def __init__(self):
        self.children = {}
        self.is_final = False
        self.string = ''

    def contains(self, s: str):
        if len(s) == 0:
            return self.is_final
        else:
            c = s[0]
            if c in self.children:
                return self.children[c].contains(s[1:])
            else:
                return False


class Trie(object):
    def __init__(self):
        self.root = TrieNode()

    def insert(self, s: str):
        self.insert_rec(self.root, s, s)

    def insert_rec(self, atNode: TrieNode, remain: str, s: str):
        if len(remain) == 0:
            atNode.is_final = True
            atNode.string = s
        else:
            c = remain[0]
            if c not in atNode.children.keys():
                atNode.children[c] = TrieNode()
            child = atNode.children[c]
            self.insert_rec(child, remain[1:], s)


total_iters = 100
contained = 0

for v in range(total_iters):
    rand_strings = []
    samples = 1000000

    for i in range(samples):
        strlen = random.randint(3, 8)

        rstring = ""
        for i in range(strlen):
            rstring = rstring + random.choice('abcdefghijklmnopqrstuvwxyz')

        rand_strings.append(rstring)

    before = time.time()

    t = Trie()

    while rand_strings:
        s = rand_strings.pop()
        if t.root.contains(s):
            contained += 1
        else:
            t.insert(s)

    after = time.time()

    elapsed_secs = after - before

    print(elapsed_secs)
    sys.stdout.flush()

print(f"contains of: {contained}")
sys.stdout.flush()
