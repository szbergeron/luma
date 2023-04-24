import random

class LinkedNode:
    def __init__(self):
        self.next = None
        #self.prev = None
        pass

class LinkedList:
    def __init__(self):
        self.head = None
        self.tail = None

    def new_last(self):
        n = LinkedNode()

        if self.head is None:
            self.head = n
            self.tail = n
        else:
            self.tail.next = n
            self.tail = n

        return n

class LinkedIntSet:
    def __init__(self):
        self.ll = LinkedList()

    def insert(self, val):
        if self.contains(val):
            return False
        else:
            self.ll.new_last().val = val
            return True

    def contains(self, val):
        h = self.ll.head
        while h is not None:
            if h.val == val:
                return True
            else:
                h = h.next
        return False

    @staticmethod
    def new():
        return LinkedIntSet()

s = LinkedIntSet.new()

contained = 0
iters = 10000

for i in range(iters):
    v = int(random.random() * iters)

    was_new = s.insert(v)

    if not was_new:
        contained += 1

print(f"pre-contained count: {contained}")


