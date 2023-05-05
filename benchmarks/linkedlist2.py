import random
import time

class LinkedNode:
    def __init__(self):
        self.val = None
        self.next = None

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

s = LinkedIntSet()

total_iters = 100

contained = 0
for v in range(total_iters):
    iters = 100000

    rands = []

    for i in range(iters):
        v = random.randint(0, iters - 1)
        rands.append(v)

    start_time = time.time()

    for i in range(iters):
        v = rands.pop()

        was_new = s.insert(v)

        if not was_new:
          contained += 1

    end_time = time.time()

    print(f"time taken for iteration {v}: {end_time - start_time:.6f}")

print(f"pre-contained count: {contained}")
