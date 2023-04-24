class LinkedNode {
  constructor() {
      this.val = null;
  }
}

class LinkedList {
  constructor() {
    this.head = null;
    this.tail = null;
  }

  new_last() {
    const n = new LinkedNode();

    if (this.head == null) {
      this.head = n;
      this.tail = n;
    } else {
      this.tail.next = n;
      this.tail = n;
    }

    return n;
  }
}

class LinkedIntSet {
  constructor() {
    this.ll = new LinkedList();
  }

  insert(val) {
    if (this.contains(val)) {
      return false;
    } else {
      this.ll.new_last().val = val;
      return true;
    }
  }

  contains(val) {
    let h = this.ll.head;

    while (h != null) {
      if (h.val === val) {
        return true;
      } else {
        h = h.next;
      }
    }

    return false;
  }
}

console.time();

const s = new LinkedIntSet();

let contained = 0;
const iters = 100000;

for (let i = 0; i < iters; i++) {
    //const v = Math.floor(Math.random() * iters);
    const v = i;

    const was_new = s.insert(v);

    if (!was_new) {
      contained++;
    }
}

console.log(`pre-contained count: ${contained}`);

console.timeEnd();
