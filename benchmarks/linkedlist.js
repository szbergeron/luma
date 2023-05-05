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


const s = new LinkedIntSet();

let total_iters = 100;

let contained = 0;
for (let v = 0; v < total_iters; v = v + 1) {
    const iters = 100000;

    let rands = [];

    for (let i = 0; i < iters; i++) {
        const v = Math.floor(Math.random() * iters);
        //const v = i;
        rands.push(v);
    }

    start = new Date();

    for (let i = 0; i < iters; i++) {
        //const v = Math.floor(Math.random() * iters);
        const v = rands.pop();

        const was_new = s.insert(v);

        if (!was_new) {
          contained++;
        }
    }

    end = new Date();

    elapsed_secs = (end - start) / 1000.0;

    console.log(elapsed_secs)
}

console.log(`pre-contained count: ${contained}`);
