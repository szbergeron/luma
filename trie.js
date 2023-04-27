class TrieNode {
    constructor() {
        this.children = new Map();
        this.is_final = false;
        this.string = '';
    }

    contains(s) {
        if (s.length === 0) {
            return this.is_final;
        } else {
            //let c = '' + s.charAt(0) + s.charAt(0);
            let c = s.charAt(0);
            if (this.children.has(c)) {
                return this.children.get(c).contains(s.slice(1));
            } else {
                return false;
            }
        }
    }
}

class Trie {
    constructor() {
        this.root = new TrieNode();
    }

    insert(s) {
        this.insertRec(this.root, s, s);
    }

    insertRec(atNode, remain, s) {
        if (remain.length === 0) {
            atNode.is_final = true;
            atNode.string = s;
        } else {
            // V8 cheats here and doesn't actually
            // store this as a hashmap unless we store the keys as
            // strings instead of chars. Technically this
            // is a "good" optimization, but it defeats
            // the actual computational cost of this benchmark,
            // so we force this specific deopt
            //let c = '' + remain.charAt(0) + remain.charAt(0); // force storage as a string for fairness
            //
            // this opt actually got deopt'd by a different
            // change (we weren't using the resulting count)
            // so V8 is acting fair again and we can let it
            // store the pure chars again
            let c = remain.charAt(0);
            if (!atNode.children.has(c)) {
                atNode.children.set(c, new TrieNode());
            }
            let child = atNode.children.get(c);
            this.insertRec(child, remain.slice(1), s);
        }
    }
}

let total_iters = 100;
let contained = 0;

for (let v = 0; v < total_iters; v = v + 1) {
    let randStrings = [];

    let samples = 1000000;

    for (let i = 0; i < samples; i++) {
        let strlen = Math.floor(Math.random() * 6) + 3;

        let rstring = "";

        for (let j = 0; j < strlen; j++) {
            rstring = rstring + Math.random().toString(36).slice(2, 3);
        }
        //console.log(rstring);

        randStrings.push(rstring);
    }

    let before = new Date();

    let t = new Trie();

    for (let i = 0; randStrings.length > 0; i++) {
        let s = randStrings.pop();
        if (t.root.contains(s)) {
            contained++;
        } else {
            t.insert(s);
        }
    }

    let after = new Date();

    let elapsed_secs = (after - before) / 1000.0;

    console.log(elapsed_secs)
}

console.log(`contains of: ${contained}`);
console.log(`time to do trie things: ${after - before} ms`);
