class Body {
    distance(other) {
        let delta_x = this.x - other.x;
        let delta_y = this.y - other.y;

        let dxp = Math.pow(delta_x, 2.0);
        let dyp = Math.pow(delta_y, 2.0);

        return Math.sqrt(dxp + dyp);
    }
}

class Vec2 {
    constructor() {
        this.x = 0.0;
        this.y = 0.0;
    }

    scale(scale) {
        let n = new Vec2();
        n.x = this.x * scale;
        n.y = this.y * scale;

        return n;
    }

    add(other) {
        let n = new Vec2();
        n.x = this.x + other.x;
        n.y = this.y + other.y;

        return n;
    }
}

class Simulation {
    constructor() {
        this.bodies = [];
    }
}

function r_hat(a, b) {
    let x = (a.x - b.x) / a.distance(b);
    let y = (a.y - b.y) / a.distance(b);

    let v = new Vec2();

    v.x = x;
    v.y = y;

    return v;
}

function grav_from(on, from_others) {
    let sum = new Vec2();

    for (let i = 0; i < from_others.length; i++) {
        let other = from_others[i];
        let G = 6.6743;

        let top = G * on.mass * other.mass;
        let bottom = Math.pow(on.distance(other), 2.0);
        let rh = r_hat(on, other);

        if (bottom !== 0.0) {
            let scale = top / bottom;
            let scaled = rh.scale(scale);

            sum = sum.add(scaled);
        }
    }

    return sum;
}

function generate_body(x, y, mass) {
    let b = new Body();
    b.x = x;
    b.y = y;
    b.mass = mass;
    b.v_x = Math.cos(x);
    b.v_y = Math.cos(y);

    return b;
}

function simulate_nbody(s) {
    let results = [];

    let last_sample = [];

    for (let sample = 0; sample < s.samples; sample++) {
        let sample_one = [];
        for (let i = 0; i < s.bodies.length; i++) {
            let cur_body = s.bodies[i];

            //let others = [];

            /*
            for (let j = 0; j < s.bodies.length; j++) {
                if (j !== i) {
                    others.push(s.bodies[j]);
                }
            }
            */

            let fv = grav_from(cur_body, s.bodies);

            cur_body.v_x = cur_body.v_x + (fv.x / cur_body.mass); // f = ma, a = f / m
            cur_body.v_y = cur_body.v_y + (fv.y / cur_body.mass);

            cur_body.x = cur_body.x + (cur_body.v_x * s.step_time);
            cur_body.y = cur_body.y + (cur_body.v_y * s.step_time);

            let r = new Vec2();
            r.x = cur_body.x;
            r.y = cur_body.y;

            sample_one.push(r);
        }

        results.push(sample_one);
    }

    return results;
}

let bodies = [];

for (let i = 0; i < 100; i++) {
    let x = Math.random() * 2000 - 1000;
    let y = Math.random() * 2000 - 1000;
    let mass = Math.random() * 100;

    let body = generate_body(x, y, mass)

    bodies.push(body)

}

s = new Simulation()

s.bodies = bodies
s.samples = 100000
s.step_time = 0.1

let l = 0;

for (let i = 0; i < 100; i++) {
    //console.time()

    start = new Date();

    let r = simulate_nbody(s);

    end = new Date();

    elapsed_secs = (end - start) / 1000.0;

    console.log(elapsed_secs)


    //console.timeEnd()

    l += r.length;

    //console.log(r.pop());

}
