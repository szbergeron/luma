use std::time::Instant;
use std::io::Write;

use crate::rand_f64;

#[derive(Copy, Clone)]
struct Body {
    x: f64,
    y: f64,
    v_x: f64,
    v_y: f64,
    mass: f64,
}

impl Body {
    fn distance(&self, b: &Body) -> f64 {
        let delta_x = self.x - b.x;
        let delta_y = self.y - b.y;
        (delta_x.powf(2.0) + delta_y.powf(2.0)).sqrt()
    }
}

/*
 * F = (G m_1 m_2) / r^2 by Newton's law of gravitation
 *
 * r_v = - ((1 + q) / r^2) * r_h
 *
 */

#[derive(Copy, Clone)]
struct ForceVector {
    x: f64,
    y: f64,

}

impl ForceVector {
    fn from_vec2(v2: Vec2) -> ForceVector {
        ForceVector { x: v2.x, y: v2.y }
    }
}

fn grav_from(on: &Body, from: &Vec<Body>) -> ForceVector {
    let mut v2 = Vec2 { x: 0.0, y: 0.0 };

    for other in from {
        //std::print("doing newton stuff");
        // do newton stuff
        //let other = from.get(i);

        let big_g = 6.6743;

        let top = big_g * on.mass * other.mass;
        let bottom = on.distance(other).powf(2.0);
        let r_hat = r_hat(on, other);

        if (bottom != 0.0) { // prevent NaNs
            let scale = top / bottom;

            let scaled = r_hat.scale(scale);

            v2 = v2 + scaled;
        }
    }

    ForceVector::from_vec2(v2)

    //struct usr::ForceVector { x: 0.0, y: 0.0 }
}

// compute the unit vector from a to b
fn r_hat(a: &Body, b: &Body) -> Vec2 {
    let x = (a.x - b.x) / a.distance(b);
    let y = (a.y - b.y) / a.distance(b);

    Vec2 { x, y }
}


fn generate_body(x: f64, y: f64, mass: f64) -> Body {
    let b = Body { x, y, mass, v_x: x.cos(), v_y: y.cos() };

    b
}

struct Simulation {
    bodies: Vec<Body>,
    samples: usize,
    step_time: f64,
}

#[derive(Copy, Clone, Debug)]
struct Vec2 {
    x: f64,
    y: f64,
}

impl Vec2 {
    fn scale(self, by: f64) -> Vec2 {
        Vec2 { x: self.x * by, y: self.y * by }
    }
}

impl std::ops::Add for Vec2 {
    type Output = Vec2;
    fn add(self, rhs: Self) -> Self::Output {
        Vec2 { x: self.x + rhs.x, y: self.y + rhs.y }
    }
}

/// don't inline so we can inspect ASM
#[inline(never)]
fn simulate_nbody(s: &mut Simulation) -> Vec<Vec<Vec2>> {
    //let results = std::Vec::with_capacity(s.samples);
    //let results = std::Vec::new();
    let mut results = Vec::new();

    //println!("simulating bodies");
    let bodies = &mut s.bodies;
    for _ in 0..s.samples {
        let mut sample_one = Vec::new();

        //std::print("doing sample " + sample.to_string());
        //for (let i = 0; i < bodies.len(); i = i + 1) {
        for i in 0..bodies.len() {
            //std::print("doing body " + i.to_string());


            //let mut other_bodies = vec![];

            //for (let j = 0; j < bodies.len(); j = j + 1) {
            for j in 0..bodies.len() {
                //std::print("pushing body " + j.to_string());
                if i != j {
                    //other_bodies.push(*bodies.get(j).unwrap());
                }
            }

            
            //let fv = struct ForceVector { x: 0.0, y: 0.0 };
            let fv = grav_from(&bodies[i], &bodies);

            let cur_body = bodies.get_mut(i).unwrap();

            cur_body.v_x = cur_body.v_x + (fv.x / cur_body.mass); // f = ma, a = f / m
            cur_body.v_y = cur_body.v_y + (fv.y / cur_body.mass);

            cur_body.x = cur_body.x + (cur_body.v_x * s.step_time);
            cur_body.y = cur_body.y + (cur_body.v_y * s.step_time);

            //std::print("new body x is " + cur_body.x.to_string());
            //std::print("new body y is " + cur_body.y.to_string());
            sample_one.push(Vec2 { x: cur_body.x, y: cur_body.y });
        }

        results.push(sample_one);
    }

    results
}

pub fn entry() {
    /*let b1 = usr::generate_body(10.0, 20.0, 5.0);
    let b2 = usr::generate_body(30.0, 10.0, 3.0);
    let b3 = usr::generate_body(0.0, 10.0, 1.0);*/

    let mut b = vec![];

    for i in 0..100 {
        let x = rand_f64();
        let y = rand_f64();
        let mass = rand_f64().abs() % 100.0;
        let body = generate_body(x, y, mass);

        b.push(body);
    }

    let mut s = Simulation { bodies: b, samples: 100000, step_time: 0.1 };

    //s.samples = 1000000;
    //s.samples = 10000;

    //s.results = std::Vec::new();
    
    //s.pr = std::print;
    //s.pr = usr::simulate_nbody;

    for i in 0..100 {
        let start = Instant::now();

        let result = simulate_nbody(&mut s);

        let end = Instant::now();

        let dur = end - start;

        //dur.print("Time to complete simulation");
        println!("{}", dur.as_secs_f64());

        
        let mut stdn = std::io::sink();
        writeln!(stdn, "result of final state:").unwrap();
        writeln!(stdn, "{:?}", result.get(result.len() - 1)).unwrap();
    }



}
