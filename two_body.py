import math
import random
from typing import List

class Body(object):
    def distance(self, other):
        delta_x = self.x - other.x
        delta_y = self.y - other.y

        dxp = math.pow(delta_x, 2.0)
        dyp = math.pow(delta_y, 2.0)

        return math.sqrt(dxp + dyp)

class Vec2(object):
    x = 0.0
    y = 0.0
    
    def scale(self, scale):
        n = Vec2()
        n.x = self.x * scale
        n.y = self.y * scale
        
        return n
    
    def __add__(self, other):
        n = Vec2()
        
        n.x = self.x + other.x
        n.y = self.y + other.y
        
        return n

class Simulation(object):
    bodies = []

def r_hat(a: Body, b: Body) -> Vec2:
    x = (a.x - b.x) / a.distance(b)
    y = (a.y - b.y) / a.distance(b)
    
    v = Vec2()
    
    v.x = x
    v.y = y
    
    return v

def grav_from(on: Body, from_others: List[Body]) -> Vec2:
    sum = Vec2()
    
    for other in from_others:
        G = 6.6743
        
        top = G * on.mass * other.mass
        bottom = math.pow(on.distance(other), 2.0)
        rh = r_hat(on, other)
        
        if bottom != 0.0:
            scale = top / bottom
            scaled = rh.scale(scale)
            
            sum = sum + scaled
            
    return sum

def generate_body(x: float, y: float, mass: float) -> Body:
    b = Body()
    b.x = x
    b.y = y
    b.mass = mass
    b.v_x = math.cos(x)
    b.v_y = math.cos(y)
    
    return b

def simulate_nbody(s: Simulation) -> List[List[Vec2]]:
    results = []
    
    for sample in range(0, s.samples):
        sample_one = []
        for i in range(0, len(s.bodies)):
            cur_body = s.bodies[i]
            
            others = []

            for j in range(0, len(s.bodies)):
                if j != i:
                    others.append(s.bodies[j])
                    
            fv = grav_from(cur_body, others)
            
            cur_body.v_x = cur_body.v_x + (fv.x / cur_body.mass) # f = ma, a = f / m
            cur_body.v_y = cur_body.v_y + (fv.y / cur_body.mass)

            cur_body.x = cur_body.x + (cur_body.v_x * s.step_time)
            cur_body.y = cur_body.y + (cur_body.v_y * s.step_time)
            
            r = Vec2()
            r.x = cur_body.x
            r.y = cur_body.y

            sample_one.append(r)
            
        results.append(sample_one)
        
bodies = []

for i in range(0, 100):
    x = random.uniform(-1000.0, 1000.0)
    y = random.uniform(-1000.0, 1000.0)
    mass = random.uniform(0.0, 100.0)
    
    body = generate_body(x, y, mass)
    
    bodies.append(body)
    
s = Simulation()

s.bodies = bodies
s.samples = 10000
s.step_time = 0.1

simulate_nbody(s)