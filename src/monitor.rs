use dashmap::DashMap;
use uuid::Uuid;

use crate::helper::interner::IStr;

pub struct Monitor {
    in_flight: DashMap<Uuid, IStr>,
}

impl Monitor {
    pub fn set_alert(&self, message: IStr) -> Uuid {
        let id = Uuid::new_v4();
        self.in_flight.insert(id, message);

        id
    }

    pub fn unset_alert(&self, which: Uuid) {
        self.in_flight.remove(&which);
    }

    pub fn instance() -> &'static Self {
        &MONITOR
    }

    pub fn emit_alerts(&self) {
        for (id, msg) in self.in_flight.clone().into_iter() {
            println!("Alert: {msg}");
        }
        println!("Done emitting alerts");
    }

    pub fn new() -> Self {
        Self { in_flight: DashMap::new() }
    }
}

lazy_static::lazy_static! {
    static ref MONITOR: Monitor = Monitor::new();
}
