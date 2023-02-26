/// Yes, it's a reference, and no, it's not a good one
///
/// This handles the work of doing inference on dynamic fields
/// given a type. It uses a stepped model, where all assignments
/// to a field are grouped based on staged visibility.
///
/// Quark on every node first runs a round, and tells
/// Transponster what assignments were made to any fields
/// on it within the initial round. Then, after all
/// Quarks have done their initial round and signaled Operator,
/// a message is sent to the Transponsters asking them
/// to say how much information they were given
/// about how to infer the types of their fields.
