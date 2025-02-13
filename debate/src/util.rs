pub enum DetectUnrecognized<A, E> {
    Unrecognized(A),
    Error(E),
}
