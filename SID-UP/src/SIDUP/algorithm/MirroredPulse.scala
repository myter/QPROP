package SIDUP.algorithm

class MirroredPulse(override val changedSources : Set[Admitter.NodeRef] = Set[Admitter.NodeRef](),val steadyValue : Any = null,val state : PulseState.Value = PulseState.pending, val changeState : ChangeState.Value = ChangeState.unchanged) extends PulseValue(changedSources,steadyValue)