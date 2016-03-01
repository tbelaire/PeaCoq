let userTacticsGroupName = "PeaCoq user tactics";

class TacticGroupNode extends ProofTreeNode {
  name: string;
  parent: GoalNode;
  solved: boolean;
  span: JQuery;
  tacticIndex: number;
  tactics: Tactic[];

  constructor(
    proofTree: ProofTree,
    parent: GoalNode,
    name: string
  ) {
    super(proofTree, just(parent));
    this.name = name;
    this.parent = parent;
    this.solved = false;
    this.tactics = [];
    this.tacticIndex = 0;
  }

  click() {
    alert("TODO: click");
  }

  getAllDescendants() {
    let children: GoalNode[] = _(this.tactics).map((t) => t.goals).flatten<GoalNode>().value();
    let descendants = _(children).map((c) => c.getAllDescendants()).flatten<ProofTreeNode>().value();
    return [].concat(children, descendants);
  }

  getAllGoalDescendants() {
    let children: GoalNode[] = _(this.tactics).map((t) => t.goals).flatten<GoalNode>().value();
    let descendants = _(children).map((c) => c.getAllGoalDescendants()).flatten<GoalNode>().value();
    return [].concat(children, descendants);
  }

  getFocusedChild(): Maybe<ProofTreeNode> {
    let viewChildren: ProofTreeNode[] = this.getViewChildren();
    if (viewChildren.length === 0) { return nothing(); }
    return just(viewChildren[this.tactics[this.tacticIndex].goalIndex]);
  }

  getFocusedTactic(): Maybe<Tactic> {
    return (
      this.tactics.length === 0
        ? nothing()
        : just(this.tactics[this.tacticIndex])
    );
  }

  getParent(): Maybe<GoalNode> { return just(this.parent); }

  getTactics(): Tactic[] {
    return this.tactics;
  }

  getViewChildren(): GoalNode[] {
    if (this.isSolved()) { return []; }
    if (this.tactics.length === 0) { return []; }
    let focusedTactic = this.tactics[this.tacticIndex];
    return focusedTactic.goals;
  }

  isSolved(): boolean {
    return this.getFocusedTactic().caseOf({
      nothing: () => false,
      just: (t) => t.isSolved(),
    });
  }

  nodeWidth(): number { return this.proofTree.getTacticWidth(); }

  onChildSolvedAndUnfocused(): void {
    let focusedTactic = fromJust(this.getFocusedTactic());
    let unsolved = _(focusedTactic.goals)
      .find(function(elt) {
        return !elt.solved;
      })
      ;
    if (unsolved === undefined) {
      this.onSolved();
    } else {
      this.proofTree.curNode = unsolved;
      this.proofTree.refreshTactics();
      this.proofTree.update();
    }
  }

  onSolved(): void {
    let self = this;
    this.solved = true;
    this.proofTree.curNode = this.parent;
    this.proofTree.update()
      .then(function() {
        self.parent.onChildSolved();
      })
      ;
  }

  shiftNextInGroup() {
    if (this.tacticIndex < this.tactics.length - 1) {
      this.tacticIndex++;
      //asyncLog("NEXTGROUPFOCUS " + nodeString(this.tactics[this.tacticIndex]));
      this.proofTree.update();
    }
  }

  shiftPrevInGroup() {
    if (this.tacticIndex > 0) {
      this.tacticIndex--;
      //asyncLog("PREVGROUPFOCUS " + nodeString(this.tactics[this.tacticIndex]));
      this.proofTree.update();
    }
  }

}