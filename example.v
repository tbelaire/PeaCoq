(*begin code*)
Theorem n_plus_z : forall n, n + 0 = n.
Proof.
    intros n.
    induction n.
    reflexivity.
    simpl.
    rewrite IHn.
    reflexivity.
Qed.
(*end code*)
