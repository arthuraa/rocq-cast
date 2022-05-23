From mathcomp Require Import ssreflect ssrfun ssrbool eqtype.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Declare Scope cast_scope.
Delimit Scope cast_scope with cast.

Definition cast T S (e : T = S) : T -> S :=
  match e with erefl => id end.

Definition eapp (T S : Type) (f : T -> S) x y (e : x = y) : f x = f y :=
  match e with erefl => erefl end.

Notation "1" := (erefl) : cast_scope.

Notation "p * q" := (etrans p q)
  (at level 40, left associativity)
  : cast_scope.

Notation "p ^-1" := (esym p) : cast_scope.

Notation "e @[ x ]" := (cast e x)
  (at level 25, right associativity, format "e @[ x ]")
  : cast_scope.

Notation "f @@ e" := (eapp f e)
  (at level 25, right associativity)
  : cast_scope.

Local Open Scope cast_scope.

Definition castK T S (e : T = S) : cancel (cast e) (cast e^-1) :=
  match e with 1 => fun x => 1 end.

Definition castKV T S (e : T = S) : cancel (cast e^-1) (cast e) :=
  match e with 1 => fun x => 1 end.

Definition castD T S R (e1 : T = S) (e2 : S = R) (x : T) :
  (e1 * e2)@[x] = e2@[e1@[x]] :=
  match e2 with 1 => 1 end.

Definition eappV T S (f : T -> S) x y (e : x = y) : (f @@ e)^-1 = f @@ e^-1 :=
  match e with erefl => erefl end.

Definition eappCE (T S : Type) (a : S) x y (e : x = y) :
  (fun _ : T => a) @@ e = erefl :=
  match e with erefl => erefl end.

Definition eappD T S (f : T -> S) x y z (exy : x = y) (eyz : y = z) :
  f @@ (exy * eyz) = f @@ exy * f @@ eyz :=
  match eyz with erefl => erefl end.

Definition eapp_comp T S R (g : S -> R) (f : T -> S) x y (exy : x = y) :
  (g \o f) @@ exy = g @@ f @@ exy :=
  match exy with erefl => erefl end.

Definition etransV T (x y z : T) (p : x = y) (q : y = z) : (p * q)^-1 = q^-1 * p^-1 :=
  match p in _ = y return forall q : y = z, (p * q)^-1 = q^-1 * p^-1 with
  | erefl => fun q => match q with erefl => erefl end
  end q.

Lemma etransA T (x y z w : T) (p : x = y) (q : y = z) (r : z = w) :
  p * (q * r) = p * q * r.
Proof. by case: w / r; case: z / q. Qed.

Definition etrans1p T (x y : T) (p : x = y) : 1 * p = p :=
  match p with erefl => erefl end.

Definition etransp1 T (x y : T) (p : x = y) : p * 1 = p :=
  match p with erefl => erefl end.

Definition etransVp T (x y : T) (p : x = y) : p^-1 * p = erefl :=
  match p with erefl => erefl end.

Definition etranspV T (x y : T) (p : x = y) : p * p^-1 = erefl :=
  match p with erefl => erefl end.

Lemma etranspK T x y z (p : x = y :> T) :
  cancel (@etrans _ x y z p) (@etrans _ y x z p^-1).
Proof. by case: y / p; case: z /. Qed.

Lemma etransKp T x y z (p : y = z :> T) :
  cancel (@etrans _ x y z ^~ p) (@etrans _ x z y ^~ p^-1).
Proof. by case: z / p; case: y /. Qed.

Lemma etranspI T (x y z : T) (p : x = y) : injective (@etrans _ x y z p).
Proof. exact: can_inj (etranspK p). Qed.

Lemma etransIp T (x y z : T) (p : y = z) : injective (@etrans _ x y z ^~ p).
Proof. exact: can_inj (etransKp p). Qed.

Lemma etrans_nat T S (f g : T -> S) (efg : forall x, f x = g x) x y (exy : x = y) :
  f @@ exy * efg y = efg x * g @@ exy.
Proof. by case: y / exy; rewrite etrans1p. Qed.

Definition eapp2 T1 T2 S (f : T1 -> T2 -> S) x1 y1 x2 y2 (e1 : x1 = y1) (e2 : x2 = y2) : f x1 x2 = f y1 y2 :=
  eapp (f x1) e2 * eapp (fun a => f a y2) e1.

