(** An attempt to create a typechecker for a small DSL that works over large RDDs *)

(**
  Scala spark RDDs and Dataframes,
  - have runttime typing at best checked at runtime
  - no encoding or hints of wide and narrow operations
  - types do not really help reasoniing about spark application behaviour
  - action vs transformation distinction help somewhat
 *)

(**
  create a language
   - descriptive of data manipulations
   - strictly typed
   - types allow for reasoning relevant properties of the comutation
 *)

(**
  Our language will use two features:
     - Row polymorphic records
     - linear types for encoding resource usage.

  The interaction of the two typing feature might not be trivial
  and we want to prove some correctness properties of a the typing relation
*)

(**
  A. Row type polimorphism
  - Traditional way of encoding database values, tables rows
  - with important operations like project join
 *)

(** Starting from Simply Typed Lamba Calculus
   - first we add the record extensions
   - then linear types
 The second step is actually a full rewrite of the interpretation,
 not a simple extension.
*)

(**
   possible ways of application:
    - toy interpreter for csv extracted/implemented in Haskell (with the nice properties)
    - standalone interpreter and compiler in scala for spark csvScheme expressions are to be compiled/transpiled to scala
    - embeddable monadic / tagless-final interpreter(s) for scala applications.
 *)

Set Warnings "-notation-overridden,-parsing,-deprecated-hint-without-locality".
From Coq Require Import Bool.Bool.
From Coq Require Import Arith.Arith.
From Coq Require Import Init.Nat.
From Coq Require Import Arith.PeanoNat. Import Nat.
From Coq Require Import Arith.EqNat.
From Coq Require Import Lia.
From Coq Require Import Lists.List. Import ListNotations.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import String.
From Coq Require Import List.


Definition env: Type := nat.

Inductive expr : Type :=
  | Var : string -> expr
  | App : expr -> expr -> expr
  | Lam : string ->  expr -> expr.

Inductive ty : Type :=
  | TInt: nat -> ty
  | TFn : ty -> ty
  | TVar : string -> ty.

Inductive value : Type :=
  | Closure : string -> env -> expr -> value
  | Int : nat -> value.

Fixpoint eval (e : expr) : value :=
  match e with
  | Var v => Int 0
  | App e1 e2 => Int 1
  | Lam v body => Int 2
  end.

Reserved Notation " t '==>' n " (at level 50, left associativity).

Inductive Reval: expr -> value -> Prop :=
  | E_Var: forall s, Reval (Var s) (Int 0)
| E_App: forall e1 e2: expr,Reval (App e1 e2) (Int 1)
| E_Lam: forall v: string, forall b: expr, Reval (Lam v b) (Int 2)
  where " t '==>' n " := (eval t n).
