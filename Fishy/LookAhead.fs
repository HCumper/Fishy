module LookAhead

open Types
open System.Diagnostics
open GenerateMoves
open Evaluation
open MakeMove
open TranspositionTable
open Fishy
open System.Collections.Generic

let rec addOneLevel (tree: Tree) (value: int) : Tree =
    match tree with
    | Leaf(_, move) ->
        let mutable newChildren = []
        for i = 1 to value do
            newChildren <- Node(value, move, SortedSet<Tree>([], Comparer<Tree>.Default)) :: newChildren
        Node(value, move, newChildren)
    | Node(v, m, children) ->
        let newChildren = SortedSet<Tree>([], Comparer<Tree>.Default)
        for child in children do
            addOneLevel child value
            |> newChildren.Add
        Node(v, m, newChildren)

let buildNTreeIterative : Tree =
    Leaf(0, defaultMove)

let tree = buildNTreeIterative
let newTree = addOneLevel tree 4
