namespace Lost.Research.DataStructures

open System
open System.Diagnostics.Contracts
open System.Numerics

module private Impl =
    // TODO: B-like tree

    [<Literal>]
    let private baseNodeWeight = 0x40000000

    let inline private calculateWeight delta =
        if delta >= 0 then
            Int32.MinValue - baseNodeWeight >>> delta
        else
            baseNodeWeight >>> (-delta)

    let inline testBit (value: bigint) bit =
        value.ToByteArray().[bit >>> 3] &&& (1uy <<< (bit &&& 7)) <> 0uy

    type Node() =
        /// right over left advantage
        let mutable disbalance = 0
        let mutable rightWeight = baseNodeWeight
        /// node over subtree advantage
        let mutable usefulnessDelta = 0
        let mutable nodeWeight = baseNodeWeight

        let mutable left = None
        let mutable right = None

        static member private getOrCreateNode node =
            match node with
            | None -> Node()
            | Some(node) -> node

        member private this.Disbalance
            with set value =
                disbalance <- Compare.fitBetween -30 30 value
                rightWeight <- calculateWeight disbalance

        member private this.UsefulnessDelta
            with set value =
                usefulnessDelta <- Compare.fitBetween -30 30 value
                nodeWeight <- calculateWeight usefulnessDelta

        member private this.GetLeftValue(value: bigint, levelSize) = value + levelSize
        member private this.GetRightValue(value: bigint, levelSize) =
            value + levelSize + levelSize

        [<Pure>]
        member this.GetRandom(random: Random, value: bigint, levelSize) =
            if random.Next() < nodeWeight then value
            else
                if random.Next() >= rightWeight then
                    let leftValue = this.GetLeftValue(value, levelSize)
                    let leftNode = Node.getOrCreateNode left
                    leftNode.GetRandom(random, leftValue, levelSize + levelSize)
                else
                    let rightValue = this.GetRightValue(value, levelSize)
                    let rightNode = Node.getOrCreateNode right
                    rightNode.GetRandom(random, rightValue, levelSize + levelSize)

        member this.Useful(value, currentValue, significantBit, levelSize) =
            if value = currentValue then
                let maximized = usefulnessDelta = 30
                this.UsefulnessDelta <- usefulnessDelta + 1
                maximized
            else
                let leftValue = this.GetLeftValue(currentValue, levelSize)
                this.UsefulnessDelta <- usefulnessDelta - 1
                let nextLevelSize = levelSize + levelSize

                if testBit leftValue significantBit = testBit value significantBit then
                    let leftNode = Node.getOrCreateNode left
                    left <- Some(leftNode)

                    let needToFix = leftNode.Useful(value, leftValue, significantBit + 1, nextLevelSize)

                    if needToFix then
                        let leftmost = disbalance = -30
                        this.Disbalance <- disbalance - 1
                        leftmost
                    else false
                else
                    let rightNode = Node.getOrCreateNode right
                    right <- Some(rightNode)
                    let rightValue = leftValue + levelSize

                    let needToFix = rightNode.Useful(value, rightValue, significantBit + 1, nextLevelSize)

                    if needToFix then
                        let rightmost = disbalance = 30
                        this.Disbalance <- disbalance + 1
                        rightmost
                    else false

open Impl

type CountableUsefulnessTree() =
    let random = Random()

    let zero = Node()

    [<Pure>]
    member this.GetRandom() = zero.GetRandom(random, 0I, 1I)
    member this.Useful(value) = zero.Useful(value, 0I, 0, 1I) |> ignore

    interface ICountableUsefulness with
        [<Pure>]
        member this.GetRandom() = this.GetRandom()
        member this.Useful(value) = this.Useful(value)

