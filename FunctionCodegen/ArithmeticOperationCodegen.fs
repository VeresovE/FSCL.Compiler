﻿namespace FSCL.Compiler.FunctionCodegen

open FSCL.Compiler
open Microsoft.FSharp.Quotations
open System.Reflection

///
///<summary>
///The function codegen step processor whose behavior is to produce the target code for arithmetic and logic operations
///</summary>
///  
[<StepProcessor("FSCL_ARITH_OP_CODEGEN_PROCESSOR", "FSCL_FUNCTION_CODEGEN_STEP")>]
type ArithmeticOperationCodegen() =
    inherit FunctionBodyCodegenProcessor()

    let HandleBinaryOp (op, a:Expr list, engine:FunctionCodegenStep) =
        "(" + engine.Continue(a.[0]) + ")" + op + "(" + engine.Continue(a.[1]) + ")"
    let HandleUnaryOp (op, a:Expr list, engine:FunctionCodegenStep) =
        op + engine.Continue(a.[0])
        
    ///
    ///<summary>
    ///The method called to execute the processor
    ///</summary>
    ///<param name="fi">The AST node (expression) to process</param>
    ///<param name="en">The owner step</param>
    ///<returns>
    ///The target code for the arithmetic or logic expression if the AST node can be processed (i.e. if the source node is an arithmetic or logic expression expression)
    ///</returns>
    ///  
    override this.Run(expr, en) =
        let engine = en :?> FunctionCodegenStep
        match expr with 
        | Patterns.Call(o, mi, args) ->
            let returnTags = engine.FunctionInfo.CustomInfo.["RETURN_EXPRESSIONS"] :?> Expr list
            let returnPrefix = 
                if (List.tryFind(fun (e:Expr) -> e = expr) returnTags).IsSome then
                    "return "
                else
                    ""
            let returnPostfix = if returnPrefix.Length > 0 then ";\n" else ""

            match expr with
            | DerivedPatterns.SpecificCall <@ (>) @> (e, t, a) -> 
                Some(returnPrefix + HandleBinaryOp(" > ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (<) @> (e, t, a)  -> 
                Some(returnPrefix + HandleBinaryOp(" < ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (>=) @> (e, t, a)  -> 
                Some(returnPrefix + HandleBinaryOp(" >= ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (<=) @> (e, t, a)  -> 
                Some(returnPrefix + HandleBinaryOp(" <= ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (=) @> (e, t, a) -> 
                Some(returnPrefix + HandleBinaryOp(" == ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (<>) @> (e, t, a) -> 
                Some(returnPrefix + HandleBinaryOp(" != ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (+) @> (e, t, a) -> 
                Some(returnPrefix + HandleBinaryOp(" + ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (*) @> (e, t, a) -> 
                Some(returnPrefix + HandleBinaryOp(" * ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (-) @> (e, t, a) -> 
                Some(returnPrefix + HandleBinaryOp(" - ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (/) @> (e, t, a) -> 
                Some(HandleBinaryOp(" / ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (%) @> (e, t, a) -> 
                Some(returnPrefix + returnPrefix + HandleBinaryOp(" % ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (&&) @> (e, t, a) -> 
                Some(returnPrefix + HandleBinaryOp(" && ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (||) @> (e, t, a) ->
                Some(returnPrefix + HandleBinaryOp(" || ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (&&&) @> (e, t, a) -> 
                Some(returnPrefix + HandleBinaryOp(" & ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (|||) @> (e, t, a) -> 
                Some(returnPrefix + HandleBinaryOp(" | ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (^^^) @> (e, t, a) -> 
                Some(returnPrefix + HandleBinaryOp(" ^ ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (~~~) @> (e, t, a) -> 
                Some(returnPrefix + HandleUnaryOp(" ~ ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (not) @> (e, t, a) -> 
                Some(returnPrefix + HandleUnaryOp(" ! ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (>>>) @> (e, t, a) -> 
                Some(returnPrefix + HandleBinaryOp(" >> ", a, engine) + returnPostfix)
            | DerivedPatterns.SpecificCall <@ (<<<) @> (e, t, a) -> 
                Some(returnPrefix + HandleBinaryOp(" << ", a, engine) + returnPostfix)
            | _ ->
                None
        | _ ->
            None