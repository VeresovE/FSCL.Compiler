﻿namespace FSCL.Compiler.FunctionTransformation

open FSCL.Compiler
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.QuotationEvaluation

[<StepProcessor("FSCL_GLOBAL_VAR_REF_TRANSFORMATION_PROCESSOR", "FSCL_FUNCTION_TRANSFORMATION_STEP",
                Dependencies = [| "FSCL_RETURN_ALLOCATION_LIFTING_TRANSFORMATION_PROCESSOR" |])>] 
type GlobalVarRefTransformation() =   
    inherit FunctionTransformationProcessor()

    override this.Run(expr, en) =
        let engine = en :?> FunctionTransformationStep
        match expr with
        | Patterns.PropertyGet(o, pi, value) ->
            // A property get can be handled only if the property has a constant value and has a reflected definition attribute
            // In this case it is considered like a C macro and its value replaces the property reference
            let attr = List.ofSeq (pi.GetCustomAttributes<ReflectedDefinitionAttribute>())
            if attr.Length > 0 then
                match pi with
                | DerivedPatterns.PropertyGetterWithReflectedDefinition(e) ->
                    let freeVars = List.ofSeq(e.GetFreeVars())
                    if freeVars.IsEmpty then
                        e
                    else
                        engine.Default(expr)
                        //raise (KernelTransformationException("Only global variables initialized with a constant right value can be referenced inside a kernel [" + pi.Name + "]"))
                | _ ->
                    //raise (KernelTransformationException("Only global variables marked with ReflectedDefinitionAttribute can be referenced inside a kernel [" + pi.Name + "]"))
                    engine.Default(expr)
            else             
                engine.Default(expr)             
        | _ ->
            engine.Default(expr)
