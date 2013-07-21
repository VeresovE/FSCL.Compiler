namespace FSCL.Compiler.ModuleParsing

open FSCL.Compiler
open FSCL.Compiler.Core.Util
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations

[<StepProcessor("FSCL_LAMBDA_PARSING_PROCESSOR", "FSCL_MODULE_PARSING_STEP", Dependencies = [| "FSCL_REFERENCE_PARSING_PROCESSOR" |])>]
type KernelLambdaParser() =      
    inherit ModuleParsingProcessor()
        
    override this.Run(mi, en) =
        let engine = en :?> ModuleParsingStep
        if (mi :? Expr) then
            match QuotationAnalysis.LambdaToMethod(mi :?> Expr) with
            | Some(mi, b) -> 
                // Create module
                let km = new KernelModule(mi, b)
                Some(km)
            | _ ->
                None
        else
            None
            