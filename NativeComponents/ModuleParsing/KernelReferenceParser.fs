namespace FSCL.Compiler.ModuleParsing

open System
open FSCL.Compiler
open FSCL.Compiler.Core.Util
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations

[<assembly:DefaultComponentAssembly>]
do()

[<StepProcessor("FSCL_REFERENCE_PARSING_PROCESSOR", "FSCL_MODULE_PARSING_STEP")>]
type KernelReferenceParser() =      
    inherit ModuleParsingProcessor()
    
    override this.Run(expr, en) =
        let engine = en :?> ModuleParsingStep
        if (expr :? Expr) then
            match QuotationAnalysis.GetKernelFromName(expr :?> Expr) with
            | Some(mi, b) -> 
                // Create module
                let km = new KernelModule(mi, b)
                Some(km)
            | _ ->
                None
        else
            None
            