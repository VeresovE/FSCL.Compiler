﻿namespace FSCL.Compiler.ModuleCodegen

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open FSCL.Compiler

[<assembly:DefaultComponentAssembly>]
do()

[<Step("FSCL_MODULE_CODEGEN_STEP",
       Dependencies = [| "FSCL_FUNCTION_CODEGEN_STEP";
                         "FSCL_FUNCTION_TRANSFORMATION_STEP";
                         "FSCL_FUNCTION_PREPROCESSING_STEP";
                         "FSCL_MODULE_PREPROCESSING_STEP";
                         "FSCL_MODULE_PARSING_STEP" |])>]
type ModuleCodegenStep(tm: TypeManager, 
                       processors: ICompilerStepProcessor list) = 
    inherit CompilerStep<KernelModule, KernelModule * String>(tm, processors)
        
    override this.Run(k) =
        let state = ref ""
        for p in processors do
            state := p.Execute((k, !state), this) :?> string
        (k, !state)