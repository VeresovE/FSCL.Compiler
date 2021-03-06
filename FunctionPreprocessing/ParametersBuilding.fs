﻿namespace FSCL.Compiler.FunctionPreprocessing

open FSCL.Compiler
open FSCL.Compiler.KernelLanguage
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Quotations
open System

[<StepProcessor("FSCL_ARGS_BUILDING_PREPROCESSING_PROCESSOR", "FSCL_FUNCTION_PREPROCESSING_STEP")>] 
type ArgumentsBuildingProcessor() =        
    inherit FunctionPreprocessingProcessor()

    override this.Run(fInfo, en) =
        // Get kernel info
        let kernelInfo = fInfo
        // Get kernel signature
        let methodInfo = kernelInfo.ID

        // Process each parameter
        for p in methodInfo.GetParameters() do
            // Create parameter info
            let parameterEntry = new KernelParameterInfo(p.Name, p.ParameterType)
            // Set var to be used in kernel body
            parameterEntry.Placeholder <- Some(Quotations.Var(p.Name, p.ParameterType, false))
            // Determine the memory space of this parameter via custom attributes
            try
                let constantAttribute = p.GetCustomAttribute<ConstantAttribute>()
                let localAttribute = p.GetCustomAttribute<LocalAttribute>()
                if constantAttribute <> null then
                    parameterEntry.AddressSpace <- KernelParameterAddressSpace.ConstantSpace
                elif localAttribute <> null then
                    parameterEntry.AddressSpace <- KernelParameterAddressSpace.LocalSpace
                else    
                    parameterEntry.AddressSpace <- KernelParameterAddressSpace.GlobalSpace 
            with 
                | :? NotSupportedException ->
                    Console.WriteLine("Warning - [FSCL.Compiler] - [AcceleratedCollections] - Constant and Local attributes are not available in lambda functions");
                    parameterEntry.AddressSpace <- KernelParameterAddressSpace.GlobalSpace 
                    
            // If the parameter is not an array set the access mode to read
            if not (p.GetType().IsArray) then
                parameterEntry.Access <- ReadOnly
            // Add the parameter to the list of kernel params
            fInfo.Parameters.Add(parameterEntry)

            
