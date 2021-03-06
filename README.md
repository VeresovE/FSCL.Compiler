FSCL.Compiler
=============

FSharp to OpenCL Compiler
-------------------------

### Latest news

*****

October 20, 2013: FSCL.Runtime and Compiler compile and execute on Mono 3.2.3 + OSX 10.8.5. 
Porting required only to add a configuration file to dll-map OSX OpenCL framework to the conventional name (OpenCL.dll) used by the .NET OpenCL wrapper. Sorry for the inconvenience.
Visual Studio solutions files can be open and built using Xamarin Studio with no effects on the repo content.

*****

October 15, 2013: FSCL.Compiler and Runtime compile correctly. Various updates to the Runtime. Sorry for the inconvenience.

*****

In the last few commits I added a new important functionality. Now it is possible to compile not only single F# kernels (a reflected function call or reference), but also expressions resulting from composing multiple kernels.
For example, if *F(x)* and *G(x,y)* are two F# kernels, programmers can compile *<@@ G(F(x), y) @@>*.

The compiler is capable of analyzing expressions containing F# kernel compositions, to produce the target code for all the kernels and to build a Call Graph that stores the dependencies between the kernels in the module.
To accomplish this task, the *KernelModule* data structure has become more complicated. In place of a kernel signature and body, now it primarly contains a call graph connecting multiple different kernels.
In addition, the *ModuleParsing* step processors are now required to produce a "raw" call graph to instantiate the kernel module. 
Simple, traditional FSCL parsing processors, like the one associated to function reference or the one that parses lambdas, produce a call graph containing only one kernel.
There are also new parsing processors, like the *CallExpression* parser, which instead produce a more complex call graph that connects multiple kernels.

We call the call graph resulting from parsing "raw" because it is only required to contain all the kernels to compile and their connections, but it is not required to contain other informations produced by the compilation pipeline, such as calls to utility functions.
The call graph is accessed and modified in multiple ways during the compilation prodcess. For example, when the return value of a kernel is replaced with an automatically-generated additional parameter (since OpenCL kernels cannot return values), eventual outbound connections from that kernel to another one (e.g. a second kernel whose i-th parameter is provided by the the return value of the first one)
are properly adapted.

Take a look to [Kernel Expressions Sample](https://github.com/GabrieleCocco/FSCL.Compiler/blob/master/KernelExpressionSample/Program.fs) and to [Accelerated Arrays Sample](https://github.com/GabrieleCocco/FSCL.Compiler/blob/master/Samples/AcceleratedArraySample/Program.fs) to get an overview of all the options that programmers can exploit to define and compile F# kernels
###FSCL documentation

A draft of the FSCL compiler and FSCL runtime specification is available: 
[FSCL compiler and runtime documentation](https://github.com/GabrieleCocco/FSCL.Compiler/blob/master/FSCL%20Documentation%20v1.0.pdf)

Note that the FSCL runtime is a separate project hosted on gihub: [FSCL runtime project](https://github.com/GabrieleCocco/FSCL.Runtime)

###At a glance

FSCL.Compiler is an F# to OpenCL compiler that allows programmers to develop OpenCL kernels inside .NET, with all the benefits of 
code-completion, type-checking and many other facilities provided by the .NET visual machine environment.
FSCL.Compiler currently supports all the features of OpenCL Specification v1.2 except for the image-related processing, which is under development.

In addition to the ability to express OpenCL-C99-like kernels in F#, FSCL.Compiler introduces many higher-level features to be used in kernel coding, such as generic types, F# structs,
F# ref cells and kernel with return types, which globally allows to increase the abstraction over "standard kernel coding".

In particular, the list of enhanced features currently supported in kernel writing are:

+ *Automatic array length*: when coding OpenCL C kernels working on arrays, the length of each array often must be explicitely passed as an additional parameter. Thanks to the power of reflection and dynamic method construction of .NET, programmers can code F# kernels without the need of passing the length of each input/output array. Whenever the length of an array is required in the kernel body, programmers can use the well-known "length" property exposed by the .NET array type. The FSCL compiler is capable of genenrating additional parameters to host the length of each input/output arrays and to replace each usage of the "length" property with a reference to the appropriate additional parameter;
+ *Ref variables*: ref variables can be used as kernel parameters to used to pass information from the kernel to the host without using arrays. Referenced values can be either primitive values, records or structs. The compiler lifts ref variables replacing them with arrays of unary length;
+ *Records and structs*: records and structs containing primitive fields can be passed to F# kernels. Struct/record parameters are processed to generate C99 struct declaration in the OpenCL kernel source;
+ *Return type*: OpenCL kernels are constrained to return no value (void). The FSCL compiler removes this contraint and allows F# kernels to return array values. When a kernel returns a value, the compiler analyzes the whole kernel body and produces a valid OpenCL C99 code with an additional buffer representing the returned data. The ability to return a value allows to define kernels as overloaded operators (e.g. a multiply operator for two matrixes, which returns a matrix);
+ *Generic kernels*: the FSCL compiler allows to declare generic F# kernels. For example, programmers can write a parallel matrix multiplication once, using generic arrays, and then run it on two integer, float, double matrices. The only contraint set by the compiler infrastructure is that generic types can be instantiated only with primitive types.
When an F# kernel containing generic parameters is compiled, FSCL produces an instance of kernel for each possible combination of primitive types assigned to each generic parameter. 

###Usage

To use the FSCL compiler to write F# kernels and to compile them into OpenCL code, programmers must:

1. Link the appropriate libraries: *FSCL.Compiler.dll* and *FSCL.Compiler.Language.dll*;
2. Open the appropriate namespaces: *FSCL.Compiler* and *FSCL.Compiler.KernelLanguage*;
3. Write the F# kernel and mark it with the *ReflectedDefinition attribute*;
4. Instantiate the *Compiler* type and call the *Compile method* passing the quotation of the function/method reference (name).

The following code sample represents a template for F# kernel definition and compilation.

    // Compiler user interface
    open FSCL.Compiler
    // Kernel language library
    open FSCL.Compiler.KernelLanguage
    // Kernel properly marked with ReflectedDefinition attribute
    [<ReflectedDefinition>]
    let VectorAdd(a: float32[], b: float32[], c: float32[]) =
        let gid = get_global_id(0)
        c.[gid] <- a.[gid] + b.[gid]
    [<EntryPoint>]
    let main argv =
        // Instantiate the compiler
        let compiler = new Compiler()
        // Compile the kernel
        let compilationResult = compiler.Compile(<@@ VectorAdd @@>)
        

