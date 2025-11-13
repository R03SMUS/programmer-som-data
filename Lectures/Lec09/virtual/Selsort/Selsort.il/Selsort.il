	.method public hidebysig static 
		void SelectionSort (
			int32[] arr
		) cil managed 
	{
		// Method begins at RVA 0x20cc
		// Header size: 12
		// Code size: 78 (0x4e)
		.maxstack 4
		.locals init (
			[0] int32,
			[1] int32,
			[2] int32,
			[3] int32,
			[4] bool,
			[5] bool,
			[6] bool
		)

		IL_0000: nop                                                            // no operation
		IL_0001: ldc.i4.0                                                       // push 0 to stack [0]
		IL_0002: stloc.0                                                        // pop stack and store in local var 0 (i)
		IL_0003: br.s IL_0041                                                   // branch to IL_0041
		// loop start (head: IL_0041)
			IL_0005: nop                                                          // no operation
			IL_0006: ldloc.0                                                      // load 0 and push to stack [i]
			IL_0007: stloc.1                                                      // store top of stack in local var 1 []
			IL_0008: ldloc.0                                                      // load 0 and push to stack [i,]
			IL_0009: ldc.i4.1                                                     // push 1 to stack [i, 1]
			IL_000a: add                                                          // add i + 1 [i+1] 
			IL_000b: stloc.3                                                      // store stack in local var 3 (j)
			IL_000c: br.s IL_0022                                                 // branch to IL_0022
			// loop start (head: IL_0022)
				IL_000e: ldarg.0                                                    // load function argument 0 to stack [arr]
				IL_000f: ldloc.3                                                    // load local 3 and push to stack [arr, j]
				IL_0010: ldelem.i4                                                  // get elem from arr ( arr[j] )
				IL_0011: ldarg.0                                                    // load function argument 0 to stack [arr[j], arr]
				IL_0012: ldloc.1                                                    // load local var 1 and push to stack [arr[j], arr, least]
				IL_0013: ldelem.i4                                                  // get elem from arr ( arr[least] ) [arr[j], arr[least]]
				IL_0014: clt                                                        // comapre two of the stack (<) [result]
				IL_0016: stloc.s 4                                                  // store stack into local var 4 []
				IL_0018: ldloc.s 4                                                  // load local var 4 and push to stack [result]
				IL_001a: brfalse.s IL_001e                                          // branch if false

				IL_001c: ldloc.3                                                    // load local var 3 and push to stack [j]
				IL_001d: stloc.1                                                    // store stack in local var 1 []

				IL_001e: ldloc.3                                                    // load local var 3 and push to stack [j]
				IL_001f: ldc.i4.1                                                   // push 1 to stack [j, 1]
				IL_0020: add                                                        // add [j+1]
				IL_0021: stloc.3                                                    // store stack in local var 3 []

				IL_0022: ldloc.3                                                    // load local var 3 and push to stack [j]
				IL_0023: ldarg.0                                                    // load function arguemnt [j, arr]
				IL_0024: ldlen                                                      // load len of top of stack [j, arrlen]
				IL_0025: conv.i4                                                    // convert to int and push to stack [j, int(arrlen)]
				IL_0026: clt                                                        // compare the top of stack
				IL_0028: stloc.s 5                                                  // store result in localvar 5 
				IL_002a: ldloc.s 5                                                  // load local var 5 and push to stack
				IL_002c: brtrue.s IL_000e                                           // branch to IL_000e if true
			// end loop

			IL_002e: ldarg.0                                                      // load function argument 0 [arr]
			IL_002f: ldloc.0                                                      // load local var 0 and push to stack [arr, i]
			IL_0030: ldelem.i4                                                    // get elem i from arr
			IL_0031: stloc.2                                                      // store in local var 2                                                      
			IL_0032: ldarg.0                                                      // load function argument 0 [arr]
			IL_0033: ldloc.0                                                      // load local var 0 and push to stack [arr, i]
			IL_0034: ldarg.0                                                      // load function argument 0 [arr, i, arr]
			IL_0035: ldloc.1                                                      // load local var 1 and push to stack [arr, i, arr, least]
			IL_0036: ldelem.i4                                                    // load elemt least from arr [arr, i, arr[least]]
			IL_0037: stelem.i4                                                    // replace elemnt []
			IL_0038: ldarg.0                                                      // load function argument 0 [arr]
			IL_0039: ldloc.1                                                      // load local var 1 and push to stack [arr, ]
			IL_003a: ldloc.2                                                      // load local var 2 and push to stack [arr, ] 
			IL_003b: stelem.i4                                                    // replace element arr 
			IL_003c: nop                                                          // no operation
			IL_003d: ldloc.0                                                      // load local var 0 and push to stack [i]
			IL_003e: ldc.i4.1                                                     // push 1 to stack [i, 1]
			IL_003f: add                                                          // add [i+1]
			IL_0040: stloc.0                                                      // store top of stack in local var 0

			IL_0041: ldloc.0                                                      // load local var 0 and push to stack [i]
			IL_0042: ldarg.0                                                      // load function argument 0 [i, arr]
			IL_0043: ldlen                                                        // get len of top of stack [i, len(arr)]
			IL_0044: conv.i4                                                      // convert to int [i, int(len(arr))]
			IL_0045: clt                                                          // comapre
			IL_0047: stloc.s 6                                                    // store in local var 6
			IL_0049: ldloc.s 6                                                    // load local var 6 and push to stack
			IL_004b: brtrue.s IL_0005                                             // branch if true 
		// end loop

		IL_004d: ret                                                            // return
	} // end of method Selsort::SelectionSort


