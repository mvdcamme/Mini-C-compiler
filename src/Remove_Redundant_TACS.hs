module Remove_Redundant_TACS where

  import ThreeAddressCode

  shouldIncludeTAC :: TAC -> Bool
  shouldIncludeTAC (AsnCode (InAddr addr1) (OutAddr addr2)) = addr1 /= addr2
  shouldIncludeTAC _ = True

  removeRedundantTACS :: TACs -> TACs
  removeRedundantTACS tacs = filter shouldIncludeTAC tacs

  optimiseFunctionTAC :: FunctionTAC -> FunctionTAC
  optimiseFunctionTAC functionTAC =
    let optimisedFTACBody = removeRedundantTACS $ fTACBody functionTAC
    in functionTAC { fTACBody = optimisedFTACBody }

  optimiseTACFile :: TACFile -> TACFile
  optimiseTACFile tacfile =
    let optimisedFunctionTACs = map optimiseFunctionTAC $ functionDefinitions tacfile
        optimisedMainFunctionTAC = fmap optimiseFunctionTAC $ mainFunctionDefinition tacfile
    in tacfile { functionDefinitions = optimisedFunctionTACs, mainFunctionDefinition = optimisedMainFunctionTAC }