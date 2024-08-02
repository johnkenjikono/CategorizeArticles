
structure ExtractCombine :> EXTRACT_COMBINE =
struct

    fun extractcombine (cmp:'k * 'k -> order,
                        ext:'a -> ('k * 'v) Seq.seq,
                        comb:'v * 'v -> 'v,
                        docs:'a MR.mapreducable) : ('k,'v) Dict.dict= 

            MR.mapreduce(fn doc=>Seq.mapreduce(fn(word,x)=>let val dict=Dict.empty 
                                                    in Dict.insert(cmp,dict,(word,x))
                                                        end,Dict.empty,fn(dictA,dictB)=>Dict.merge(cmp,comb,dictA,dictB),ext(doc))
            
            ,Dict.empty,fn(dict1,dict2)=>Dict.merge(cmp,comb,dict1,dict2),docs)
    
end
