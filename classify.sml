
structure NaiveBayes :> NAIVE_BAYES_CLASSIFIER =
struct

    type category = string

    type labeled_document = category * string Seq.seq
    type document = string Seq.seq
        
    type statistics = 
          (category,int) Dict.dict           (* maps each category to number of documents with that category *)
        * (category,int) Dict.dict           (* maps each category to number of words in documents with that category *)
        * (category * string, int) Dict.dict (* maps each (cat,word) to frequency *)
        * category Seq.seq                   (* list of categories (no duplicates) *)
        * int                                (* total number of documents *)
        * int                                (* total number of different words *)

    (* TASK *)


    fun compareStringPairs ((s1,s2),(s3,s4)):order=
        String.compare(s1^s2,s3^s4)

    fun gather (train : labeled_document MR.mapreducable) : statistics = 
        let 
        val numdocincat=ExtractCombine.extractcombine(String.compare,
                                              fn (cat,doc) => Seq.singleton((cat,1)),
                                              Int.+, 
                                              train)
                           
         val numwordswithcat=ExtractCombine.extractcombine(String.compare,
                                              fn (cat,doc) => Seq.singleton((cat,Seq.length(doc))),
                                              Int.+, 
                                              train)
                                              
         val catwordtofreq=ExtractCombine.extractcombine(compareStringPairs,
                                              fn (cat,doc) => Seq.map (fn word => ((cat,word), 1),doc),
                                              Int.+, 
                                              train)

        val cats=Seq.map(fn (cat,int)=>cat,Dict.toSeq(numwordswithcat))
        val docs=Seq.reduce(fn(int1,int2)=>int1+int2,0,Seq.map(fn(cat,int)=>int,Dict.toSeq(numdocincat)))

        
        val words=Dict.size(ExtractCombine.extractcombine(String.compare,
                                                          fn (cat,doc) => Seq.map(fn word => (word, 1),doc),
                                                          Int.+,
                                                          train))

        in
          (numdocincat,numwordswithcat,catwordtofreq,cats,docs,words)
        end

    (* TASK *)

    fun probdocincat (num_docs_by_cat,total_num_docs)=
      Math.ln(Real.fromInt(num_docs_by_cat)/Real.fromInt(total_num_docs))

  fun probwordincat (freqs,num_words_by_cat,word,cat,total_num_words):real= case Dict.lookup(compareStringPairs,freqs,(cat,word)) of
                                                                        NONE=>Math.ln(1.0/Real.fromInt(total_num_words))
                                                                        |SOME x=> Math.ln(Real.fromInt(x)/Real.fromInt(Dict.lookup'(String.compare,num_words_by_cat,cat)))
    fun possible_classifications 
        ((num_docs_by_cat,
          num_words_by_cat,
          freqs,
          all_categories, 
          total_num_docs,
          total_num_words) : statistics,
         test_doc : document) : (category * real) Seq.seq = 
        
        Seq.map(fn(cat,num)=>(cat,probdocincat(num,total_num_docs)+
                              Seq.mapreduce(fn word=> probwordincat(freqs,num_words_by_cat,word,cat,total_num_words),0.0,Real.+,test_doc)),Dict.toSeq(num_docs_by_cat))
                                                                                        

    (* TASK *)
    fun classify (stats : statistics, test_doc : document) : (category * real) = 
      let val classifications=possible_classifications(stats,test_doc)
      in
        Seq.reduce(fn((cat1,real1),(cat2,real2))=>case Real.compare(real1,real2) of
                                                  EQUAL=>(cat1,real1)      (*If they are equal does it matter what I output*)  
                                                  |LESS=>(cat2,real2)
                                                  |GREATER=>(cat1,real1),("",Real.negInf),classifications)
      end

    (* TASK *)
    fun train_classifier (train : labeled_document MR.mapreducable) : document -> (category * real) =
        let 
          val stats=gather(train)
          in 
            fn doc=>classify(stats,doc)
            end
        
end

(*

Small - 5,8 (~ 62.5%)
Medium - 680,808 (~ 84.16%)
Big - 70122,78899 (~ 88.88%)

So it seems like the bigger the sample size the more accurate the machine learning will be. If there are more samples to
scan through it will have a much more accurate sample to map the new articles to.

*)