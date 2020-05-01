(* This functor is a valid TrainerMaker for a single layer net, that is, it can be used
   to update an net if given: [In],  input data, [Out] output data, and a [Derivative] a 
   Derivative module, all defined in trainer.ml 
*)
module Make : Trainer.TrainerMaker
