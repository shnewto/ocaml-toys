type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]


let to_rna dna =
    let convert_nucleotide dna_nucleotide = 
        match dna_nucleotide with 
        | `G -> `C
        | `C -> `G
        | `T -> `A
        | `A -> `U
    in
    List.map (fun n -> convert_nucleotide n) dna
