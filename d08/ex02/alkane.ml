class alkane n =
  object
    inherit Molecule.molecule (
        (match n with
         | 1 -> "Meth"
         | 2 -> "Eth"
         | 3 -> "Prop"
         | 4 -> "But"
         | 5 -> "Pent"
         | 6 -> "Hex"
         | 7 -> "Hept"
         | 8 -> "Oct"
         | 9 -> "Non"
         | 10 -> "Dec"
         | 11 -> "Undec"
         | 12 -> "Dodec"
         | _ -> invalid_arg "n should be between 1 and 12")
        ^ "ane",
        List.init n (fun _ -> new Atom.carbon) @
        List.init (2 * n + 2) (fun _ -> new Atom.hydrogen)
      )
  end

class methane =
  object
    inherit alkane 1
  end

class ethane =
  object
    inherit alkane 2
  end

class octane =
  object
    inherit alkane 8
  end
