let rec hfs_f = function
  | n when n < 0 -> -1
  | 0 -> 1
  | n -> n - hfs_m (hfs_f (n - 1))

and hfs_m = function
  | n when n < 0 -> -1
  | 0 -> 0
  | n -> n - hfs_f (hfs_m (n - 1))
