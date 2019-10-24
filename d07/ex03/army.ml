class ['a] army =
  object
    val soldiers : 'a list = []
    method add s = {< soldiers = s::soldiers >}
    method delete = match soldiers with
      | [] -> failwith "soldiers list is empty"
      | _::tl -> {< soldiers = tl >}
  end
