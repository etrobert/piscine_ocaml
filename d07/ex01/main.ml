let () =
  let jean = new People.people ("Jean") in
  print_endline jean#to_string;
  jean#talk;
  jean#die;
  let doc = new Doctor.doctor ("Tom", 42, jean) in
  print_endline doc#to_string;
  doc#talk;
  let doc = doc#travel_in_time 2015 2020 in
  print_endline doc#to_string;
  doc#use_sonic_screwdriver
