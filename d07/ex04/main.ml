let rec destroy_them_all (a:'a Army.army) =
  print_endline "one down...";
  destroy_them_all (a#delete)

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
  doc#use_sonic_screwdriver;
  Random.self_init ();
  let dalek = new Dalek.dalek in
  print_endline dalek#to_string;
  dalek#talk;
  dalek#exterminate jean;
  print_endline dalek#to_string;

  let people_army = new Army.army in
  let people_army = people_army#add jean in
  let people_army = (((people_army#add jean)#add jean)#add jean)#add jean in
  (try
     destroy_them_all people_army
   with e -> print_endline ("Exception caught: " ^ Printexc.to_string e));

  let dalek_army = new Army.army in
  let dalek_army = dalek_army#add dalek in
  let dalek_army = (((dalek_army#add dalek)#add dalek)#add dalek)#add dalek in
  (try
     destroy_them_all dalek_army
   with e -> print_endline ("Exception caught: " ^ Printexc.to_string e));

  let doctor_army = new Army.army in
  let doctor_army = doctor_army#add doc in
  let doctor_army = (((doctor_army#add doc)#add doc)#add doc)#add doc in
  (try
     destroy_them_all doctor_army
   with e -> print_endline ("Exception caught: " ^ Printexc.to_string e));

  (new Galifrey.galifrey)#do_time_war
