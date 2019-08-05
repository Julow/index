let key_size = 44

let value_size = 12

let index_size = 5_000_000

let () = Random.self_init ()

let random_char () = char_of_int (33 + Random.int 94)

let random_string size = String.init size (fun _i -> random_char ())

module Key = struct
  type t = string

  let v () = random_string key_size

  let hash = Hashtbl.hash

  let hash_size = 30

  let encode s = s

  let decode s off = String.sub s off key_size

  let encoded_size = key_size

  let equal = String.equal

  let pp s = Fmt.fmt "%s" s
end

module Value = struct
  type t = string

  let v () = random_string value_size

  let encode s = s

  let decode s off = String.sub s off value_size

  let encoded_size = value_size

  let pp s = Fmt.fmt "%s" s
end

module Stats =
struct

  type t = {
    desc : string;
    start_time : float;
    start_gc_stat : Gc.stat;
    max : int;
    major_alarm : Gc.alarm;
  }

  let alarm () =
    let st = Gc.stat () in
    Fmt.pr "Major collection occurred, remaining %d words (minor heap) and %d blocks (major heap)\n"
      st.live_words st.live_blocks

  (** [max] is only used to print progress *)
  let start ~desc max =
    Fmt.pr "Starting %s, %d loops\n%!" desc max;
    let start_time = Sys.time () in
    let start_gc_stat = Gc.quick_stat () in
    let major_alarm = Gc.create_alarm alarm in
    { desc; start_time; start_gc_stat; max; major_alarm }

  (** Calling it every few loops. [progress] is from 0 to [t.max] *)
  let progress t progress =
    Fmt.epr "\t%4dk/%dk\r%!" (progress / 1000) (t.max / 1000)

  let pp_size ppf s =
    Fmt.pf ppf "%.2f Mw" (s /. 1_000_000.)

  (** Print stats *)
  let stop t =
    let total_time = Sys.time () -. t.start_time in
    let gc1 = t.start_gc_stat and gc2 = Gc.quick_stat () in
    Fmt.pr "Stopping %s, %d loops:\n" t.desc t.max;
    Fmt.pr "  Total time: %f seconds\n" total_time;
    Fmt.pr "  Total allocated (minor heap): %a words\n" pp_size (gc2.minor_words -. gc1.minor_words);
    Fmt.pr "  Total allocated (major heap): %a words\n" pp_size (gc2.major_words -. gc1.major_words);
    Fmt.pr "    Including promoted: %a words\n" pp_size (gc2.promoted_words -. gc1.promoted_words);
    Fmt.pr "  Total minor collections: %d\n" (gc2.minor_collections - gc1.minor_collections);
    Fmt.pr "  Total major collections: %d\n" (gc2.major_collections - gc1.major_collections);
    Fmt.pr "%!";
    Gc.delete_alarm t.major_alarm

end

module Index = Index_unix.Make (Key) (Value)

let index_name = "hello"

let log_size = 500_000

let fan_out_size = 13

let t = Index.v ~fresh:true ~log_size ~fan_out_size index_name

let () =
  let st = Stats.start ~desc:"Adding" index_size in
  let rec loop bindings i =
    if i = 0 then bindings
    else
      let count = index_size - i in
      if count mod 10_000 = 0 then Stats.progress st count;
      let k, v = (Key.v (), Value.v ()) in
      Index.add t k v;
      loop ((k, v) :: bindings) (i - 1)
  in
  let bindings = loop [] index_size in
  Stats.stop st;
  let st = Stats.start ~desc:"Finding" index_size in
  let rec loop count = function
    | [] -> ()
    | (k, v) :: tl -> (
        if count mod 10_000 = 0 then Stats.progress st count;
        match Index.find_all t k with
        | [] -> assert false
        | l ->
            assert (List.mem v l);
            loop (count + 1) tl )
  in
  loop 0 bindings;
  Stats.stop st;
  ()
