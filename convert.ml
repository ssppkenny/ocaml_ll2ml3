open Avcodec
open Split
open Detect
open Iconv
open Taglib

module T = Tqdm.Tqdm

let () = Printexc.record_backtrace true

module Compat = struct
    let map_file _ _ _ _ _ = assert false
  let () = ignore map_file

  include Bigarray.Genarray
  include Unix
end

  let myprog filename = T.with_bar 1 ~f:(fun tqdm -> 
      Printf.printf "Converting %s\n" filename;
    for v = 1 to 1 do 
        Unix.sleepf 0.1;
        T.update tqdm v
    done;
    T.update tqdm 1
  ) 

  let split_to_files flac_name cue_name =
      let p1 = String.cat "shntool split -f " "\"" in
      let p2 = String.cat p1 cue_name in 
      let p3 = String.cat p2 "\" -o flac -t %n__%p__%a__%t \"" in
      let p4 = String.cat p3 flac_name in
      let command = String.cat p4 "\"" in
      mysplit command

  let find_files folder input_file ext =
      let new_ext = String.cat "." ext in
      let list_files = Sys.readdir folder in
      let flac_files = List.filter (fun x -> String.equal (Caml.Filename.extension x) new_ext) (Array.to_list list_files) in
      let flac_files_without_input = List.filter (fun x -> not (String.equal x input_file)) flac_files in
      let sorted_files = List.sort String.compare  flac_files_without_input in
      List.filter (fun x -> not (String.equal x (String.cat input_file ".flac"))) sorted_files

  let myconvert input_file output_file from_codec to_codec =

      let _ = Thread.create myprog input_file in 
      let in_codec = Audio.find_decoder_by_name from_codec in

      let parser = Audio.create_parser in_codec in
      let decoder = Audio.create_decoder in_codec in

      let in_fd = Unix.openfile input_file [Unix.O_RDONLY] 0 in

      let out_file = Av.open_output output_file in
      let codec = Avcodec.Audio.find_encoder_by_name to_codec in
      let channel_layout = Avcodec.Audio.find_best_channel_layout codec `Stereo in
      let sample_format = Avcodec.Audio.find_best_sample_format codec `Dbl in
      let rate = if to_codec == "flac" then 96000 else 48000 in
      let sample_rate = Avcodec.Audio.find_best_sample_rate codec rate in
      let time_base = { Avutil.num = 1; den = sample_rate } in
      let out_stream =
          Av.new_audio_stream ~channel_layout ~sample_format ~sample_rate ~time_base
      ~codec out_file
      in

  let filter = ref None in
  let get_filter frame =
      match !filter with
      | Some f -> f
      | None ->
              let in_params =
                  {
                      Avfilter.Utils.sample_rate =
                          Avutil.Audio.frame_get_sample_rate frame;
              channel_layout = Avutil.Audio.frame_get_channel_layout frame;
              sample_format = Avutil.Audio.frame_get_sample_format frame;
      }
  in
          let in_time_base = { Avutil.num = 1; den = sample_rate } in
          let out_frame_size =
              if List.mem `Variable_frame_size (Avcodec.capabilities codec) then
                  512
      else Av.get_frame_size out_stream
          in
          let out_params =
              { Avfilter.Utils.sample_rate; sample_format; channel_layout }
          in
          let f =
              Avfilter.Utils.init_audio_converter ~in_params ~in_time_base
              ~out_params ~out_frame_size ()
          in
          filter := Some f;
          f
          in

  let pts = ref 0L in
  let on_frame frame =
      Avutil.Frame.set_pts frame (Some !pts);
    pts := Int64.add !pts (Int64.of_int (Avutil.Audio.frame_nb_samples frame));
    Av.write_frame out_stream frame
  in

  let write_frame frame =
      let filter = get_filter frame in
      Avfilter.Utils.convert_audio filter on_frame (`Frame frame)
      in

  Compat.map_file in_fd Bigarray.Int8_unsigned Bigarray.c_layout false [| -1 |]
      |> Bigarray.array1_of_genarray
  |> Packet.parse_data parser @@ Avcodec.decode decoder @@ write_frame;

  Avcodec.flush_decoder decoder @@ write_frame;

  Avfilter.Utils.convert_audio (Option.get !filter) on_frame `Flush;
  Unix.close in_fd;
  Av.close out_file;

  Gc.full_major ();
  Gc.full_major ();
  ()

  let tag_file filename =
      let parts = Str.split (Str.regexp "__") (Caml.Filename.remove_extension filename) in
      let arr = Array.of_list parts in
      let f = File.open_file `Autodetect filename in
      let _ = Taglib.tag_set_title f arr.(3) in
      let _ = Taglib.tag_set_artist f arr.(1) in
      let _ = Taglib.tag_set_album f arr.(2) in
      let _ = Taglib.tag_set_track f (int_of_string arr.(0)) in 
      let _ = File.file_save f in
      Taglib.close_file f


  let convert_to_mp3 filename =
      let output_file = String.cat (Caml.Filename.remove_extension filename) ".mp3" in
      let output_file_name = Str.(global_replace (Str.regexp "\"") "" output_file) in
      let _ = myconvert filename  output_file_name "flac" "libmp3lame" in
      Sys.remove filename

  let () =
      if Array.length Sys.argv < 3 then (
          Printf.eprintf
      "      usage: %s <input audio file>  <input cue file>"
      Sys.argv.(0);
    exit 1);

  let _ = Printf.printf "Converting %s\n" Sys.argv.(1) in ();
  let _ = Printf.printf "Using cue file %s\n" Sys.argv.(2) in ();

  Avutil.Log.set_level `Debug;
  Avutil.Log.set_callback print_string;

  let new_flac = (String.cat Sys.argv.(1) ".flac") in
  let iscp1251 = detect Sys.argv.(2) in
  let tmp_cue = "_tmp.cue" in
  let new_cue = if iscp1251 == 1 then "_tmp.cue" else Sys.argv.(2) in
  let _ = if iscp1251 == 1 then iconv Sys.argv.(2) new_cue else 0 in
  let _ = myconvert Sys.argv.(1) new_flac "flac" "flac" in
  let _ = split_to_files new_flac new_cue in
  let _ = if Sys.file_exists tmp_cue then Sys.remove new_cue else () in
  let files_to_convert = find_files "." Sys.argv.(1) "flac" in
  let _ = List.iter convert_to_mp3 files_to_convert in 
  let files_to_tag = find_files "." Sys.argv.(1) "mp3" in
  let _ = List.iter tag_file files_to_tag in Sys.remove new_flac

