open Avcodec
open Split

let () = Printexc.record_backtrace true

module Compat = struct
  let map_file _ _ _ _ _ = assert false
  let () = ignore map_file

  include Bigarray.Genarray
  include Unix
end

let split_to_files flac_name cue_name =
    let p1 = String.cat "shntool split -f " "\"" in
    let p2 = String.cat p1 cue_name in 
    let p3 = String.cat p2 "\" -o flac -t %t \"" in
    let p4 = String.cat p3 flac_name in
    let command = String.cat p4 "\"" in
    mysplit command

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf
      "      usage: %s <input audio file>  <input cue file>"
      Sys.argv.(0);
    exit 1);

  Avutil.Log.set_level `Debug;
  Avutil.Log.set_callback print_string;

  let in_codec = Audio.find_decoder_by_name "flac" in

  let parser = Audio.create_parser in_codec in
  let decoder = Audio.create_decoder in_codec in

  let in_fd = Unix.openfile Sys.argv.(1) [Unix.O_RDONLY] 0 in

  let out_file = Av.open_output (String.cat Sys.argv.(1) ".flac") in
  let codec = Avcodec.Audio.find_encoder_by_name "flac" in
  let channel_layout = Avcodec.Audio.find_best_channel_layout codec `Stereo in
  let sample_format = Avcodec.Audio.find_best_sample_format codec `Dbl in
  let sample_rate = Avcodec.Audio.find_best_sample_rate codec 96000 in
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
  let _ = split_to_files (String.cat Sys.argv.(1) ".flac") Sys.argv.(2) in ()

