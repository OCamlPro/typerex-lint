(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*   (GNU General Public Licence version 3.0).                            *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

open D3pie_types

let create_data_content label value =
  let data_content : dataContent Js.t = Js.Unsafe.obj [||] in
  data_content##label <- Js.string label;
  data_content##value <- value;
  data_content

let create_data content =
  let data : data Js.t = Js.Unsafe.obj [||] in
  data##content <- content;
  data

let create_settings data =
  let settings : settings Js.t = Js.Unsafe.obj [||] in
  settings##data <- data;
  settings
    
let create_pie div data =
  let contents =
    Array.of_list (List.map begin fun (label,value) ->
      create_data_content label value
    end data)
  in
  let settings = create_settings (create_data (Js.array contents)) in
  let d3pie_cstr = Js.Unsafe.global##_d3pie in
  Js.Unsafe.new_obj d3pie_cstr [|
    Js.Unsafe.inject div;
    Js.Unsafe.inject settings;
